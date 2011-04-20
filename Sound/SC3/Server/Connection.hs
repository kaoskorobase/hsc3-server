{-# LANGUAGE ExistentialQuantification
           , FlexibleContexts
           , GeneralizedNewtypeDeriving #-}
-- | A 'Connection' encapsulates the transport needed for communicating with the synthesis server, the client-side state (e.g. resource id allocators) and various synchronisation primitives.
module Sound.SC3.Server.Connection
  ( Connection
  , state
  , new
  , close
    -- * Allocation
  , alloc
  , free
  , allocMany
  , freeMany
  , allocRange
  , freeRange
    -- * Communication and synchronisation
  , async
  , syncWith
  , syncWithAll
  , sync
  , unsafeSync
  ) where

import           Control.Concurrent (forkIO)
import           Control.Concurrent.MVar
import           Control.Monad
import           Data.Accessor
import qualified Data.HashTable as Hash
import           Sound.OpenSoundControl (Datum(..), OSC(..), Transport, immediately)
import qualified Sound.OpenSoundControl as OSC

import           Sound.SC3 (notify)
import           Sound.SC3.Server.Notification (Notification, done, synced)
import           Sound.SC3.Server.Allocator (Id, IdAllocator, Range, RangeAllocator)
import qualified Sound.SC3.Server.Allocator as Alloc
import           Sound.SC3.Server.State (Allocator, State, SyncId)
import qualified Sound.SC3.Server.State as State

type ListenerId  = Int
type Listener    = OSC -> IO ()
data ListenerMap = ListenerMap !(Hash.HashTable ListenerId Listener) !ListenerId
data Connection  = forall t . Transport t => Connection t (MVar State) (MVar ListenerMap)

state :: Connection -> MVar State
state (Connection _ s _) = s

listeners :: Connection -> MVar ListenerMap
listeners (Connection _ _ l) = l

initServer :: Connection -> IO ()
initServer c = do
    Bundle immediately [notify True] `syncWith` done "notify" $ c
    return ()

recvLoop :: Connection -> IO ()
recvLoop c@(Connection t _ _) = do
    osc <- OSC.recv t
    withMVar (listeners c) (\(ListenerMap h _) -> mapM_ (\(_, l) -> l osc) =<< Hash.toList h)
    recvLoop c

-- | Create a new connection given the initial server state and an OSC transport.
new :: Transport t => State -> t -> IO Connection
new s t = do
    ios <- newMVar s
    h  <- Hash.new (==) Hash.hashInt
    lm <- newMVar (ListenerMap h 0)
    let c = Connection t ios lm
    _ <- forkIO $ recvLoop c
    initServer c
    return c

-- | Close the connection.
--
-- The behavior of sending messages after closing the connection is undefined.
close :: Connection -> IO ()
close (Connection t _ _) = OSC.close t

-- ====================================================================
-- Allocation

withAllocator :: Connection -> Allocator a -> (a -> IO (b, a)) -> IO b
withAllocator c a f = modifyMVar (state c) $ \s -> do
    let x = s ^. a
    (i, x') <- f x
    return $ (a ^= x' $ s, i)

withAllocator_ :: Connection -> Allocator a -> (a -> IO a) -> IO ()
withAllocator_ c a f = withAllocator c a $ liftM ((,)()) . f

alloc :: IdAllocator a => Connection -> Allocator a -> IO (Id a)
alloc c a = withAllocator c a Alloc.alloc

free :: IdAllocator a => Connection -> Allocator a -> Id a -> IO ()
free c a = withAllocator_ c a . Alloc.free

allocMany :: IdAllocator a => Connection -> Allocator a -> Int -> IO [Id a]
allocMany c a = withAllocator c a . Alloc.allocMany

freeMany :: IdAllocator a => Connection -> Allocator a -> [Id a] -> IO ()
freeMany c a = withAllocator_ c a . Alloc.freeMany

allocRange :: RangeAllocator a => Connection -> Allocator a -> Int -> IO (Range (Id a))
allocRange c a = withAllocator c a . Alloc.allocRange

freeRange :: RangeAllocator a => Connection -> Allocator a -> Range (Id a) -> IO ()
freeRange c a = withAllocator_ c a . Alloc.freeRange

-- ====================================================================
-- Communication and synchronization

-- Add a listener.
--
-- Listeners are entered in a hash table, although the allocation behavior may be more stack-like.
addListener :: Listener -> Connection -> IO ListenerId
addListener l c = modifyMVar
                    (listeners c) $
                    \(ListenerMap h lid) -> do
                        Hash.insert h lid l
                        -- lc <- Hash.longestChain h
                        -- putStrLn $ "addListener: longestChain=" ++ show (length lc)
                        return (ListenerMap h (lid+1), lid)

-- Remove a listener.
removeListener :: ListenerId -> Connection -> IO ()
removeListener uid c = modifyMVar_ (listeners c) (\lm@(ListenerMap h _) -> Hash.delete h uid >> return lm)

-- | Send an OSC packet asynchronously.
async :: OSC -> Connection -> IO ()
async osc (Connection t _ _) = OSC.send t osc

-- | Send an OSC packet and wait for a notification.
--
-- Returns the transformed value.
--
-- NOTE: There is a race condition between sending the OSC message that has an asynchronous effect and registering the listener, and that's why the OSC packet to be sent has to be passed as an argument.
syncWith :: OSC -> Notification a -> Connection -> IO a
syncWith s f c = do
    res <- newEmptyMVar
    uid <- addListener (action res) c
    s `async` c
    a <- takeMVar res
    removeListener uid c
    return a
    where
        action res osc = do
            case f osc of
                Nothing -> return ()
                Just a  -> putMVar res a

syncWithAll :: OSC -> [Notification a] -> Connection -> IO [a]
syncWithAll s fs c = do
    mv <- newMVar fs
    res <- newEmptyMVar
    uid <- addListener (action mv res) c 
    s `async` c
    as <- replicateM (length fs) (takeMVar res)
    removeListener uid c
    return as
    where
        action mv res osc =
            modifyMVar_ mv $ \fs -> filterM (filterFunc res osc) fs
        filterFunc res osc f =
            case f osc of
                Nothing -> return True
                Just a  -> putMVar res a >> return False

-- | Append a @\/sync@ message to an OSC packet.
appendSync :: OSC -> SyncId -> OSC
appendSync p i =
    case p of
        m@(Message _ _) -> Bundle immediately [m, s]
        (Bundle t xs)   -> Bundle t (xs ++ [s])
    where s = Message "/sync" [Int (fromIntegral i)]

-- | Send an OSC packet and wait for the synchronization barrier.
sync :: OSC -> Connection -> IO ()
sync osc c = do
    i <- alloc c State.syncIdAllocator
    _ <- osc `appendSync` i `syncWith` synced i $ c
    free c State.syncIdAllocator i

-- NOTE: This is only guaranteed to work with a transport that preserves
-- packet order. NOTE 2: And not even then ;)
unsafeSync :: Connection -> IO ()
unsafeSync = sync (Bundle immediately [])
