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
  , send
  , waitFor
  , waitFor_
  , waitForAll
  , waitForAll_
  , sync
  , unsafeSync
  ) where

import           Control.Concurrent (forkIO)
import           Control.Concurrent.MVar
import           Control.Concurrent.Chan
import           Control.Monad (liftM, replicateM, void)
import           Data.Accessor
import qualified Data.IntMap as IntMap
import qualified Data.Foldable as IntMap
import           Sound.OpenSoundControl (Datum(..), OSC(..), Transport, immediately)
import qualified Sound.OpenSoundControl as OSC

import           Sound.SC3 (notify)
import           Sound.SC3.Server.Notification (Notification(..), synced)
import           Sound.SC3.Server.Allocator (Id, IdAllocator, Range, RangeAllocator)
import qualified Sound.SC3.Server.Allocator as Alloc
import           Sound.SC3.Server.State (Allocator, State, SyncId)
import qualified Sound.SC3.Server.State as State

type ListenerId  = Int
type Listener    = OSC -> IO ()
data ListenerMap = ListenerMap (IntMap.IntMap Listener) !ListenerId
data Connection  = forall t . Transport t => Connection t (MVar State) (MVar ListenerMap)

state :: Connection -> MVar State
state (Connection _ s _) = s

listeners :: Connection -> MVar ListenerMap
listeners (Connection _ _ l) = l

initServer :: Connection -> IO ()
initServer c = sync c (Bundle immediately [notify True])

recvLoop :: Connection -> IO ()
recvLoop c@(Connection t _ _) = do
    osc <- OSC.recv t
    withMVar (listeners c) (\(ListenerMap m _) -> IntMap.mapM_ ($osc) m)
    recvLoop c

-- | Create a new connection given the initial server state and an OSC transport.
new :: Transport t => State -> t -> IO Connection
new s t = do
    ios <- newMVar s
    lm <- newMVar (ListenerMap IntMap.empty 0)
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

-- | Create a listener from an IO action and a notification.
mkListener :: (a -> IO ()) -> Notification a -> Listener
mkListener f n osc =
    case n `match` osc of
        Nothing -> return ()
        Just a  -> f a

-- | Add a listener.
--
-- Listeners are entered in a hash table, although the allocation behavior may be more stack-like.
addListener :: Connection -> Listener -> IO ListenerId
addListener c l =
    modifyMVar (listeners c) $
        \(ListenerMap m i) -> do
            -- Hash.insert h lid l
            -- lc <- Hash.longestChain h
            -- putStrLn $ "addListener: longestChain=" ++ show (length lc)
            return (ListenerMap (IntMap.insert i l m) (i+1), i)

-- | Remove a listener.
removeListener :: Connection -> ListenerId -> IO ()
removeListener c uid =
    modifyMVar_ (listeners c) $
        \(ListenerMap m i) -> do
            -- Hash.delete h uid
            return (ListenerMap  (IntMap.delete uid m) i)

-- | Send an OSC packet asynchronously.
send :: Connection -> OSC -> IO ()
send (Connection t _ _) = OSC.send t

-- | Send an OSC packet and wait for a notification.
--
-- Returns the transformed value.
waitFor :: Connection -> OSC -> Notification a -> IO a
waitFor c osc n = do
    res <- newEmptyMVar
    uid <- addListener c (mkListener (putMVar res) n)
    send c osc
    a <- takeMVar res
    removeListener c uid
    return a

-- | Send an OSC packet and wait for a notification.
--
-- Ignores the transformed value.
waitFor_ :: Connection -> OSC -> Notification a -> IO ()
waitFor_ c osc n = void $ waitFor c osc n

-- | Send an OSC packet and wait for a list of notifications.
--
-- Returns the transformed values, in unspecified order.
waitForAll :: Connection -> OSC -> [Notification a] -> IO [a]
waitForAll c osc [] =
    send c osc >> return []
waitForAll c osc ns = do
    res <- newChan
    uids <- mapM (addListener c . mkListener (writeChan res)) ns
    send c osc
    as <- replicateM (length ns) (readChan res)
    mapM_ (removeListener c) uids
    return as

-- | Send an OSC packet and wait for a list of notifications.
--
-- Ignores the transformed values.
waitForAll_ :: Connection -> OSC -> [Notification a] -> IO ()
waitForAll_ c osc ns = void $ waitForAll c osc ns

-- | Append a @\/sync@ message to an OSC packet.
appendSync :: OSC -> SyncId -> OSC
appendSync p i =
    case p of
        m@(Message _ _) -> Bundle immediately [m, s]
        (Bundle t xs)   -> Bundle t (xs ++ [s])
    where s = Message "/sync" [Int (fromIntegral i)]

-- | Send an OSC packet and wait for the synchronization barrier.
sync :: Connection -> OSC -> IO ()
sync c osc = do
    i <- alloc c State.syncIdAllocator
    waitFor_ c (osc `appendSync` i) (synced i)
    free c State.syncIdAllocator i

-- NOTE: This is only guaranteed to work with a transport that preserves
-- packet order. NOTE 2: And not even then ;)
unsafeSync :: Connection -> IO ()
unsafeSync c = sync c (Bundle immediately [])
