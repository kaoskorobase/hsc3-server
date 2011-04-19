{-# LANGUAGE ExistentialQuantification
           , GeneralizedNewtypeDeriving #-}
module Sound.SC3.Server.Connection (
    Connection
  , state
  , new
  , close
  , fork
  , async
  , syncWith
  , sync
  , unsafeSync
) where

import           Control.Concurrent (ThreadId, forkIO)
import           Control.Concurrent.MVar
import qualified Data.HashTable as Hash
import           Sound.OpenSoundControl (Datum(..), OSC(..), Transport, immediately)
import qualified Sound.OpenSoundControl as OSC

import           Sound.SC3 (notify)
import           Sound.SC3.Server.Notification (Notification, done, synced)
import           Sound.SC3.Server.State (State, SyncId)
import qualified Sound.SC3.Server.State as State
import qualified Sound.SC3.Server.State.Concurrent as IOState

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
    ios <- IOState.fromState s
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

-- | Fork a new thread sharing the same connection.
fork :: Connection -> (Connection -> IO ()) -> IO ThreadId
fork c f = forkIO (f c)

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

    where

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
    i <- IOState.alloc State.syncIdAllocator (state c)
    _ <- osc `appendSync` i `syncWith` synced i $ c
    IOState.free State.syncIdAllocator (state c) i
    return ()

-- NOTE: This is only guaranteed to work with a transport that preserves
-- packet order. NOTE 2: And not even then ;)
unsafeSync :: Connection -> IO ()
unsafeSync = sync (Bundle immediately [])
