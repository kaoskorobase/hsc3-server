{-# LANGUAGE ExistentialQuantification
           , FlexibleContexts
           , GeneralizedNewtypeDeriving #-}
-- | A 'Connection' encapsulates the communication with the synthesis server.
-- This module provides functions for opening and closing connections, as well
-- as communication and synchronisation primitives.
module Sound.SC3.Server.Connection
  ( Connection
    -- * Creation and termination
  , open
  , close
    -- * Communication and synchronisation
  , send
  , waitFor
  , waitFor_
  , waitForAll
  , waitForAll_
  ) where

import           Control.Concurrent (forkIO)
import           Control.Concurrent.MVar
import           Control.Concurrent.Chan
import           Control.Monad (replicateM, void)
import           Sound.OpenSoundControl (OSC(..), Transport)
import qualified Sound.OpenSoundControl as OSC
import           Sound.SC3.Server.Notification (Notification(..))
import           Sound.SC3.Server.Connection.ListenerMap (Listener, ListenerId, ListenerMap)
import qualified Sound.SC3.Server.Connection.ListenerMap as ListenerMap

data Connection  = forall t . Transport t => Connection t (MVar ListenerMap)

listeners :: Connection -> MVar ListenerMap
listeners (Connection _ l) = l

recvLoop :: Connection -> IO ()
recvLoop c@(Connection t ls) = do
    osc <- OSC.recv t
    withMVar ls (ListenerMap.broadcast osc)
    recvLoop c

-- | Create a new connection given an OSC transport.
open :: Transport t => t -> IO Connection
open t = do
    ls <- newMVar =<< ListenerMap.empty
    let c = Connection t ls
    void $ forkIO $ recvLoop c
    return c

-- | Close the connection.
--
-- The behavior of sending messages after closing the connection is undefined.
close :: Connection -> IO ()
close (Connection t _) = OSC.close t

-- ====================================================================
-- Communication and synchronization

-- | Create a listener from an IO action and a notification.
mkListener :: (a -> IO ()) -> Notification a -> Listener
mkListener f n osc =
    case n `match` osc of
        Nothing -> return ()
        Just a  -> f a

-- | Add a listener to the listener map.
addListener :: Connection -> Listener -> IO ListenerId
addListener c l = modifyMVar (listeners c) $ \lm -> do
    (uid, lm') <- ListenerMap.add l lm
    return (lm', uid)

-- | Remove a listener from the listener map.
removeListener :: Connection -> ListenerId -> IO ()
removeListener c uid = modifyMVar_ (listeners c) (ListenerMap.delete uid)

-- | Send an OSC packet asynchronously.
send :: Connection -> OSC -> IO ()
send (Connection t _) = OSC.send t

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

