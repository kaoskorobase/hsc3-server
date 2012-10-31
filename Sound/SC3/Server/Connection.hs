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
    -- * Sending packets
  , send
    -- * Receiving packets
  , Listener
  , ListenerId
  , notificationListener
  , addListener
  , removeListener
  ) where

import           Control.Concurrent (forkIO)
import           Control.Concurrent.MVar
import qualified Control.Exception as E
import           Control.Monad (void)
import           Sound.OpenSoundControl (OSC(..), Transport)
import qualified Sound.OpenSoundControl as OSC
import           Sound.SC3.Server.Notification (Notification(..))
import           Sound.SC3.Server.Connection.ListenerMap (Listener, ListenerId, ListenerMap)
import qualified Sound.SC3.Server.Connection.ListenerMap as ListenerMap

data Connection  = forall t . Transport t => Connection t (MVar ListenerMap)

listeners :: Connection -> MVar ListenerMap
listeners (Connection _ l) = l

try_recv :: Transport t => t -> IO (Either E.SomeException OSC)
try_recv = E.try . OSC.recv

recvLoop :: Connection -> IO ()
recvLoop c@(Connection t ls) = do
  e <- try_recv t
  case e of
    Left _ -> return ()
    Right osc -> do
      withMVar ls $ ListenerMap.broadcast osc
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

-- | Send an OSC packet asynchronously.
send :: Connection -> OSC -> IO ()
send (Connection t _) = OSC.send t

-- ====================================================================
-- Listeners

-- | Create a listener from an IO action and a notification.
notificationListener :: (a -> IO ()) -> Notification a -> Listener
notificationListener f n osc =
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
