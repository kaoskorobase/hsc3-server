{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- | A 'Connection' encapsulates the communication with the synthesis server.
-- This module provides functions for opening and closing connections, as well
-- as communication and synchronisation primitives.
module Sound.SC3.Server.Connection (
  Connection
  -- * Creation and termination
, open
, close
  -- * Sending packets
, send
  -- * Receiving packets
, withListener
) where

import           Control.Concurrent (ThreadId, forkIO, myThreadId)
import           Control.Concurrent.MVar
import qualified Control.Exception as E
import           Control.Monad (void)
import qualified Data.HashTable.IO as H
import           Sound.OSC.FD (OSC(..), Packet, Transport)
import qualified Sound.OSC.FD as OSC

type Listener = Packet -> IO ()
type ListenerMap = H.CuckooHashTable ThreadId Listener

data Connection  = forall t . Transport t => Connection t (MVar ListenerMap)

listeners :: Connection -> MVar ListenerMap
listeners (Connection _ l) = l

try_recv :: Transport t => t -> IO (Either E.SomeException Packet)
try_recv = E.try . OSC.recvPacket

recvLoop :: Connection -> IO ()
recvLoop c@(Connection t ls) = do
  e <- try_recv t
  case e of
    Left _ -> return ()
    Right osc -> do
      withMVar ls $ H.mapM_ (\(_, l) -> l osc)
      recvLoop c

-- | Create a new connection given an OSC transport.
open :: Transport t => t -> IO Connection
open t = do
  ls <- newMVar =<< H.new
  let c = Connection t ls
  void $ forkIO $ recvLoop c
  return c

-- | Close the connection.
--
-- The behavior of sending messages after closing the connection is undefined.
close :: Connection -> IO ()
close (Connection t _) = OSC.close t

-- | Send an OSC packet asynchronously.
send :: OSC o => Connection -> o -> IO ()
send (Connection t _) = OSC.sendOSC t

-- ====================================================================
-- Listeners

-- | Add a listener to the listener map.
addListener :: Connection -> Listener -> IO (ThreadId, Maybe Listener)
addListener c l = do
  uid <- myThreadId
  withMVar (listeners c) $ \lm -> do
    l' <- H.lookup lm uid
    H.insert lm uid l
    return (uid, l')

-- | Remove a listener from the listener map.
removeListener :: Connection -> ThreadId -> Maybe Listener -> IO ()
removeListener c uid Nothing = withMVar (listeners c) $ \ls -> H.delete ls uid
removeListener c uid (Just l) = withMVar (listeners c) $ \ls -> H.insert ls uid l

-- | Perform an IO action with a registered listener that is automatically removed.
withListener :: Connection -> (Packet -> IO ()) -> IO a -> IO a
withListener c l = do
  E.bracket
    (addListener c l)
    (uncurry (removeListener c))
    . const
