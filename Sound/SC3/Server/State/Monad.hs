{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Sound.SC3.Server.State.Monad (
-- * Server Monad
  Server
, runServer
-- * Server options
, MonadServer(..)
, serverOption
-- * Allocation
, BufferId
, BufferIdAllocator
, ControlBusId
, ControlBusIdAllocator
, AudioBusId
, AudioBusIdAllocator
, NodeId
, NodeIdAllocator
, MonadIdAllocator(..)
-- * Communication and synchronization
, MonadSendOSC(..)
, MonadRecvOSC(..)
, SyncId
, SyncIdAllocator
, sync
, unsafeSync
-- * Concurrency
, fork
) where

import           Control.Applicative (Applicative)
import           Control.Concurrent (ThreadId)
import qualified Control.Concurrent.Lifted as Conc
import           Control.Concurrent.MVar (MVar, newMVar, modifyMVar)
import           Control.Monad (ap, liftM, replicateM)
import           Control.Monad.Base (MonadBase(..))
import           Control.Monad.Fix (MonadFix)
import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Control.Monad.Trans.Control (MonadBaseControl(..))
import           Control.Monad.Trans.Reader (ReaderT(..))
import qualified Control.Monad.Trans.Reader as R
import           Sound.OpenSoundControl (Bundle(..), Datum(Int), Message(..), OSC, Packet(..), immediately)
import qualified Sound.SC3.Server.Allocator as A
import           Sound.SC3.Server.Command (notify)
import           Sound.SC3.Server.Connection (Connection)
import qualified Sound.SC3.Server.Connection as C
import           Sound.SC3.Server.Notification (Notification, synced)
import           Sound.SC3.Server.Process.Options (ServerOptions)
import           Sound.SC3.Server.State ( AudioBusId, AudioBusIdAllocator
                                        , BufferId, BufferIdAllocator
                                        , ControlBusId, ControlBusIdAllocator
                                        , NodeId, NodeIdAllocator
                                        , SyncId, SyncIdAllocator
                                        )
import qualified Sound.SC3.Server.State as State
import           Sound.SC3.Server.State.Monad.Class

data State = State {
    _serverOptions         :: ServerOptions
  , _connection            :: Connection
  , _syncIdAllocator       :: MVar SyncIdAllocator
  , _nodeIdAllocator       :: MVar NodeIdAllocator
  , _bufferIdAllocator     :: MVar BufferIdAllocator
  , _audioBusIdAllocator   :: MVar AudioBusIdAllocator
  , _controlBusIdAllocator :: MVar ControlBusIdAllocator
  }

newtype Server a = Server { unServer :: ReaderT State IO a }
    deriving (Applicative, Functor, Monad, MonadFix, MonadIO)

instance MonadBase IO Server where
  {-# INLINE liftBase #-}
  liftBase = liftIO

instance MonadBaseControl IO Server where
  newtype StM Server a = StM_Server a
  {-# INLINE liftBaseWith #-}
  liftBaseWith f = do
    s <- Server R.ask
    liftIO $ f $ flip runReaderT s . unServer . fmap StM_Server
  {-# INLINE restoreM #-}
  restoreM (StM_Server a) = return a

-- | Run a 'Server' computation given a connection and return the result.
runServer :: Server a -> ServerOptions -> Connection -> IO a
runServer (Server r) so c =
  return (State so c)
    `ap` new State.syncIdAllocator
    `ap` new State.nodeIdAllocator
    `ap` new State.bufferIdAllocator
    `ap` new State.audioBusIdAllocator
    `ap` new State.controlBusIdAllocator
    >>= runReaderT (init >> r)
  where
    as = State.mkAllocators so
    new :: MonadIO m => (State.Allocators -> a) -> m (MVar a)
    new f = liftIO $ newMVar (f as)
    -- Register with server to receive notifications.
    (Server init) = sync (Packet_Bundle (Bundle immediately [notify True]))

instance MonadServer Server where
  serverOptions = Server $ R.asks _serverOptions
  rootNodeId    = return (fromIntegral 0)

withAllocator :: (State -> MVar a) -> (a -> IO (b, a)) -> Server b
withAllocator a f = Server $ do
  mv <- R.asks a
  liftIO $ modifyMVar mv $ \s -> do
    (i, s') <- f s
    -- Evaluate allocator before putting it back into MVar.
    return $! s' `seq` (s', i)

withAllocator_ :: (State -> MVar a) -> (a -> IO a) -> Server ()
withAllocator_ a f = withAllocator a (liftM ((,)()) . f)

instance MonadIdAllocator Server where
  newtype Allocator Server a = Allocator (State -> MVar a)

  syncIdAllocator       = Allocator _syncIdAllocator
  nodeIdAllocator       = Allocator _nodeIdAllocator
  bufferIdAllocator     = Allocator _bufferIdAllocator
  audioBusIdAllocator   = Allocator _audioBusIdAllocator
  controlBusIdAllocator = Allocator _controlBusIdAllocator

  alloc (Allocator a)      = withAllocator  a   A.alloc
  free (Allocator a)       = withAllocator_ a . A.free
  allocRange (Allocator a) = withAllocator  a . A.allocRange
  freeRange (Allocator a)  = withAllocator_ a . A.freeRange

withConnection :: (Connection -> IO a) -> Server a
withConnection f = Server $ R.asks _connection >>= \c -> liftIO (f c)

sendC :: OSC o => Connection -> o -> IO ()
sendC c osc = do
  -- TODO: Make this configurable
  --print osc
  C.send c osc

instance MonadSendOSC Server where
  send osc = withConnection $ \c -> sendC c osc

-- | Send an OSC packet and wait for a notification.
--
-- Returns the transformed value.
_waitFor :: OSC o => Connection -> o -> Notification a -> IO a
_waitFor c osc n = do
  res <- Conc.newEmptyMVar
  uid <- C.addListener c (C.notificationListener (Conc.putMVar res) n)
  sendC c osc
  a <- Conc.takeMVar res
  C.removeListener c uid
  return a

-- | Send an OSC packet and wait for a list of notifications.
--
-- Returns the transformed values, in unspecified order.
_waitForAll :: OSC o => Connection -> o -> [Notification a] -> IO [a]
_waitForAll c osc [] =
  sendC c osc >> return []
_waitForAll c osc ns = do
  res <- Conc.newChan
  uids <- mapM (C.addListener c . C.notificationListener (Conc.writeChan res)) ns
  sendC c osc
  as <- replicateM (length ns) (Conc.readChan res)
  mapM_ (C.removeListener c) uids
  return as

instance MonadRecvOSC Server where
  waitFor osc n     = withConnection $ \c -> _waitFor c osc n
  waitForAll osc ns = withConnection $ \c -> _waitForAll c osc ns

-- | Append a @\/sync@ message to an OSC packet.
appendSync :: Packet -> SyncId -> Packet
appendSync p i =
  case p of
    Packet_Message m -> Packet_Bundle (Bundle immediately [m, s])
    Packet_Bundle (Bundle t xs) -> Packet_Bundle (Bundle t (xs ++ [s]))
  where s = Message "/sync" [Int (fromIntegral i)]

-- | Send an OSC packet and wait for the synchronization barrier.
sync :: Packet -> Server ()
sync osc = do
  i <- alloc syncIdAllocator
  waitFor_ (osc `appendSync` i) (synced i)
  free syncIdAllocator i

-- NOTE: This is only guaranteed to work with a transport that preserves
-- packet order. NOTE 2: And not even then ;)
unsafeSync :: Server ()
unsafeSync = sync (Packet_Bundle (Bundle immediately []))

-- | Fork a computation in a new thread and return the thread id.
--
-- This is an alias for 'Control.Concurrent.Lifted.fork'.
fork :: Server () -> Server ThreadId
{-# INLINE fork #-}
--fork (Server a) = Server R.ask >>= liftIO . Conc.forkIO . R.runReaderT a
fork = Conc.fork
