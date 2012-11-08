{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies #-}

module Sound.SC3.Server.Monad (
-- * Server Monad
  ServerT
, runServerT
, Server
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

import           Control.Applicative (Alternative, Applicative)
import           Control.Concurrent (ThreadId)
import qualified Control.Concurrent as Conc
import           Control.Concurrent.MVar (MVar, newMVar, modifyMVar)
import           Control.Monad (MonadPlus, ap, liftM, replicateM)
import           Control.Monad.Base (MonadBase(..), liftBaseDefault)
import           Control.Monad.Fix (MonadFix)
import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Control.Monad.Trans.Reader (ReaderT(..))
import qualified Control.Monad.Trans.Reader as R
--import           Control.Monad.Trans.Resource (MonadResource, MonadThrow)
import           Control.Monad.Trans.Class (MonadTrans(..))
--import           Control.Monad.Trans.Control
import           Sound.OpenSoundControl (Datum(..), OSC(..), immediately)
import qualified Sound.SC3.Server.Allocator as A
--import           Sound.SC3.Server.Allocator.Range (Range)
import           Sound.SC3.Server.Command (notify)
import           Sound.SC3.Server.Connection (Connection)
import qualified Sound.SC3.Server.Connection as C
import           Sound.SC3.Server.Monad.Class
import           Sound.SC3.Server.Notification (Notification, synced)
import           Sound.SC3.Server.Process.Options (ServerOptions)
import           Sound.SC3.Server.State ( AudioBusId, AudioBusIdAllocator
                                        , BufferId, BufferIdAllocator
                                        , ControlBusId, ControlBusIdAllocator
                                        , NodeId, NodeIdAllocator
                                        , SyncId, SyncIdAllocator
                                        )
import qualified Sound.SC3.Server.State as State

data State = State {
    _serverOptions         :: ServerOptions
  , _connection            :: Connection
  , _syncIdAllocator       :: MVar SyncIdAllocator
  , _nodeIdAllocator       :: MVar NodeIdAllocator
  , _bufferIdAllocator     :: MVar BufferIdAllocator
  , _audioBusIdAllocator   :: MVar AudioBusIdAllocator
  , _controlBusIdAllocator :: MVar ControlBusIdAllocator
  }

newtype ServerT m a = ServerT (ReaderT State m a)
    deriving (Alternative, Applicative, Functor, Monad, MonadFix, MonadIO, MonadPlus, {- MonadResource, MonadThrow, -} MonadTrans)

type Server = ServerT IO

-- instance MonadBase b m => MonadBase b (ServerT m) where
--     {-# INLINE liftBase #-}
--     liftBase = liftBaseDefault
-- 
-- instance MonadTransControl ServerT where
--     newtype StT ServerT a = StServerT {unStServerT::a}
--     {-# INLINE liftWith #-}
--     liftWith f = ServerT $ ReaderT $ \r -> f $ \t -> liftM StServerT $ runReaderT (unServerT t) r
--     {-# INLINE restoreT #-}
--     restoreT = ServerT . ReaderT . const . liftM unStServerT
-- 
-- instance MonadBaseControl b m => MonadBaseControl b (ServerT m) where
--     newtype StM (ServerT m) a = StMT { unStMT :: ComposeSt ServerT m a }
--     {-# INLINE liftBaseWith #-}
--     liftBaseWith = defaultLiftBaseWith StMT
--     {-# INLINE restoreM #-}
--     restoreM = defaultRestoreM   unStMT

-- | Run a 'ServerT' computation given a connection and return the result.
runServerT :: MonadIO m => ServerT m a -> ServerOptions -> Connection -> m a
runServerT (ServerT r) so c =
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
    (ServerT init) = sync (Bundle immediately [notify True])

-- | Run a 'Server' computation given a connection and return the result in the IO monad.
runServer :: Server a -> ServerOptions -> Connection -> IO a
runServer = runServerT

instance Monad m => MonadServer (ServerT m) where
  serverOptions = ServerT $ R.asks _serverOptions
  rootNodeId    = return (fromIntegral 0)

withAllocator :: MonadIO m => (State -> MVar a) -> (a -> IO (b, a)) -> ServerT m b
withAllocator a f = ServerT $ do
  mv <- R.asks a
  liftIO $ modifyMVar mv $ \s -> do
    (i, s') <- f s
    -- Evaluate allocator before putting back to MVar
    return $! s' `seq` (s', i)

withAllocator_ :: MonadIO m => (State -> MVar a) -> (a -> IO a) -> ServerT m ()
withAllocator_ a f = withAllocator a (liftM ((,)()) . f)

instance MonadIO m => MonadIdAllocator (ServerT m) where
  newtype Allocator (ServerT m) a = Allocator (State -> MVar a)

  syncIdAllocator       = Allocator _syncIdAllocator
  nodeIdAllocator       = Allocator _nodeIdAllocator
  bufferIdAllocator     = Allocator _bufferIdAllocator
  audioBusIdAllocator   = Allocator _audioBusIdAllocator
  controlBusIdAllocator = Allocator _controlBusIdAllocator

  alloc (Allocator a)      = withAllocator  a A.alloc
  free (Allocator a)       = withAllocator_ a . A.free
  allocRange (Allocator a) = withAllocator  a . A.allocRange
  freeRange (Allocator a)  = withAllocator_ a . A.freeRange

withConnection :: MonadIO m => (Connection -> IO a) -> ServerT m a
withConnection f = ServerT $ R.asks _connection >>= \c -> liftIO (f c)

sendC :: Connection -> OSC -> IO ()
sendC c osc = do
  -- TODO: Make this configurable
  --print osc
  C.send c osc

instance MonadIO m => MonadSendOSC (ServerT m) where
  send osc = withConnection $ \c -> sendC c osc

-- | Send an OSC packet and wait for a notification.
--
-- Returns the transformed value.
_waitFor :: Connection -> OSC -> Notification a -> IO a
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
_waitForAll :: Connection -> OSC -> [Notification a] -> IO [a]
_waitForAll c osc [] =
  sendC c osc >> return []
_waitForAll c osc ns = do
  res <- Conc.newChan
  uids <- mapM (C.addListener c . C.notificationListener (Conc.writeChan res)) ns
  sendC c osc
  as <- replicateM (length ns) (Conc.readChan res)
  mapM_ (C.removeListener c) uids
  return as

instance MonadIO m => MonadRecvOSC (ServerT m) where
  waitFor osc n     = withConnection $ \c -> _waitFor c osc n
  waitForAll osc ns = withConnection $ \c -> _waitForAll c osc ns

-- | Append a @\/sync@ message to an OSC packet.
appendSync :: OSC -> SyncId -> OSC
appendSync p i =
  case p of
    m@(Message _ _) -> Bundle immediately [m, s]
    (Bundle t xs)   -> Bundle t (xs ++ [s])
  where s = Message "/sync" [Int (fromIntegral i)]

-- | Send an OSC packet and wait for the synchronization barrier.
sync :: MonadIO m => OSC -> ServerT m ()
sync osc = do
  i <- alloc syncIdAllocator
  waitFor_ (osc `appendSync` i) (synced i)
  free syncIdAllocator i

-- NOTE: This is only guaranteed to work with a transport that preserves
-- packet order. NOTE 2: And not even then ;)
unsafeSync :: MonadIO m => ServerT m ()
unsafeSync = sync (Bundle immediately [])

-- | Fork a computation in a new thread and return the thread id.
fork :: ServerT IO () -> ServerT IO ThreadId
fork (ServerT a) = ServerT R.ask >>= liftIO . Conc.forkIO . R.runReaderT a
