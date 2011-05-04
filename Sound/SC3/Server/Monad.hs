{-# LANGUAGE FlexibleContexts
           , GeneralizedNewtypeDeriving
           , MultiParamTypeClasses #-}
module Sound.SC3.Server.Monad
  ( -- * Server Monad
    ServerT
  , runServerT
  , Server
  , runServer
  , liftIO
  , connection
  -- * Server options
  , MonadServer(..)
  , serverOption
  -- * Allocation
  , Allocator
  , BufferId
  , BufferIdAllocator
  , bufferIdAllocator
  , BusId
  , BusIdAllocator
  , audioBusIdAllocator
  , controlBusIdAllocator
  , NodeId
  , NodeIdAllocator
  , nodeIdAllocator
  , Range
  , MonadIdAllocator(..)
  -- * Communication and synchronization
  , MonadSendOSC(..)
  , MonadRecvOSC(..)
  , SyncId
  , SyncIdAllocator
  , syncIdAllocator
  , sync
  , unsafeSync
  -- * Concurrency
  , fork
  ) where

import           Control.Applicative
import           Control.Concurrent (ThreadId, forkIO)
import           Control.Concurrent.MVar.Strict
import           Control.Failure (Failure(..))
import           Control.Monad (MonadPlus, liftM)
import           Control.Monad.Fix (MonadFix)
import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Control.Monad.Trans.Reader (ReaderT(..), ask, asks)
import           Control.Monad.Trans.Class (MonadTrans)
import           Data.Accessor
import           Sound.OpenSoundControl (OSC)
import           Sound.SC3.Server.Allocator (AllocFailure, Id, IdAllocator, RangeAllocator, Range)
import           Sound.SC3.Server.Connection (Connection)
import qualified Sound.SC3.Server.Connection as C
import           Sound.SC3.Server.Notification (Notification)
import           Sound.SC3.Server.Options (ServerOptions)
import           Sound.SC3.Server.State ( Allocator
                                        , BufferId, BufferIdAllocator, bufferIdAllocator
                                        , BusId, BusIdAllocator, audioBusIdAllocator, controlBusIdAllocator
                                        , NodeId, NodeIdAllocator, nodeIdAllocator
                                        , SyncId, SyncIdAllocator, syncIdAllocator
                                        , State)
import qualified Sound.SC3.Server.State as State

newtype ServerT m a = ServerT (ReaderT Connection m a)
    deriving (Alternative, Applicative, Functor, Monad, MonadFix, MonadIO, MonadPlus, MonadTrans)

type Server = ServerT IO

instance MonadIO m => Failure AllocFailure (ServerT m) where
    failure = liftIO . failure

liftConn :: MonadIO m => (Connection -> IO a) -> ServerT m a
liftConn f = ServerT $ ask >>= \c -> liftIO (f c)

liftState :: MonadIO m => (State -> a) -> ServerT m a
liftState f = ServerT $ asks C.state >>= liftIO . readMVar >>= return . f

-- | Run a 'ServerT' computation given a connection and return the result.
runServerT :: ServerT m a -> Connection -> m a
runServerT (ServerT r) = runReaderT r

-- | Run a 'Server' computation given a connection and return the result in the IO monad.
runServer :: Server a -> Connection -> IO a
runServer = runServerT

-- | Return the connection.
connection :: MonadIO m => ServerT m Connection
connection = liftConn return

class Monad m => MonadServer m where
    -- | Return the server options.
    serverOptions :: m ServerOptions

-- | Return a server option.
serverOption :: MonadServer m => (ServerOptions -> a) -> m a
serverOption = flip liftM serverOptions

-- serverOptions :: MonadIO m => ServerT m ServerOptions
instance MonadIO m => MonadServer (ServerT m) where
    serverOptions = liftState (getVal State.serverOptions)

-- | Monadic resource id management interface.
class Failure AllocFailure m => MonadIdAllocator m where
    -- | Return the root node id.
    rootNodeId :: m NodeId

    -- | Allocate an id using the given allocator.
    alloc :: (IdAllocator a) => Allocator a -> m (Id a)

    -- | Free an id using the given allocator.
    free :: (IdAllocator a) => Allocator a -> Id a -> m ()

    -- | Allocate a number of ids using the given allocator.
    allocMany :: (IdAllocator a) => Allocator a -> Int -> m [Id a]

    -- | Free a number of ids using the given allocator.
    freeMany :: (IdAllocator a) => Allocator a -> [Id a] -> m ()

    -- | Allocate a contiguous range of ids using the given allocator.
    allocRange :: (RangeAllocator a) => Allocator a -> Int -> m (Range (Id a))

    -- | Free a contiguous range of ids using the given allocator.
    freeRange :: (RangeAllocator a) => Allocator a -> Range (Id a) -> m ()

instance MonadIO m => MonadIdAllocator (ServerT m) where
    rootNodeId = liftState State.rootNodeId
    alloc a = liftConn $ \c -> C.alloc c a
    free a i = liftConn $ \c -> C.free c a i
    allocMany a n = liftConn $ \c -> C.allocMany c a n
    freeMany a is = liftConn $ \c -> C.freeMany c a is
    allocRange a n = liftConn $ \c -> C.allocRange c a n
    freeRange a r = liftConn $ \c -> C.freeRange c a r

class Monad m => MonadSendOSC m where
    send :: OSC -> m ()

instance MonadIO m => MonadSendOSC (ServerT m) where
    send osc = liftConn $ \c -> C.send c osc

class Monad m => MonadRecvOSC m where
    -- | Wait for a notification and return the result.
    waitFor :: OSC -> Notification a -> m a
    -- | Wait for a notification and ignore the result.
    waitFor_ :: OSC -> Notification a -> m ()
    -- | Wait for a set of notifications and return their results in unspecified order.
    waitForAll :: OSC -> [Notification a] -> m [a]
    -- | Wait for a set of notifications and ignore their results.
    waitForAll_ :: OSC -> [Notification a] -> m ()

instance MonadIO m => MonadRecvOSC (ServerT m) where
    waitFor osc n = liftConn $ \c -> C.waitFor c osc n
    waitFor_ osc n = liftConn $ \c -> C.waitFor_ c osc n
    waitForAll osc ns = liftConn $ \c -> C.waitForAll c osc ns
    waitForAll_ osc ns = liftConn $ \c -> C.waitForAll_ c osc ns

sync :: (MonadIO m) => OSC -> ServerT m ()
sync osc = liftConn $ \c -> C.sync c osc

unsafeSync :: (MonadIO m) => ServerT m ()
unsafeSync = liftConn C.unsafeSync

-- | Fork a computation in a new thread and return the thread id.
fork :: (MonadIO m) => ServerT IO () -> ServerT m ThreadId
fork a = liftConn $ \c -> forkIO (runServerT a c)
