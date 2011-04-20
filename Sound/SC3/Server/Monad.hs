{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Sound.SC3.Server.Monad
  ( -- * Server Monad
    ServerT
  , runServerT
  , Server
  , runServer
  , liftIO
  , connection
  , serverOptions
  , rootNodeId
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
  , alloc
  , free
  , allocMany
  , freeMany
  , Range
  , allocRange
  , freeRange
  -- * Communication and synchronization
  , fork
  , SyncId
  , SyncIdAllocator
  , syncIdAllocator
  , send
  , syncWith
  , syncWithAll
  , sync
  , unsafeSync
  ) where

import           Control.Concurrent (ThreadId, forkIO)
import           Control.Concurrent.MVar.Strict
import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Control.Monad.Trans.Reader (ReaderT(..), ask, asks)
import           Control.Monad.Trans.Class (MonadTrans)
import           Data.Accessor
import           Sound.OpenSoundControl (OSC)
import           Sound.SC3.Server.Allocator (Id, IdAllocator, RangeAllocator, Range)
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
    deriving (Functor, Monad, MonadIO, MonadTrans)

type Server = ServerT IO

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

-- | Return the server options.
serverOptions :: MonadIO m => ServerT m ServerOptions
serverOptions = liftState (getVal State.serverOptions)

-- | Return the root node id.
rootNodeId :: MonadIO m => ServerT m NodeId
rootNodeId = liftState State.rootNodeId

-- | Allocate an id using the given allocator.
alloc :: (IdAllocator a, MonadIO m) => Allocator a -> ServerT m (Id a)
alloc a = liftConn $ \c -> C.alloc c a

-- | Free an id using the given allocator.
free :: (IdAllocator a, MonadIO m) => Allocator a -> Id a -> ServerT m ()
free a i = liftConn $ \c -> C.free c a i

-- | Allocate a number of ids using the given allocator.
allocMany :: (IdAllocator a, MonadIO m) => Allocator a -> Int -> ServerT m [Id a]
allocMany a n = liftConn $ \c -> C.allocMany c a n

-- | Free a number of ids using the given allocator.
freeMany :: (IdAllocator a, MonadIO m) => Allocator a -> [Id a] -> ServerT m ()
freeMany a is = liftConn $ \c -> C.freeMany c a is

-- | Allocate a contiguous range of ids using the given allocator.
allocRange :: (RangeAllocator a, MonadIO m) => Allocator a -> Int -> ServerT m (Range (Id a))
allocRange a n = liftConn $ \c -> C.allocRange c a n

-- | Free a contiguous range of ids using the given allocator.
freeRange :: (RangeAllocator a, MonadIO m) => Allocator a -> Range (Id a) -> ServerT m ()
freeRange a r = liftConn $ \c -> C.freeRange c a r

-- | Fork a computation in a new thread and return the thread id.
fork :: (MonadIO m) => ServerT IO () -> ServerT m ThreadId
fork a = liftConn $ \c -> forkIO (runServerT a c)

send :: (MonadIO m) => OSC -> ServerT m ()
send osc = liftConn $ \c -> C.send c osc

syncWith :: (MonadIO m) => OSC -> Notification a -> ServerT m a
syncWith osc n = liftConn $ \c -> C.syncWith c osc n

syncWithAll :: (MonadIO m) => OSC -> [Notification a] -> ServerT m [a]
syncWithAll osc ns = liftConn $ \c -> C.syncWithAll c osc ns

sync :: (MonadIO m) => OSC -> ServerT m ()
sync osc = liftConn $ \c -> C.sync c osc

unsafeSync :: (MonadIO m) => ServerT m ()
unsafeSync = liftConn C.unsafeSync
