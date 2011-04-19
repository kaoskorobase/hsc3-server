{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Sound.SC3.Server.Monad (
  -- *Server Monad
    ServerT
  , runServerT
  , Server
  , runServer
  , connection
  , liftIO
  , rootNodeId
  -- *Allocators
  , BufferId
  , bufferIdAllocator
  , BusId
  , audioBusIdAllocator
  , controlBusIdAllocator
  , NodeId
  , nodeIdAllocator
  , alloc
  , free
  , allocMany
  , freeMany
  , Range
  , allocRange
  , freeRange
  -- *Synchronization
  , fork
  , async
  , syncWith
  , syncWithAll
  , sync
  , unsafeSync
) where

import           Control.Concurrent (ThreadId)
import           Control.Concurrent.MVar.Strict
import           Control.Monad (liftM)
import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Control.Monad.Trans.Reader (ReaderT(..), ask, asks)
import           Control.Monad.Trans.Class (MonadTrans)
import           Data.Accessor
import           Sound.OpenSoundControl (OSC)
import           Sound.SC3.Server.Allocator (Id, IdAllocator, RangeAllocator, Range)
import           Sound.SC3.Server.Connection (Connection)
import qualified Sound.SC3.Server.Connection as C
import           Sound.SC3.Server.Notification (Notification)
import           Sound.SC3.Server.State ( BufferId, bufferIdAllocator
                                        , BusId, audioBusIdAllocator, controlBusIdAllocator
                                        , NodeId, nodeIdAllocator
                                        , State)
import qualified Sound.SC3.Server.State as State
import qualified Sound.SC3.Server.State.Concurrent as IOState

newtype ServerT m a = ServerT (ReaderT Connection m a)
    deriving (Functor, Monad, MonadIO, MonadTrans)

type Server = ServerT IO

type Allocator a = Accessor State a

liftConn :: MonadIO m => (Connection -> IO a) -> ServerT m a
liftConn f = ServerT $ ask >>= \c -> liftIO (f c)

liftState :: MonadIO m => (State -> a) -> ServerT m a
liftState f = ServerT $ asks C.state >>= liftIO . readMVar >>= return . f

connection :: MonadIO m => ServerT m Connection
connection = liftConn return

state :: MonadIO m => ServerT m (MVar State)
state = liftM C.state connection

-- | Run a 'ServerT' computation given a connection and return the result.
runServerT :: ServerT m a -> Connection -> m a
runServerT (ServerT r) = runReaderT r

-- | Run a 'Server' computation given a connection and return the result in the IO monad.
runServer :: Server a -> Connection -> IO a
runServer = runServerT

-- | Return the root node id.
rootNodeId :: MonadIO m => ServerT m NodeId
rootNodeId = liftState State.rootNodeId

-- | Allocate an id using the given allocator.
alloc :: (IdAllocator a, MonadIO m) => Allocator a -> ServerT m (Id a)
alloc a = state >>= liftIO . IOState.alloc a

-- | Free an id using the given allocator.
free :: (IdAllocator a, MonadIO m) => Allocator a -> Id a -> ServerT m ()
free a i = state >>= liftIO . flip (IOState.free a) i

-- | Allocate a number of ids using the given allocator.
allocMany :: (IdAllocator a, MonadIO m) => Allocator a -> Int -> ServerT m [Id a]
allocMany a n = state >>= liftIO . flip (IOState.allocMany a) n

-- | Free a number of ids using the given allocator.
freeMany :: (IdAllocator a, MonadIO m) => Allocator a -> [Id a] -> ServerT m ()
freeMany a is = state >>= liftIO . flip (IOState.freeMany a) is

-- | Allocate a contiguous range of ids using the given allocator.
allocRange :: (RangeAllocator a, MonadIO m) => Allocator a -> Int -> ServerT m (Range (Id a))
allocRange a n = state >>= liftIO . flip (IOState.allocRange a) n

-- | Free a contiguous range of ids using the given allocator.
freeRange :: (RangeAllocator a, MonadIO m) => Allocator a -> Range (Id a) -> ServerT m ()
freeRange a r = state >>= liftIO . flip (IOState.freeRange a) r

fork :: (MonadIO m) => ServerT IO () -> ServerT m ThreadId
fork = liftConn . flip C.fork . runServerT

async :: (MonadIO m) => OSC -> ServerT m ()
async = liftConn . C.async

syncWith :: (MonadIO m) => OSC -> Notification a -> ServerT m a
syncWith s = liftConn . C.syncWith s

syncWithAll :: (MonadIO m) => OSC -> [Notification a] -> ServerT m [a]
syncWithAll s = liftConn . C.syncWithAll s

sync :: (MonadIO m) => OSC -> ServerT m ()
sync = liftConn . C.sync

unsafeSync :: (MonadIO m) => ServerT m ()
unsafeSync = liftConn C.unsafeSync
