{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Sound.SC3.Server.Monad (
  -- *Server Monad
    ServerT
  , runServerT
  , Server
  , runServer
  , lift
  , liftIO
  , rootNode
  -- *Allocators
  , BufferId
  , bufferId
  , BusId
  , busId
  , NodeId
  , nodeId
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
  , syncAddress
  , sync
  , unsafeSync
) where

import           Control.Concurrent (ThreadId)
import           Control.Concurrent.MVar.Strict
import           Control.Monad.Reader (MonadReader, ReaderT(..), ask, asks, lift)
import           Control.Monad.Trans (MonadIO, MonadTrans, liftIO)
import           Data.Accessor
import           Sound.SC3 (Rate(..))
import           Sound.SC3.Server.Allocator (Id, IdAllocator, RangeAllocator, Range)
import           Sound.SC3.Server.Connection (Connection)
import qualified Sound.SC3.Server.Connection as C
-- import           Sound.SC3.Server.Process.Options (ServerOptions, numberOfInputBusChannels, numberOfOutputBusChannels)
import           Sound.SC3.Server.Notification (Notification)
import           Sound.SC3.Server.State (BufferId, BufferIdAllocator, BusId, BusIdAllocator, NodeId, NodeIdAllocator, State)
import qualified Sound.SC3.Server.State as State
import qualified Sound.SC3.Server.State.Concurrent as IOState

import           Sound.OpenSoundControl (OSC)

newtype ServerT m a = ServerT (ReaderT Connection m a)
    deriving (Functor, Monad, MonadReader Connection, MonadIO, MonadTrans)

type Server = ServerT IO

type Allocator a = Accessor State a

liftConn :: MonadIO m => (Connection -> IO a) -> ServerT m a
liftConn f = ask >>= \c -> liftIO (f c)

liftState :: MonadIO m => (State -> a) -> ServerT m a
liftState f = asks C.state >>= liftIO . readMVar >>= return . f

-- | Run a 'ServerT' computation given a connection and return the result.
runServerT :: ServerT m a -> Connection -> m a
runServerT (ServerT r) = runReaderT r

-- | Run a 'Server' computation given a connection and return the result in the IO monad.
runServer :: Server a -> Connection -> IO a
runServer = runServerT

-- | Return the root node id.
rootNode :: MonadIO m => ServerT m NodeId
rootNode = liftState State.rootNode

-- | Node id allocator.
nodeId :: Allocator NodeIdAllocator
nodeId = State.nodeId

-- | Buffer id allocator.
bufferId :: Allocator BufferIdAllocator
bufferId = State.bufferId

-- | Bus id allocator, indexed by rate.
busId :: Rate -> Allocator BusIdAllocator
busId AR = State.audioBusId
busId KR = State.controlBusId
busId r  = error ("No bus allocator for rate " ++ show r)

-- | Allocate an id using the given allocator.
alloc :: (IdAllocator a, MonadIO m) => Allocator a -> ServerT m (Id a)
alloc a = asks C.state >>= liftIO . IOState.alloc a

-- | Free an id using the given allocator.
free :: (IdAllocator a, MonadIO m) => Allocator a -> Id a -> ServerT m ()
free a i = asks C.state >>= liftIO . flip (IOState.free a) i

-- | Allocate a number of ids using the given allocator.
allocMany :: (IdAllocator a, MonadIO m) => Allocator a -> Int -> ServerT m [Id a]
allocMany a n = asks C.state >>= liftIO . flip (IOState.allocMany a) n

-- | Free a number of ids using the given allocator.
freeMany :: (IdAllocator a, MonadIO m) => Allocator a -> [Id a] -> ServerT m ()
freeMany a is = asks C.state >>= liftIO . flip (IOState.freeMany a) is

-- | Allocate a contiguous range of ids using the given allocator.
allocRange :: (RangeAllocator a, MonadIO m) => Allocator a -> Int -> ServerT m (Range (Id a))
allocRange a n = asks C.state >>= liftIO . flip (IOState.allocRange a) n

-- | Free a contiguous range of ids using the given allocator.
freeRange :: (RangeAllocator a, MonadIO m) => Allocator a -> Range (Id a) -> ServerT m ()
freeRange a r = asks C.state >>= liftIO . flip (IOState.freeRange a) r

fork :: (MonadIO m) => ServerT IO () -> ServerT m ThreadId
fork = liftConn . flip C.fork . runServerT

async :: (MonadIO m) => OSC -> ServerT m ()
async = liftConn . C.async

syncWith :: (MonadIO m) => OSC -> Notification a -> ServerT m a
syncWith s = liftConn . C.syncWith s

syncAddress :: (MonadIO m) => OSC -> String -> ServerT m OSC
syncAddress s = liftConn . C.syncAddress s

sync :: (MonadIO m) => OSC -> ServerT m ()
sync = liftConn . C.sync

unsafeSync :: (MonadIO m) => ServerT m ()
unsafeSync = liftConn C.unsafeSync
