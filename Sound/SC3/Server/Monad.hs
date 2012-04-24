{-# LANGUAGE FlexibleContexts
           , FlexibleInstances
           , GeneralizedNewtypeDeriving
           , MultiParamTypeClasses
           , TypeFamilies
           , UndecidableInstances #-}
module Sound.SC3.Server.Monad
  ( -- * Server Monad
    ServerT
  , runServerT
  , Server
  , runServer
  , liftIO
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
import           Control.Concurrent.Lifted (ThreadId)
import qualified Control.Concurrent.Lifted as CL
import           Control.Concurrent.MVar.Strict
import           Control.DeepSeq (NFData)
import           Control.Monad (MonadPlus, liftM)
import           Control.Monad.Base (MonadBase(..), liftBaseDefault)
import           Control.Monad.Fix (MonadFix)
import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Control.Monad.Trans.Reader (ReaderT(..), asks)
import           Control.Monad.Trans.Resource (MonadThrow)
import           Control.Monad.Trans.Class (MonadTrans(..))
import           Control.Monad.Trans.Control
import           Sound.OpenSoundControl (Datum(..), OSC(..), immediately)
import           Sound.SC3.Server.Allocator (Id, IdAllocator, RangeAllocator, Range)
import qualified Sound.SC3.Server.Allocator as A
import           Sound.SC3.Server.Command (notify)
import           Sound.SC3.Server.Connection (Connection)
import qualified Sound.SC3.Server.Connection as C
import           Sound.SC3.Server.Notification (Notification, synced)
import           Sound.SC3.Server.Options (ServerOptions)
import           Sound.SC3.Server.State ( BufferId, BufferIdAllocator 
                                        , BusId, BusIdAllocator
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
  , _controlBusIdAllocator :: MVar BusIdAllocator
  , _audioBusIdAllocator   :: MVar BusIdAllocator
  }

newtype ServerT m a = ServerT { unServerT :: ReaderT State m a }
    deriving (Alternative, Applicative, Functor, Monad, MonadFix, MonadIO, MonadPlus, MonadThrow, MonadTrans)

type Server = ServerT IO

instance MonadBase b m => MonadBase b (ServerT m) where
    {-# INLINE liftBase #-}
    liftBase = liftBaseDefault

instance MonadTransControl ServerT where
    newtype StT ServerT a = StServerT {unStServerT::a}
    {-# INLINE liftWith #-}
    liftWith f = ServerT $ ReaderT $ \r -> f $ \t -> liftM StServerT $ runReaderT (unServerT t) r
    {-# INLINE restoreT #-}
    restoreT = ServerT . ReaderT . const . liftM unStServerT

instance MonadBaseControl b m => MonadBaseControl b (ServerT m) where
    newtype StM (ServerT m) a = StMT { unStMT :: ComposeSt ServerT m a }
    {-# INLINE liftBaseWith #-}
    liftBaseWith = defaultLiftBaseWith StMT
    {-# INLINE restoreM #-}
    restoreM = defaultRestoreM   unStMT

newtype Allocator a = Allocator (State -> MVar a)

syncIdAllocator :: Allocator SyncIdAllocator
syncIdAllocator = Allocator _syncIdAllocator

nodeIdAllocator :: Allocator NodeIdAllocator
nodeIdAllocator = Allocator _nodeIdAllocator

bufferIdAllocator :: Allocator BufferIdAllocator
bufferIdAllocator = Allocator _bufferIdAllocator

controlBusIdAllocator :: Allocator BusIdAllocator
controlBusIdAllocator = Allocator _controlBusIdAllocator

audioBusIdAllocator :: Allocator BusIdAllocator
audioBusIdAllocator = Allocator _audioBusIdAllocator

-- | Run a 'ServerT' computation given a connection and return the result.
runServerT :: MonadIO m => ServerT m a -> ServerOptions -> Connection -> m a
runServerT (ServerT r) so c = do
    sa <- new State.syncIdAllocator
    na <- new State.nodeIdAllocator
    ba <- new State.bufferIdAllocator
    ca <- new State.controlBusIdAllocator
    aa <- new State.audioBusIdAllocator
    let s = State so c sa na ba ca aa
    runReaderT (init >> r) s
    where 
        as = State.mkAllocators so
        new :: (IdAllocator a, NFData a, MonadIO m) => (State.Allocators -> a) -> m (MVar a)
        new f = liftIO $ newMVar (f as)
        -- Register with server to receive notifications.
        (ServerT init) = sync (Bundle immediately [notify True])

-- | Run a 'Server' computation given a connection and return the result in the IO monad.
runServer :: Server a -> ServerOptions -> Connection -> IO a
runServer = runServerT

class Monad m => MonadServer m where
    -- | Return the server options.
    serverOptions :: m ServerOptions

-- | Return a server option.
serverOption :: MonadServer m => (ServerOptions -> a) -> m a
serverOption = flip liftM serverOptions

-- serverOptions :: MonadIO m => ServerT m ServerOptions
instance Monad m => MonadServer (ServerT m) where
    serverOptions = ServerT $ asks _serverOptions

-- | Monadic resource id management interface.
class Monad m => MonadIdAllocator m where
    -- | Return the root node id.
    rootNodeId :: m NodeId

    -- | Allocate an id using the given allocator.
    alloc :: (IdAllocator a, NFData a) => Allocator a -> m (Id a)

    -- | Free an id using the given allocator.
    free :: (IdAllocator a, NFData a) => Allocator a -> Id a -> m ()

    -- | Allocate a number of ids using the given allocator.
    allocMany :: (IdAllocator a, NFData a) => Allocator a -> Int -> m [Id a]

    -- | Free a number of ids using the given allocator.
    freeMany :: (IdAllocator a, NFData a) => Allocator a -> [Id a] -> m ()

    -- | Allocate a contiguous range of ids using the given allocator.
    allocRange :: (RangeAllocator a, NFData a) => Allocator a -> Int -> m (Range (Id a))

    -- | Free a contiguous range of ids using the given allocator.
    freeRange :: (RangeAllocator a, NFData a) => Allocator a -> Range (Id a) -> m ()

withAllocator :: (IdAllocator a, NFData a, MonadIO m) => (a -> IO (b, a)) -> Allocator a -> ServerT m b
withAllocator f (Allocator a) = ServerT $ do
    mv <- asks a
    liftIO $ modifyMVar mv $ \s -> do
        (i, s') <- f s
        return $! (s', i)

withAllocator_ :: (IdAllocator a, NFData a, MonadIO m) => (a -> IO a) -> Allocator a -> ServerT m ()
withAllocator_ f = withAllocator (liftM ((,)()) . f)

instance (MonadIO m) => MonadIdAllocator (ServerT m) where
    rootNodeId      = return (fromIntegral 0)
    alloc a         = withAllocator A.alloc a
    free a i        = withAllocator_ (A.free i) a
    allocMany a n   = withAllocator (A.allocMany n) a
    freeMany a is   = withAllocator_ (A.freeMany is) a
    allocRange a n  = withAllocator (A.allocRange n) a
    freeRange a r   = withAllocator_ (A.freeRange r) a

class Monad m => MonadSendOSC m where
    send :: OSC -> m ()

withConnection :: MonadIO m => (Connection -> IO a) -> ServerT m a
withConnection f = ServerT $ asks _connection >>= \c -> liftIO (f c)

instance MonadIO m => MonadSendOSC (ServerT m) where
    send osc = withConnection $ \c -> C.send c osc

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
    waitFor osc n      = withConnection $ \c -> C.waitFor c osc n
    waitFor_ osc n     = withConnection $ \c -> C.waitFor_ c osc n
    waitForAll osc ns  = withConnection $ \c -> C.waitForAll c osc ns
    waitForAll_ osc ns = withConnection $ \c -> C.waitForAll_ c osc ns

-- | Append a @\/sync@ message to an OSC packet.
appendSync :: OSC -> SyncId -> OSC
appendSync p i =
    case p of
        m@(Message _ _) -> Bundle immediately [m, s]
        (Bundle t xs)   -> Bundle t (xs ++ [s])
    where s = Message "/sync" [Int (fromIntegral i)]

-- | Send an OSC packet and wait for the synchronization barrier.
sync :: (MonadIO m) => OSC -> ServerT m ()
sync osc = do
    i <- alloc syncIdAllocator
    waitFor_ (osc `appendSync` i) (synced i)
    free syncIdAllocator i

-- NOTE: This is only guaranteed to work with a transport that preserves
-- packet order. NOTE 2: And not even then ;)
unsafeSync :: (MonadIO m) => ServerT m ()
unsafeSync = sync (Bundle immediately [])


-- | Fork a computation in a new thread and return the thread id.
fork :: (MonadBaseControl IO m) => ServerT m () -> ServerT m ThreadId
fork = CL.fork

