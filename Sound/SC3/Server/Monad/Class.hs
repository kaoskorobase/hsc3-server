{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
module Sound.SC3.Server.Monad.Class (
  MonadServer(..)
, serverOption
, MonadIdAllocator(..)
, allocMany
, freeMany
, MonadSendOSC(..)
, MonadRecvOSC(..)
) where

--import           Control.Exception.Lifted (mask, onException)
import           Control.Failure (Failure)
import           Control.Monad (liftM, replicateM)
--import           Control.Monad.Trans.Control (MonadBaseControl)
import           Sound.OpenSoundControl (OSC)
import           Sound.SC3.Server.Allocator (AllocFailure, Id, IdAllocator, RangeAllocator)
--import qualified Sound.SC3.Server.Allocator as Alloc
import           Sound.SC3.Server.Allocator.Range (Range)
import           Sound.SC3.Server.Notification (Notification)
import           Sound.SC3.Server.State ( AudioBusIdAllocator
                                        , ControlBusIdAllocator
                                        , BufferIdAllocator
                                        , NodeId
                                        , NodeIdAllocator
                                        , SyncIdAllocator )
import           Sound.SC3.Server.Process (ServerOptions)

class Monad m => MonadServer m where
  -- | Return the server options.
  serverOptions :: m ServerOptions
  -- | Return the root node id.
  rootNodeId :: m NodeId

-- | Return a server option.
serverOption :: MonadServer m => (ServerOptions -> a) -> m a
serverOption = flip liftM serverOptions

-- | Monadic resource id management interface.
class Monad m => MonadIdAllocator m where
  data Allocator m a

  nodeIdAllocator       :: Allocator m NodeIdAllocator
  syncIdAllocator       :: Allocator m SyncIdAllocator
  bufferIdAllocator     :: Allocator m BufferIdAllocator
  audioBusIdAllocator   :: Allocator m AudioBusIdAllocator
  controlBusIdAllocator :: Allocator m ControlBusIdAllocator

  -- | Allocate an id using the given allocator.
  alloc :: IdAllocator a => Allocator m a -> m (Id a)
  -- | Free an id using the given allocator.
  free :: IdAllocator a => Allocator m a -> Id a -> m ()
  -- | Allocate a contiguous range of ids using the given allocator.
  allocRange :: RangeAllocator a => Allocator m a -> Int -> m (Range (Id a))
  -- | Free a contiguous range of ids using the given allocator.
  freeRange :: RangeAllocator a => Allocator m a -> Range (Id a) -> m ()

-- | Allocate a number of ids using the given allocator.
allocMany :: (IdAllocator a, Failure AllocFailure m, MonadIdAllocator m) => Allocator m a -> Int -> m [Id a]
allocMany a n = replicateM n (alloc a)

-- | Free a number of ids using the given allocator.
freeMany :: (IdAllocator a, Failure AllocFailure m, MonadIdAllocator m) => Allocator m a -> [Id a] -> m ()
freeMany a = mapM_ (free a)

class Monad m => MonadSendOSC m where
  send :: OSC -> m ()

class MonadSendOSC m => MonadRecvOSC m where
  -- | Wait for a notification and return the result.
  waitFor :: OSC -> Notification a -> m a
  -- | Wait for a notification and ignore the result.
  waitFor_ :: OSC -> Notification a -> m ()
  waitFor_ osc n = waitFor osc n >> return ()
  -- | Wait for a set of notifications and return their results in unspecified order.
  waitForAll :: OSC -> [Notification a] -> m [a]
  -- | Wait for a set of notifications and ignore their results.
  waitForAll_ :: OSC -> [Notification a] -> m ()
  waitForAll_ osc ns = waitForAll osc ns >> return ()
