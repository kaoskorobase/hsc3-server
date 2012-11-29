{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
module Sound.SC3.Server.State.Monad.Class (
  MonadServer(..)
, serverOption
, MonadIdAllocator(..)
, RequestOSC(..)
) where

import Control.Monad (liftM)
import Sound.OpenSoundControl (OSC)
import Sound.OSC.Transport.Monad (SendOSC)
import Sound.SC3.Server.Allocator (Id, IdAllocator, RangeAllocator, Statistics)
import Sound.SC3.Server.Allocator.Range (Range)
import Sound.SC3.Server.Notification (Notification)
import Sound.SC3.Server.State ( AudioBusIdAllocator
                              , ControlBusIdAllocator
                              , BufferIdAllocator
                              , NodeId
                              , NodeIdAllocator
                              , SyncIdAllocator )
import Sound.SC3.Server.Process (ServerOptions)

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

  -- | 'NodeId' allocator.
  nodeIdAllocator       :: Allocator m NodeIdAllocator
  -- | 'SyncId' allocator.
  syncIdAllocator       :: Allocator m SyncIdAllocator
  -- | 'BufferId' allocator.
  bufferIdAllocator     :: Allocator m BufferIdAllocator
  -- | 'AudioBusId' allocator.
  audioBusIdAllocator   :: Allocator m AudioBusIdAllocator
  -- | 'ControlBusId' allocator.
  controlBusIdAllocator :: Allocator m ControlBusIdAllocator

  -- | Allocate an id using the given allocator.
  alloc :: IdAllocator a => Allocator m a -> m (Id a)
  -- | Free an id using the given allocator.
  free :: IdAllocator a => Allocator m a -> Id a -> m ()
  -- | Return allocator statistics
  statistics :: IdAllocator a => Allocator m a -> m Statistics

  -- | Allocate a contiguous range of ids using the given allocator.
  allocRange :: RangeAllocator a => Allocator m a -> Int -> m (Range (Id a))
  -- | Free a contiguous range of ids using the given allocator.
  freeRange :: RangeAllocator a => Allocator m a -> Range (Id a) -> m ()

class SendOSC m => RequestOSC m where
  -- | Wait for a notification and return the result.
  request :: OSC o => o -> Notification a -> m a
  -- | Wait for a set of notifications and return their results in unspecified order.
  requestAll :: OSC o => o -> [Notification a] -> m [a]
