{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies #-}

-- | Data type for holding server state.
--
-- The server state consists mainly of the allocators needed for different types of resources, such as nodes, buffers and buses.
module Sound.SC3.Server.State (
  SyncId
, SyncIdAllocator
, syncIdAllocator
, NodeId
, NodeIdAllocator
, nodeIdAllocator
, BufferId
, BufferIdAllocator
, bufferIdAllocator
, ControlBusId
, ControlBusIdAllocator
, controlBusIdAllocator
, AudioBusId
, AudioBusIdAllocator
, audioBusIdAllocator
, Allocators
, mkAllocators
) where

import           Data.Int (Int32)
import           Sound.SC3.Server.Allocator (IdAllocator(..), RangeAllocator(..))
import qualified Sound.SC3.Server.Allocator.BlockAllocator.FirstFit as FirstFitAllocator
import qualified Sound.SC3.Server.Allocator.Range as Range
import qualified Sound.SC3.Server.Allocator.SetAllocator as SetAllocator
import qualified Sound.SC3.Server.Allocator.SimpleAllocator as SimpleAllocator
import qualified Sound.SC3.Server.Allocator.Wrapped as Wrapped
import           Sound.SC3.Server.Process.Options (ServerOptions(..))

-- | Synchronisation barrier id.
newtype SyncId = SyncId Int32 deriving (Bounded, Enum, Eq, Integral, Num, Ord, Real, Show)

-- | Synchronisation barrier id allocator.
data SyncIdAllocator = forall a . (IdAllocator a, Id a ~ SyncId) => SyncIdAllocator !a

instance IdAllocator SyncIdAllocator where
    type Id SyncIdAllocator = SyncId
    alloc  (SyncIdAllocator a) = Wrapped.alloc SyncIdAllocator a
    free i (SyncIdAllocator a) = Wrapped.free SyncIdAllocator i a
    statistics (SyncIdAllocator a) = Wrapped.statistics a

-- | Node id.
newtype NodeId = NodeId Int32 deriving (Bounded, Enum, Eq, Integral, Num, Ord, Real, Show)

-- | Node id allocator.
data NodeIdAllocator = forall a . (IdAllocator a, Id a ~ NodeId) => NodeIdAllocator !a

instance IdAllocator NodeIdAllocator where
    type Id NodeIdAllocator = NodeId
    alloc  (NodeIdAllocator a) = Wrapped.alloc NodeIdAllocator a
    free i (NodeIdAllocator a) = Wrapped.free NodeIdAllocator i a
    statistics (NodeIdAllocator a) = Wrapped.statistics a

-- | Buffer id.
newtype BufferId = BufferId Int32 deriving (Bounded, Enum, Eq, Integral, Num, Ord, Real, Show)

-- | Buffer id allocator.
data BufferIdAllocator = forall a . (RangeAllocator a, Id a ~ BufferId) => BufferIdAllocator !a

instance IdAllocator BufferIdAllocator where
    type Id BufferIdAllocator = BufferId
    alloc  (BufferIdAllocator a) = Wrapped.alloc BufferIdAllocator a
    free i (BufferIdAllocator a) = Wrapped.free BufferIdAllocator i a
    statistics (BufferIdAllocator a) = Wrapped.statistics a

instance RangeAllocator BufferIdAllocator where
    allocRange n (BufferIdAllocator a) = Wrapped.allocRange BufferIdAllocator n a
    freeRange r (BufferIdAllocator a) = Wrapped.freeRange BufferIdAllocator r a

-- | Control bus id.
newtype ControlBusId = ControlBusId Int32
                     deriving (Bounded, Enum, Eq, Integral, Num, Ord, Real, Show)

-- | Control bus id allocator.
data ControlBusIdAllocator = forall a . (RangeAllocator a, Id a ~ ControlBusId) => ControlBusIdAllocator !a

instance IdAllocator ControlBusIdAllocator where
    type Id ControlBusIdAllocator = ControlBusId
    alloc  (ControlBusIdAllocator a) = Wrapped.alloc ControlBusIdAllocator a
    free i (ControlBusIdAllocator a) = Wrapped.free ControlBusIdAllocator i a
    statistics (ControlBusIdAllocator a) = Wrapped.statistics a

instance RangeAllocator ControlBusIdAllocator where
    allocRange n (ControlBusIdAllocator a) = Wrapped.allocRange ControlBusIdAllocator n a
    freeRange r (ControlBusIdAllocator a) = Wrapped.freeRange ControlBusIdAllocator r a

-- | Audio bus id.
newtype AudioBusId = AudioBusId Int32
                     deriving (Bounded, Enum, Eq, Integral, Num, Ord, Real, Show)

-- | Audio bus id allocator.
data AudioBusIdAllocator = forall a . (RangeAllocator a, Id a ~ AudioBusId) => AudioBusIdAllocator !a

instance IdAllocator AudioBusIdAllocator where
    type Id AudioBusIdAllocator = AudioBusId
    alloc  (AudioBusIdAllocator a) = Wrapped.alloc AudioBusIdAllocator a
    free i (AudioBusIdAllocator a) = Wrapped.free AudioBusIdAllocator i a
    statistics (AudioBusIdAllocator a) = Wrapped.statistics a

instance RangeAllocator AudioBusIdAllocator where
    allocRange n (AudioBusIdAllocator a) = Wrapped.allocRange AudioBusIdAllocator n a
    freeRange r (AudioBusIdAllocator a) = Wrapped.freeRange AudioBusIdAllocator r a

-- | Server allocators.
data Allocators = Allocators {
    syncIdAllocator       :: SyncIdAllocator
  , nodeIdAllocator       :: NodeIdAllocator
  , bufferIdAllocator     :: BufferIdAllocator
  , audioBusIdAllocator   :: AudioBusIdAllocator
  , controlBusIdAllocator :: ControlBusIdAllocator
  }

-- | Create a new state with default allocators.
mkAllocators :: ServerOptions -> Allocators
mkAllocators os =
  Allocators {
      syncIdAllocator =
        SyncIdAllocator $
          SimpleAllocator.cons
            (Range.range 0 (maxBound :: SyncId))
    , nodeIdAllocator =
        NodeIdAllocator $
          SetAllocator.cons
            (Range.range 1000 (1000 + fromIntegral (maxNumberOfNodes os)))
    , bufferIdAllocator =
        BufferIdAllocator $
          FirstFitAllocator.bestFit
            FirstFitAllocator.LazyCoalescing
            (Range.range 0 (fromIntegral (numberOfSampleBuffers os)))
    , audioBusIdAllocator =
        AudioBusIdAllocator $
          FirstFitAllocator.bestFit
            FirstFitAllocator.LazyCoalescing
            (Range.range
              (fromIntegral numHardwareChannels)
              (fromIntegral (numHardwareChannels + numberOfAudioBusChannels os)))
    , controlBusIdAllocator =
        ControlBusIdAllocator $
          FirstFitAllocator.bestFit
            FirstFitAllocator.LazyCoalescing
            (Range.range 0 (fromIntegral (numberOfControlBusChannels os)))
  }
  where
    numHardwareChannels = numberOfInputBusChannels os
                        + numberOfOutputBusChannels os
