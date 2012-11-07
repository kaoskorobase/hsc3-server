{-# LANGUAGE
    ExistentialQuantification
  , GADTs
  , GeneralizedNewtypeDeriving
  , TypeFamilies #-}

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

import           Control.DeepSeq (NFData(..))
import           Data.Int (Int32)
import           Sound.SC3.Server.Allocator (IdAllocator(..), RangeAllocator(..))
import qualified Sound.SC3.Server.Allocator.BlockAllocator.FirstFit as FirstFitAllocator
import qualified Sound.SC3.Server.Allocator.Range as Range
import qualified Sound.SC3.Server.Allocator.SetAllocator as SetAllocator
import qualified Sound.SC3.Server.Allocator.SimpleAllocator as SimpleAllocator
import qualified Sound.SC3.Server.Allocator.Wrapped as Wrapped
import           Sound.SC3.Server.Process.Options (ServerOptions(..))

-- | Synchronisation barrier id.
newtype SyncId = SyncId Int32 deriving (Bounded, Enum, Eq, Integral, NFData, Num, Ord, Real, Show)

-- | Synchronisation barrier id allocator.
data SyncIdAllocator = forall a . (IdAllocator a, NFData a, Id a ~ SyncId) => SyncIdAllocator !a

instance IdAllocator SyncIdAllocator where
    type Id SyncIdAllocator = SyncId
    alloc  (SyncIdAllocator a) = Wrapped.alloc SyncIdAllocator a
    free i (SyncIdAllocator a) = Wrapped.free SyncIdAllocator i a
    statistics (SyncIdAllocator a) = Wrapped.statistics a

instance NFData SyncIdAllocator where
    rnf (SyncIdAllocator a) = rnf a `seq` ()

-- | Node id.
newtype NodeId = NodeId Int32 deriving (Bounded, Enum, Eq, Integral, NFData, Num, Ord, Real, Show)

-- | Node id allocator.
data NodeIdAllocator = forall a . (IdAllocator a, NFData a, Id a ~ NodeId) => NodeIdAllocator !a

instance IdAllocator NodeIdAllocator where
    type Id NodeIdAllocator = NodeId
    alloc  (NodeIdAllocator a) = Wrapped.alloc NodeIdAllocator a
    free i (NodeIdAllocator a) = Wrapped.free NodeIdAllocator i a
    statistics (NodeIdAllocator a) = Wrapped.statistics a

instance NFData NodeIdAllocator where
    rnf (NodeIdAllocator a) = rnf a `seq` ()

-- | Buffer id.
newtype BufferId = BufferId Int32 deriving (Bounded, Enum, Eq, Integral, NFData, Num, Ord, Real, Show)

-- | Buffer id allocator.
data BufferIdAllocator = forall a . (RangeAllocator a, NFData a, Id a ~ BufferId) => BufferIdAllocator !a

instance IdAllocator BufferIdAllocator where
    type Id BufferIdAllocator = BufferId
    alloc  (BufferIdAllocator a) = Wrapped.alloc BufferIdAllocator a
    free i (BufferIdAllocator a) = Wrapped.free BufferIdAllocator i a
    statistics (BufferIdAllocator a) = Wrapped.statistics a

instance RangeAllocator BufferIdAllocator where
    allocRange n (BufferIdAllocator a) = Wrapped.allocRange BufferIdAllocator n a
    freeRange r (BufferIdAllocator a) = Wrapped.freeRange BufferIdAllocator r a

instance NFData BufferIdAllocator where
    rnf (BufferIdAllocator a) = rnf a `seq` ()

-- | Bus id.
newtype BusId = BusId Int32 deriving (Bounded, Enum, Eq, Integral, NFData, Num, Ord, Real, Show)

-- | Bus id allocator.
data BusIdAllocator = forall a . (RangeAllocator a, NFData a, Id a ~ BusId) => BusIdAllocator !a

instance IdAllocator BusIdAllocator where
    type Id BusIdAllocator = BusId
    alloc  (BusIdAllocator a) = Wrapped.alloc BusIdAllocator a
    free i (BusIdAllocator a) = Wrapped.free BusIdAllocator i a
    statistics (BusIdAllocator a) = Wrapped.statistics a

instance RangeAllocator BusIdAllocator where
    allocRange n (BusIdAllocator a) = Wrapped.allocRange BusIdAllocator n a
    freeRange r (BusIdAllocator a) = Wrapped.freeRange BusIdAllocator r a

instance NFData BusIdAllocator where
    rnf (BusIdAllocator a) = rnf a `seq` ()

-- | Server allocators.
data Allocators = Allocators {
    syncIdAllocator       :: !SyncIdAllocator
  , nodeIdAllocator       :: !NodeIdAllocator
  , bufferIdAllocator     :: !BufferIdAllocator
  , controlBusIdAllocator :: !BusIdAllocator
  , audioBusIdAllocator   :: !BusIdAllocator
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
    , controlBusIdAllocator =
        ControlBusIdAllocator $
          FirstFitAllocator.bestFit
            FirstFitAllocator.LazyCoalescing
            (Range.range 0 (fromIntegral (numberOfControlBusChannels os)))
    , audioBusIdAllocator =
        AudioBusIdAllocator $
          FirstFitAllocator.bestFit
            FirstFitAllocator.LazyCoalescing
            (Range.range
              (fromIntegral numHardwareChannels)
              (fromIntegral (numHardwareChannels + numberOfAudioBusChannels os)))
  }
  where
    numHardwareChannels = numberOfInputBusChannels os
                        + numberOfOutputBusChannels os
