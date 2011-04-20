{-# LANGUAGE
    CPP
  , ExistentialQuantification
  , GADTs
  , GeneralizedNewtypeDeriving
  , TypeFamilies #-}

#include "Accessor.h"

-- | Data type for holding server state.
--
-- The server state consists mainly of the allocators needed for different types of resources, such as nodes, buffers and buses.
module Sound.SC3.Server.State (
    State
  , serverOptions
  , Allocator
  , SyncId
  , SyncIdAllocator
  , syncIdAllocator
  , NodeId
  , NodeIdAllocator
  , nodeIdAllocator
  , BufferId
  , BufferIdAllocator
  , bufferIdAllocator
  , BusId
  , BusIdAllocator
  , controlBusIdAllocator
  , audioBusIdAllocator
  , rootNodeId
  , new
) where

import           Control.DeepSeq (NFData(..))
import           Data.Accessor
import           Data.Int (Int32)
import           Sound.SC3.Server.Allocator (IdAllocator(..), RangeAllocator(..))
import qualified Sound.SC3.Server.Allocator as Alloc
import qualified Sound.SC3.Server.Allocator.BlockAllocator.FirstFit as FirstFitAllocator
import qualified Sound.SC3.Server.Allocator.SetAllocator as SetAllocator
import qualified Sound.SC3.Server.Allocator.SimpleAllocator as SimpleAllocator
import qualified Sound.SC3.Server.Allocator.Wrapped as Wrapped
import           Sound.SC3.Server.Options (ServerOptions(..))

-- | Allocator accessor.
type Allocator a = Accessor State a

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

-- | Server state.
data State = State {
    _serverOptions         :: !ServerOptions
  , _syncIdAllocator       :: !SyncIdAllocator
  , _nodeIdAllocator       :: !NodeIdAllocator
  , _bufferIdAllocator     :: !BufferIdAllocator
  , _controlBusIdAllocator :: !BusIdAllocator
  , _audioBusIdAllocator   :: !BusIdAllocator
  }

instance NFData State where
    rnf (State x1 x2 x3 x4 x5 x6) =
            x1 `seq`
        rnf x2 `seq`
        rnf x3 `seq`
        rnf x4 `seq`
        rnf x5 `seq`
        rnf x6 `seq` ()

-- | Server options used to create this instance.
ACCESSOR(serverOptions,         _serverOptions,         State, ServerOptions)
-- | Synchronisation id allocator.
ACCESSOR(syncIdAllocator,       _syncIdAllocator,       State, SyncIdAllocator)
-- | Node id allocator.
ACCESSOR(nodeIdAllocator,       _nodeIdAllocator,       State, NodeIdAllocator)
-- | Buffer id allocator.
ACCESSOR(bufferIdAllocator,     _bufferIdAllocator,     State, BufferIdAllocator)
-- | Control bus id allocator.
ACCESSOR(controlBusIdAllocator, _controlBusIdAllocator, State, BusIdAllocator)
-- | Audio bus id allocator.
ACCESSOR(audioBusIdAllocator,   _audioBusIdAllocator,   State, BusIdAllocator)

-- | Root node id.
rootNodeId :: State -> NodeId
rootNodeId = const (NodeId 0)

-- | Create a new state with default allocators.
new :: ServerOptions -> State
new os =
    State {
        _serverOptions         = os
      , _syncIdAllocator       = sid
      , _nodeIdAllocator       = nid
      , _bufferIdAllocator     = bid
      , _controlBusIdAllocator = cid
      , _audioBusIdAllocator   = aid
    }
    where
        sid = SyncIdAllocator (SimpleAllocator.cons (Alloc.range 0 (maxBound :: SyncId)))
        nid = NodeIdAllocator (SetAllocator.cons (Alloc.range 1000 (1000 + fromIntegral (maxNumberOfNodes os))))
        bid = BufferIdAllocator (FirstFitAllocator.bestFit
                                 FirstFitAllocator.LazyCoalescing
                                 (Alloc.range 0 (fromIntegral (numberOfSampleBuffers os))))
        cid = BusIdAllocator (FirstFitAllocator.bestFit
                              FirstFitAllocator.LazyCoalescing
                              (Alloc.range 0 (fromIntegral (numberOfControlBusChannels os))))
        aid = BusIdAllocator (FirstFitAllocator.bestFit
                              FirstFitAllocator.LazyCoalescing
                              (Alloc.range
                                (fromIntegral numHardwareChannels)
                                (fromIntegral (numHardwareChannels + numberOfAudioBusChannels os))))
        numHardwareChannels = numberOfInputBusChannels os
                            + numberOfOutputBusChannels os
