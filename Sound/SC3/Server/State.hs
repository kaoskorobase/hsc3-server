{-# LANGUAGE
    CPP
  , ExistentialQuantification
  , GADTs
  , GeneralizedNewtypeDeriving
  , TypeFamilies #-}

#include "Accessor.h"

module Sound.SC3.Server.State (
    State
  , options
  , SyncId
  , SyncIdAllocator
  , NodeId
  , NodeIdAllocator
  , BufferId
  , BufferIdAllocator
  , BusId
  , BusIdAllocator
  , syncId
  , nodeId
  , bufferId
  , controlBusId
  , audioBusId
  , rootNode
  , new
  , Alloc.alloc
  , Alloc.free
  , Alloc.allocMany
  , Alloc.freeMany
  , Alloc.allocRange
  , Alloc.freeRange
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

newtype SyncId = SyncId Int32 deriving (Bounded, Enum, Eq, Integral, NFData, Num, Ord, Real, Show)
data SyncIdAllocator = forall a . (IdAllocator a, NFData a, Id a ~ SyncId) => SyncIdAllocator !a

instance IdAllocator SyncIdAllocator where
    type Id SyncIdAllocator = SyncId
    alloc  (SyncIdAllocator a) = Wrapped.alloc SyncIdAllocator a
    free i (SyncIdAllocator a) = Wrapped.free SyncIdAllocator i a
    statistics (SyncIdAllocator a) = Wrapped.statistics a

instance NFData SyncIdAllocator where
    rnf (SyncIdAllocator a) = rnf a `seq` ()

newtype NodeId = NodeId Int32 deriving (Bounded, Enum, Eq, Integral, NFData, Num, Ord, Real, Show)
data NodeIdAllocator = forall a . (IdAllocator a, NFData a, Id a ~ NodeId) => NodeIdAllocator !a

instance IdAllocator NodeIdAllocator where
    type Id NodeIdAllocator = NodeId
    alloc  (NodeIdAllocator a) = Wrapped.alloc NodeIdAllocator a
    free i (NodeIdAllocator a) = Wrapped.free NodeIdAllocator i a
    statistics (NodeIdAllocator a) = Wrapped.statistics a

instance NFData NodeIdAllocator where
    rnf (NodeIdAllocator a) = rnf a `seq` ()

newtype BufferId = BufferId Int32 deriving (Bounded, Enum, Eq, Integral, NFData, Num, Ord, Real, Show)
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

newtype BusId = BusId Int32 deriving (Bounded, Enum, Eq, Integral, NFData, Num, Ord, Real, Show)
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

data State = State {
    _options      :: !ServerOptions
  , _syncId       :: !SyncIdAllocator
  , _nodeId       :: !NodeIdAllocator
  , _bufferId     :: !BufferIdAllocator
  , _controlBusId :: !BusIdAllocator
  , _audioBusId   :: !BusIdAllocator
  }

instance NFData State where
    rnf (State x1 x2 x3 x4 x5 x6) =
            x1 `seq`
        rnf x2 `seq`
        rnf x3 `seq`
        rnf x4 `seq`
        rnf x5 `seq`
        rnf x6 `seq` ()

ACCESSOR(options,      _options,      State, ServerOptions)
ACCESSOR(syncId,       _syncId,       State, SyncIdAllocator)
ACCESSOR(nodeId,       _nodeId,       State, NodeIdAllocator)
ACCESSOR(bufferId,     _bufferId,     State, BufferIdAllocator)
ACCESSOR(controlBusId, _controlBusId, State, BusIdAllocator)
ACCESSOR(audioBusId,   _audioBusId,   State, BusIdAllocator)

rootNode :: State -> NodeId
rootNode = const (NodeId 0)

new :: ServerOptions -> State
new os =
    State {
        _options      = os
      , _syncId       = sid
      , _nodeId       = nid
      , _bufferId     = bid
      , _controlBusId = cid
      , _audioBusId   = aid
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
