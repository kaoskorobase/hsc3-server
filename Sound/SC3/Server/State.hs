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
  , Alloc.allocMany
  , Alloc.allocRange
  , Alloc.free
) where

import           Control.Arrow (second)
import           Control.DeepSeq (NFData(..))
import           Control.Monad (liftM)
import           Data.Accessor
import           Sound.SC3.Server.Allocator (IdAllocator(..))
import qualified Sound.SC3.Server.Allocator as Alloc
import           Sound.SC3.Server.Allocator.SetAllocator (SetAllocator)
import qualified Sound.SC3.Server.Allocator.SetAllocator as SetAlloc
import           Sound.SC3.Server.Allocator.SimpleAllocator (SimpleAllocator)
import qualified Sound.SC3.Server.Allocator.SimpleAllocator as SAlloc
import           Sound.SC3.Server.Options (ServerOptions(..))

allocM f = liftM (second f) . Alloc.alloc
freeM f i = liftM f . Alloc.free i

newtype SyncId = SyncId Int deriving (Bounded, Enum, Eq, Integral, NFData, Num, Ord, Real, Show)
data SyncIdAllocator = forall a . (IdAllocator a, NFData a, Id a ~ SyncId) => SyncIdAllocator !a

instance IdAllocator SyncIdAllocator where
    type Id SyncIdAllocator = SyncId
    alloc  (SyncIdAllocator a) = allocM SyncIdAllocator a
    free i (SyncIdAllocator a) = freeM SyncIdAllocator i a

instance NFData SyncIdAllocator where
    rnf (SyncIdAllocator a) = rnf a `seq` ()

newtype NodeId = NodeId Int deriving (Bounded, Enum, Eq, Integral, NFData, Num, Ord, Real, Show)
data NodeIdAllocator = forall a . (IdAllocator a, NFData a, Id a ~ NodeId) => NodeIdAllocator !a

instance IdAllocator NodeIdAllocator where
    type Id NodeIdAllocator = NodeId
    alloc  (NodeIdAllocator a) = allocM NodeIdAllocator a
    free i (NodeIdAllocator a) = freeM NodeIdAllocator i a

instance NFData NodeIdAllocator where
    rnf (NodeIdAllocator a) = rnf a `seq` ()

newtype BufferId = BufferId Int deriving (Bounded, Enum, Eq, Integral, NFData, Num, Ord, Real, Show)
data BufferIdAllocator = forall a . (IdAllocator a, NFData a, Id a ~ BufferId) => BufferIdAllocator !a

instance IdAllocator BufferIdAllocator where
    type Id BufferIdAllocator = BufferId
    alloc  (BufferIdAllocator a) = allocM BufferIdAllocator a
    free i (BufferIdAllocator a) = freeM BufferIdAllocator i a

instance NFData BufferIdAllocator where
    rnf (BufferIdAllocator a) = rnf a `seq` ()

newtype BusId = BusId Int deriving (Bounded, Enum, Eq, Integral, NFData, Num, Ord, Real, Show)
data BusIdAllocator = forall a . (IdAllocator a, NFData a, Id a ~ BusId) => BusIdAllocator !a

instance IdAllocator BusIdAllocator where
    type Id BusIdAllocator = BusId
    alloc  (BusIdAllocator a) = allocM BusIdAllocator a
    free i (BusIdAllocator a) = freeM BusIdAllocator i a

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
        sid = SyncIdAllocator
                (SAlloc.cons :: SimpleAllocator SyncId)
        nid = NodeIdAllocator
                (SetAlloc.cons $ Alloc.range 1000 (1000 + fromIntegral (maxNumberOfNodes os)) :: SetAllocator NodeId)
        bid = BufferIdAllocator
                (SetAlloc.cons $ Alloc.range 0 (fromIntegral (numberOfSampleBuffers os - 1)) :: SetAllocator BufferId)
        cid = BusIdAllocator
                (SetAlloc.cons $ Alloc.range 0 (fromIntegral (numberOfControlBusChannels os - 1)) :: SetAllocator BusId)
        aid = BusIdAllocator
                (SetAlloc.cons $ Alloc.range
                    (fromIntegral numHardwareChannels)
                    (fromIntegral (numHardwareChannels + numberOfAudioBusChannels os)) :: SetAllocator BusId)
        numHardwareChannels = numberOfInputBusChannels os
                            + numberOfOutputBusChannels os
