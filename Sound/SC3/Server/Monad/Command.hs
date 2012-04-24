{-# LANGUAGE ExistentialQuantification
           , FlexibleContexts
           , FlexibleInstances
           , GeneralizedNewtypeDeriving
           , MultiParamTypeClasses #-}
module Sound.SC3.Server.Monad.Command
  (
  -- * Master controls
    status
  , PrintLevel(..)
  , dumpOSC
  -- * Synth definitions
  , SynthDef(name)
  -- , d_recv
  -- , d_load
  -- , d_loadDir
  , d_named
  , d_default
  , d_recv
  , d_free
  -- * Resources
  -- ** Nodes
  , Node(..)
  , AddAction(..)
  , n_after
  , n_before
  , n_fill
  , n_free
  , BusMapping(..)
  , n_query_
  , n_query
  , n_run_
  , n_set
  , n_setn
  , n_trace
  , n_order
  -- *** Synths
  , Synth(..)
  , s_new
  , s_new_
  , s_release
  -- *** Groups
  , Group(..)
  , rootNode
  , g_new
  , g_new_
  , g_deepFree
  , g_freeAll
  , g_head
  , g_tail
  , g_dumpTree
  -- ** Buffers
  , Buffer
  , b_alloc
  , b_read
  , HeaderFormat(..)
  , SampleFormat(..)
  , b_write
  , b_free
  , b_zero
  , b_query
  -- ** Buses
  , Bus(..)
  , busId
  , numChannels
  , AudioBus(..)
  , inputBus
  , outputBus
  , newAudioBus
  , ControlBus(..)
  , newControlBus
  ) where

--import qualified Codec.Compression.BZip as BZip
--import qualified Codec.Digest.SHA as SHA
import           Control.Arrow (first)
import           Control.Failure (Failure, failure)
import           Control.Monad (liftM)
import           Control.Monad.IO.Class (MonadIO)
import           Sound.SC3 (Rate(..), UGen)
import qualified Sound.SC3.Server.Allocator.Range as Range
import           Sound.SC3.Server.Monad hiding (sync, unsafeSync)
import qualified Sound.SC3.Server.Monad as M
import           Sound.SC3.Server.Monad.Send
import qualified Sound.SC3.Server.Synthdef as Synthdef
import           Sound.SC3.Server.Allocator (AllocFailure(..))
import           Sound.SC3.Server.Command (AddAction(..), PrintLevel(..))
import qualified Sound.SC3.Server.Command as C
import qualified Sound.SC3.Server.Command.Completion as C
import qualified Sound.SC3.Server.Notification as N
import           Sound.SC3.Server.Options (ServerOptions(..))
import           Sound.OpenSoundControl (OSC(..))

-- ====================================================================
-- Utils

-- | Construct a function suitable for 'mkAsync'.
mkC :: a -> (OSC -> a) -> (Maybe OSC -> a)
mkC f _ Nothing    = f
mkC _ f (Just osc) = f osc

-- ====================================================================
-- Master controls

status :: MonadIO m => RequestT m (Deferred m N.Status)
status = send C.status >> after N.status_reply (return ())

dumpOSC :: MonadIO m => PrintLevel -> RequestT m (Deferred m ())
dumpOSC p = do
    i <- M.alloc M.syncIdAllocator
    send (C.dumpOSC p)
    send (C.sync (fromIntegral i))
    after_ (N.synced i) (return ())

-- ====================================================================
-- Synth definitions

newtype SynthDef = SynthDef {
    name  :: String
  } deriving (Eq, Show)

d_named :: String -> SynthDef
d_named = SynthDef

d_default :: SynthDef
d_default = d_named "default"

-- | Compute a unique name for a UGen graph.
-- graphName :: UGen -> String
-- graphName = SHA.showBSasHex . SHA.hash SHA.SHA256 . BZip.compress . Synthdef.graphdef . Synthdef.synth

-- | Create a new synth definition.
-- d_new :: Monad m => String -> UGen -> Async m SynthDef
-- d_new prefix ugen
--     | length prefix < 127 = mkAsync $ return (sd, f)
--     | otherwise = error "d_new: name prefix too long, resulting string exceeds 255 characters"
--     where
--         sd = SynthDef (prefix ++ "-" ++ graphName ugen)
--         f osc = (mkC C.d_recv C.d_recv' osc) (Synthdef.synthdef (name sd) ugen)
d_recv :: Monad m => String -> UGen -> Async m SynthDef
d_recv name ugen
    | length name < 255 = mkAsync $ return (SynthDef name, f)
    | otherwise = error "d_recv: name too long, resulting string exceeds 255 characters"
    where
        f osc = (mkC C.d_recv C.d_recv' osc) (Synthdef.synthdef name ugen)

-- | Remove definition once all nodes using it have ended.
d_free :: Monad m => SynthDef -> RequestT m ()
d_free = send . C.d_free . (:[]) . name

-- ====================================================================
-- Node

class Node a where
    nodeId :: a -> NodeId

data AbstractNode = forall n . (Eq n, Node n, Show n) => AbstractNode n

instance Eq AbstractNode where
    (AbstractNode a) == (AbstractNode b) = nodeId a == nodeId b

instance Node AbstractNode where
    nodeId (AbstractNode n) = nodeId n

instance Show AbstractNode where
    show (AbstractNode n) = show n

n_wrap :: (Eq n, Node n, Show n) => n -> AbstractNode
n_wrap = AbstractNode

-- | Place node @a@ after node @b@.
n_after :: (Node a, Node b, Monad m) => a -> b -> RequestT m ()
n_after a b = send $ C.n_after [(fromIntegral (nodeId a), fromIntegral (nodeId b))]

-- | Place node @a@ before node @b@.
n_before :: (Node a, Node b, Monad m) => a -> b -> RequestT m ()
n_before a b = send $ C.n_after [(fromIntegral (nodeId a), fromIntegral (nodeId b))]

-- | Fill ranges of a node's control values.
n_fill :: (Node a, Monad m) => a -> [(String, Int, Double)] -> RequestT m ()
n_fill n = send . C.n_fill (fromIntegral (nodeId n))

-- | Delete a node.
n_free :: (Node a, MonadIO m) => a -> RequestT m ()
n_free n = do
    send $ C.n_free [fromIntegral (nodeId n)]
    finally $ M.free M.nodeIdAllocator (nodeId n)

-- | Mapping node controls to buses.
class BusMapping n b where
    -- | Map a node's controls to read from a control bus.
    n_map :: (Node n, Bus b, Monad m) => n -> String -> b -> RequestT m ()
    -- | Remove a control's mapping to a control bus.
    n_unmap :: (Node n, Bus b, Monad m) => n -> String -> b -> RequestT m ()

instance BusMapping n ControlBus where
    n_map n c b = send msg
        where
            nid = fromIntegral (nodeId n)
            bid = fromIntegral (busId b)
            msg = if numChannels b > 1
                  then C.n_mapn nid [(c, bid, numChannels b)]
                  else C.n_map  nid [(c, bid)]
    n_unmap n c b = send msg
        where
            nid = fromIntegral (nodeId n)
            msg = if numChannels b > 1
                  then C.n_mapn nid [(c, -1, numChannels b)]
                  else C.n_map  nid [(c, -1)]

instance BusMapping n AudioBus where
    n_map n c b = send msg
        where
            nid = fromIntegral (nodeId n)
            bid = fromIntegral (busId b)
            msg = if numChannels b > 1
                  then C.n_mapan nid [(c, bid, numChannels b)]
                  else C.n_mapa  nid [(c, bid)]
    n_unmap n c b = send msg
        where
            nid = fromIntegral (nodeId n)
            msg = if numChannels b > 1
                  then C.n_mapan nid [(c, -1, numChannels b)]
                  else C.n_mapa  nid [(c, -1)]

-- | Query a node.
n_query_ :: (Node a, Monad m) => a -> RequestT m ()
n_query_ n = send $ C.n_query [fromIntegral (nodeId n)]

-- | Query a node.
n_query :: (Node a, MonadIO m) => a -> RequestT m (Deferred m N.NodeNotification)
n_query n = n_query_ n >> after (N.n_info (nodeId n)) (return ())

-- | Turn node on or off.
n_run_ :: (Node a, Monad m) => a -> Bool -> RequestT m ()
n_run_ n b = send $ C.n_run [(fromIntegral (nodeId n), b)]

-- | Set a node's control values.
n_set :: (Node a, Monad m) => a -> [(String, Double)] -> RequestT m ()
n_set n = send . C.n_set (fromIntegral (nodeId n))

-- | Set ranges of a node's control values.
n_setn :: (Node a, Monad m) => a -> [(String, [Double])] -> RequestT m ()
n_setn n = send . C.n_setn (fromIntegral (nodeId n))

-- | Trace a node.
n_trace :: (Node a, Monad m) => a -> RequestT m ()
n_trace n = send $ C.n_trace [fromIntegral (nodeId n)]

-- | Move an ordered sequence of nodes.
n_order :: (Node n, Monad m) => AddAction -> n -> [AbstractNode] -> RequestT m ()
n_order a n = send . C.n_order a (fromIntegral (nodeId n)) . map (fromIntegral.nodeId)

-- ====================================================================
-- Synth

newtype Synth = Synth NodeId deriving (Eq, Ord, Show)

instance Node Synth where
    nodeId (Synth nid) = nid

s_new :: MonadIO m => SynthDef -> AddAction -> Group -> [(String, Double)] -> RequestT m Synth
s_new d a g xs = do
    nid <- M.alloc M.nodeIdAllocator
    send $ C.s_new (name d) (fromIntegral nid) a (fromIntegral (nodeId g)) xs
    return $ Synth nid

s_new_ :: MonadIO m => SynthDef -> AddAction -> [(String, Double)] -> RequestT m Synth
s_new_ d a xs = rootNode >>= \g -> s_new d a g xs

s_release :: (Node a, MonadIO m) => Double -> a -> RequestT m (Deferred m ())
s_release r n = do
    send (C.n_set1 (fromIntegral nid) "gate" r)
    after_ (N.n_end_ nid) (M.free M.nodeIdAllocator nid)
    where nid = nodeId n

-- ====================================================================
-- Group

newtype Group = Group NodeId deriving (Eq, Ord, Show)

instance Node Group where
    nodeId (Group nid) = nid

rootNode :: MonadIdAllocator m => m Group
rootNode = liftM Group M.rootNodeId

g_new :: MonadIO m => AddAction -> Group -> RequestT m Group
g_new a p = do
    nid <- M.alloc M.nodeIdAllocator
    send $ C.g_new [(fromIntegral nid, a, fromIntegral (nodeId p))]
    return $ Group nid

g_new_ :: MonadIO m => AddAction -> RequestT m Group
g_new_ a = rootNode >>= g_new a

g_deepFree :: Monad m => Group -> RequestT m ()
g_deepFree g = send $ C.g_deepFree [fromIntegral (nodeId g)]

g_freeAll :: Monad m => Group -> RequestT m ()
g_freeAll g = send $ C.g_freeAll [fromIntegral (nodeId g)]

g_head :: (Node n, Monad m) => Group -> n -> RequestT m ()
g_head g n = send $ C.g_head [(fromIntegral (nodeId g), fromIntegral (nodeId n))]

g_tail :: (Node n, Monad m) => Group -> n -> RequestT m ()
g_tail g n = send $ C.g_tail [(fromIntegral (nodeId g), fromIntegral (nodeId n))]

g_dumpTree :: Monad m => [(Group, Bool)] -> RequestT m ()
g_dumpTree = send . C.g_dumpTree . map (first (fromIntegral . nodeId))

-- ====================================================================
-- Buffer

newtype Buffer = Buffer { bufferId :: BufferId } deriving (Eq, Ord, Show)

b_alloc :: MonadIO m => Int -> Int -> Async m Buffer
b_alloc n c = mkAsync $ do
    bid <- M.alloc M.bufferIdAllocator
    let f osc = (mkC C.b_alloc C.b_alloc' osc) (fromIntegral bid) n c
    return (Buffer bid, f)

b_read :: MonadIO m =>
    Buffer
 -> FilePath
 -> Maybe Int
 -> Maybe Int
 -> Maybe Int
 -> Bool
 -> Async m ()
b_read (Buffer bid) path
       fileOffset numFrames bufferOffset
       leaveOpen = mkAsync_ f
    where
        f osc = (mkC C.b_read C.b_read' osc)
                    (fromIntegral bid) path
                    (maybe 0 id fileOffset)
                    (maybe (-1) id numFrames)
                    (maybe 0 id bufferOffset)
                    leaveOpen

data HeaderFormat =
    Aiff
  | Next
  | Wav
  | Ircam
  | Raw
  deriving (Enum, Eq, Read, Show)

data SampleFormat =
    PcmInt8
  | PcmInt16
  | PcmInt24
  | PcmInt32
  | PcmFloat
  | PcmDouble
  | PcmMulaw
  | PcmAlaw
  deriving (Enum, Eq, Read, Show)

headerFormatString :: HeaderFormat -> String
headerFormatString Aiff  = "aiff"
headerFormatString Next  = "next"
headerFormatString Wav   = "wav"
headerFormatString Ircam = "ircam"
headerFormatString Raw   = "raw"

sampleFormatString :: SampleFormat -> String
sampleFormatString PcmInt8   = "int8"
sampleFormatString PcmInt16  = "int16"
sampleFormatString PcmInt24  = "int24"
sampleFormatString PcmInt32  = "int32"
sampleFormatString PcmFloat  = "float"
sampleFormatString PcmDouble = "double"
sampleFormatString PcmMulaw  = "mulaw"
sampleFormatString PcmAlaw   = "alaw"

b_write :: MonadIO m =>
    Buffer
 -> FilePath
 -> HeaderFormat
 -> SampleFormat
 -> Maybe Int
 -> Maybe Int
 -> Bool
 -> Async m ()
b_write (Buffer bid) path
        headerFormat sampleFormat
        fileOffset numFrames
        leaveOpen = mkAsync_ f
    where
        f osc = (mkC C.b_write C.b_write' osc)
                    (fromIntegral bid) path
                    (headerFormatString headerFormat)
                    (sampleFormatString sampleFormat)
                    (maybe 0 id fileOffset)
                    (maybe (-1) id numFrames)
                    leaveOpen

b_free :: MonadIO m => Buffer -> Async m ()
b_free b = mkAsync $ do
    let bid = bufferId b
    M.free M.bufferIdAllocator bid
    let f osc = (mkC C.b_free C.b_free' osc) (fromIntegral bid)
    return ((), f)

b_zero :: MonadIO m => Buffer -> Async m ()
b_zero (Buffer bid) = mkAsync_ f
    where
        f osc = (mkC C.b_zero C.b_zero' osc) (fromIntegral bid)

b_query :: MonadIO m => Buffer -> RequestT m (Deferred m N.BufferInfo)
b_query (Buffer bid) = do
    send (C.b_query [fromIntegral bid])
    after (N.b_info bid) (return ())

-- ====================================================================
-- Bus

-- | Abstract interface for control and audio rate buses.
class Bus a where
    rate :: a -> Rate
    busIdRange :: a -> Range BusId
    freeBus :: MonadIdAllocator m => a -> m ()

-- | Bus id.
busId :: Bus a => a -> BusId
busId = Range.begin . busIdRange

-- | Number of channels of the bus.
numChannels :: Bus a => a -> Int
numChannels = Range.size . busIdRange

-- | Audio bus.
newtype AudioBus = AudioBus { audioBusId :: Range BusId } deriving (Eq, Show)

instance Bus AudioBus where
    rate _ = AR
    busIdRange = audioBusId
    freeBus = M.freeRange M.audioBusIdAllocator . audioBusId

-- | Allocate audio bus with the specified number of channels.
newAudioBus :: MonadIdAllocator m => Int -> m AudioBus
newAudioBus = liftM AudioBus . M.allocRange M.audioBusIdAllocator

-- | Get hardware input bus.
inputBus :: (MonadServer m, Failure AllocFailure m) => Int -> Int -> m AudioBus
inputBus n i = do
    k <- serverOption numberOfOutputBusChannels
    m <- serverOption numberOfInputBusChannels
    let r = Range.sized n (fromIntegral (k+i))
    if Range.begin r < fromIntegral k || Range.end r >= fromIntegral (k+m)
        then failure InvalidId
        else return (AudioBus r)

-- | Get hardware output bus.
outputBus :: (MonadServer m, Failure AllocFailure m) => Int -> Int -> m AudioBus
outputBus n i = do
    k <- serverOption numberOfOutputBusChannels
    let r = Range.sized n (fromIntegral i)
    if Range.begin r < 0 || Range.end r >= fromIntegral k
        then failure InvalidId
        else return (AudioBus r)

-- | Control bus.
newtype ControlBus = ControlBus { controlBusId :: Range BusId } deriving (Eq, Show)

instance Bus ControlBus where
    rate _ = KR
    busIdRange = controlBusId
    freeBus = M.freeRange M.controlBusIdAllocator . controlBusId

-- | Allocate control bus with the specified number of channels.
newControlBus :: MonadIdAllocator m => Int -> m ControlBus
newControlBus = liftM ControlBus . M.allocRange M.controlBusIdAllocator
