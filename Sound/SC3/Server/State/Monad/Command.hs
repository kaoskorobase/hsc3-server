{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Sound.SC3.Server.State.Monad.Command (
-- * Requests
  Request
, R.exec
, R.exec_
, Result
, R.extract
-- * Master controls
, status
, statusM
, PrintLevel(..)
, dumpOSC
, clearSched
, ErrorScope(..)
, ErrorMode(..)
, errorMode
-- * Synth definitions
, SynthDef(name)
-- , d_recv
, d_named
, d_default
, d_recv
, d_load
, d_loadDir
, d_free
-- * Resources
-- ** Nodes
, Node(..)
, AddAction(..)
, AbstractNode
, node
, n_after
, n_before
, n_fill
, n_free
, BusMapping(..)
, n_query_
, n_query
, n_queryM
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
, s_get
, s_getn
, s_noid
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
--, g_queryTree
-- ** Plugin Commands
, cmd
-- ** Unit Generator Commands
, u_cmd
-- ** Buffers
, Buffer
, bufferId
, b_alloc
, b_allocRead
, b_allocReadChannel
, b_read
, b_readChannel
, SoundFileFormat(..)
, SampleFormat(..)
, b_write
, b_free
, b_zero
, b_set
, b_setn
, b_fill
, b_gen
, b_gen_sine1
, b_gen_sine2
, b_gen_sine3
, b_gen_cheby
, b_gen_copy
, b_close
, b_query
, b_queryM
--, b_get
--, b_getn
-- ** Buses
, Bus(..)
, AudioBus(audioBusIdRange)
, audioBusId
, inputBus
, outputBus
, newAudioBus
, ControlBus(controlBusIdRange)
, controlBusId
, newControlBus
-- *** Control Bus Commands
--, c_set
--, c_setn
--, c_fill
--, c_get
--, c_getn
) where

--import qualified Codec.Compression.BZip as BZip
--import qualified Codec.Digest.SHA as SHA
import           Control.Arrow (first)
import           Control.Failure (Failure, failure)
import           Control.Monad (liftM, unless)
import           Control.Monad.IO.Class (MonadIO)
import           Sound.OpenSoundControl (Datum(..), OSC(..))
import           Sound.SC3 (Rate(..), UGen)
import           Sound.SC3.Server.Allocator.Range (Range)
import qualified Sound.SC3.Server.Allocator.Range as Range
import qualified Sound.SC3.Server.Command.Completion as C
import qualified Sound.SC3.Server.Synthdef as Synthdef
import           Sound.SC3.Server.Allocator (AllocFailure(..))
import           Sound.SC3.Server.Command (AddAction(..), ErrorScope(..), ErrorMode(..), PrintLevel(..))
import qualified Sound.SC3.Server.Command as C
import           Sound.SC3.Server.Enum (SoundFileFormat(..), SampleFormat(..))
import qualified Sound.SC3.Server.Notification as N
import           Sound.SC3.Server.Process.Options (ServerOptions(..))
import           Sound.SC3.Server.State (AudioBusId, BufferId, ControlBusId, NodeId)
import           Sound.SC3.Server.State.Monad (sendOSC)
import qualified Sound.SC3.Server.State.Monad as M
import           Sound.SC3.Server.State.Monad.Class (MonadIdAllocator, MonadServer, RequestOSC, serverOption)
import           Sound.SC3.Server.State.Monad.Request (Request, Result, after_, finally, mkAsync, mkAsync_, mkSync, waitFor)
import qualified Sound.SC3.Server.State.Monad.Request as R
import           Sound.SC3.UGen.Enum (B_Gen)

-- ====================================================================
-- Utils

-- | Construct a function suitable for 'mkAsync'.
mkC :: OSC o => a -> (o -> a) -> (Maybe o -> a)
mkC f _ Nothing    = f
mkC _ f (Just osc) = f osc

get :: (MonadIdAllocator m, RequestOSC m, MonadIO m) => Request m (Result a) -> m a
get m = R.exec_ m >>= R.extract

withSync :: MonadIdAllocator m => OSC o => o -> Request m ()
withSync c = do
  sendOSC c
  sendOSC =<< mkSync

-- ====================================================================
-- Master controls

-- | Request server status.
status :: MonadIO m => Request m (Result N.Status)
status = do
  sendOSC C.status
  waitFor N.status_reply

-- | Request server status.
statusM :: (MonadIdAllocator m, RequestOSC m, MonadIO m) => m N.Status
statusM = get status

-- | Select printing of incoming Open Sound Control messages.
dumpOSC :: MonadIdAllocator m => PrintLevel -> Request m ()
dumpOSC p = withSync (C.dumpOSC p)

-- | Remove all bundles from the scheduling queue.
clearSched :: Monad m => Request m ()
clearSched = sendOSC C.clearSched

-- | Set error posting scope and mode.
errorMode :: Monad m => ErrorScope -> ErrorMode -> Request m ()
errorMode scope = sendOSC . C.errorMode scope

-- ====================================================================
-- Synth definitions

newtype SynthDef = SynthDef {
    name  :: String
  } deriving (Eq, Show)

-- | Construct a synth definition from a name.
d_named :: String -> SynthDef
d_named = SynthDef

-- | The default synth definition.
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

-- | Create a synth definition from a name and a UGen graph.
d_recv :: Monad m => String -> UGen -> Request m SynthDef
d_recv name ugen
    | length name < 255 = mkAsync $ return (SynthDef name, f)
    | otherwise = error "d_recv: name too long, resulting string exceeds 255 characters"
    where
        f osc = (mkC C.d_recv C.d_recv' osc) (Synthdef.synthdef name ugen)

-- | Load a synth definition from a named file. (Asynchronous)
d_load :: Monad m => FilePath -> Request m ()
d_load fp = mkAsync_ $ \osc -> mkC C.d_load C.d_load' osc $ fp

-- | Load a directory of synth definition files. (Asynchronous)
d_loadDir :: Monad m => FilePath -> Request m ()
d_loadDir fp = mkAsync_ $ \osc -> mkC C.d_loadDir C.d_loadDir' osc $ fp

-- | Remove definition once all nodes using it have ended.
d_free :: Monad m => SynthDef -> Request m ()
d_free = sendOSC . C.d_free . (:[]) . name

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

-- | Construct an abstract node wrapper.
node :: (Eq n, Node n, Show n) => n -> AbstractNode
node = AbstractNode

-- | Place node @a@ after node @b@.
n_after :: (Node a, Node b, Monad m) => a -> b -> Request m ()
n_after a b = sendOSC $ C.n_after [(fromIntegral (nodeId a), fromIntegral (nodeId b))]

-- | Place node @a@ before node @b@.
n_before :: (Node a, Node b, Monad m) => a -> b -> Request m ()
n_before a b = sendOSC $ C.n_after [(fromIntegral (nodeId a), fromIntegral (nodeId b))]

-- | Fill ranges of a node's control values.
n_fill :: (Node a, Monad m) => a -> [(String, Int, Double)] -> Request m ()
n_fill n = sendOSC . C.n_fill (fromIntegral (nodeId n))

-- | Delete a node.
n_free :: (Node a, MonadIdAllocator m) => a -> Request m ()
n_free n = do
    sendOSC $ C.n_free [fromIntegral (nodeId n)]
    finally $ M.free M.nodeIdAllocator (nodeId n)

-- | Mapping node controls to buses.
class BusMapping n b where
    -- | Map a node's controls to read from a control bus.
    n_map :: (Node n, Bus b, Monad m) => n -> String -> b -> Request m ()
    -- | Remove a control's mapping to a control bus.
    n_unmap :: (Node n, Bus b, Monad m) => n -> String -> b -> Request m ()

instance BusMapping n ControlBus where
    n_map n c b = sendOSC msg
        where
            nid = fromIntegral (nodeId n)
            bid = fromIntegral (controlBusId b)
            msg = if numChannels b > 1
                  then C.n_mapn nid [(c, bid, numChannels b)]
                  else C.n_map  nid [(c, bid)]
    n_unmap n c b = sendOSC msg
        where
            nid = fromIntegral (nodeId n)
            msg = if numChannels b > 1
                  then C.n_mapn nid [(c, -1, numChannels b)]
                  else C.n_map  nid [(c, -1)]

instance BusMapping n AudioBus where
    n_map n c b = sendOSC msg
        where
            nid = fromIntegral (nodeId n)
            bid = fromIntegral (audioBusId b)
            msg = if numChannels b > 1
                  then C.n_mapan nid [(c, bid, numChannels b)]
                  else C.n_mapa  nid [(c, bid)]
    n_unmap n c b = sendOSC msg
        where
            nid = fromIntegral (nodeId n)
            msg = if numChannels b > 1
                  then C.n_mapan nid [(c, -1, numChannels b)]
                  else C.n_mapa  nid [(c, -1)]

-- | Query a node.
n_query_ :: (Node a, Monad m) => a -> Request m ()
n_query_ n = sendOSC (C.n_query [fromIntegral (nodeId n)])

-- | Query a node.
n_query :: (Node a, MonadIO m) => a -> Request m (Result N.NodeNotification)
n_query n = do
  n_query_ n
  waitFor (N.n_info (nodeId n))

-- | Query a node.
n_queryM :: (Node a, MonadIdAllocator m, RequestOSC m, MonadIO m) => a -> m N.NodeNotification
n_queryM = get . n_query

-- | Turn node on or off.
n_run_ :: (Node a, Monad m) => a -> Bool -> Request m ()
n_run_ n b = sendOSC $ C.n_run [(fromIntegral (nodeId n), b)]

-- | Set a node's control values.
n_set :: (Node a, Monad m) => a -> [(String, Double)] -> Request m ()
n_set n = sendOSC . C.n_set (fromIntegral (nodeId n))

-- | Set ranges of a node's control values.
n_setn :: (Node a, Monad m) => a -> [(String, [Double])] -> Request m ()
n_setn n = sendOSC . C.n_setn (fromIntegral (nodeId n))

-- | Trace a node.
n_trace :: (Node a, Monad m) => a -> Request m ()
n_trace n = sendOSC $ C.n_trace [fromIntegral (nodeId n)]

-- | Move an ordered sequence of nodes.
n_order :: (Node n, Monad m) => AddAction -> n -> [AbstractNode] -> Request m ()
n_order a n = sendOSC . C.n_order a (fromIntegral (nodeId n)) . map (fromIntegral.nodeId)

-- ====================================================================
-- Synth

newtype Synth = Synth NodeId deriving (Eq, Ord, Show)

instance Node Synth where
    nodeId (Synth nid) = nid

-- | Create a new synth.
s_new :: MonadIdAllocator m => SynthDef -> AddAction -> Group -> [(String, Double)] -> Request m Synth
s_new d a g xs = do
    nid <- M.alloc M.nodeIdAllocator
    sendOSC $ C.s_new (name d) (fromIntegral nid) a (fromIntegral (nodeId g)) xs
    return $ Synth nid

-- | Create a new synth in the root group.
s_new_ :: (MonadServer m, MonadIdAllocator m) => SynthDef -> AddAction -> [(String, Double)] -> Request m Synth
s_new_ d a xs = rootNode >>= \g -> s_new d a g xs

-- | Release a synth with a "gate" envelope control.
s_release :: MonadIdAllocator m => Double -> Synth -> Request m ()
s_release r s = do
  sendOSC (C.n_set1 (fromIntegral nid) "gate" r)
  after_ (N.n_end_ nid) (M.free M.nodeIdAllocator nid)
  where nid = nodeId s

-- | Get control values.
s_get :: MonadIO m => Synth -> [String] -> Request m (Result [(Either Int String, Double)])
s_get s cs = do
  sendOSC (C.s_get (fromIntegral nid) cs)
  waitFor (N.n_set nid)
  where nid = nodeId s

-- | Get ranges of control values.
s_getn :: MonadIO m => Synth -> [(String, Int)] -> Request m (Result [(Either Int String, [Double])])
s_getn s cs = do
  sendOSC (C.s_getn (fromIntegral nid) cs)
  waitFor (N.n_setn nid)
  where nid = nodeId s

-- | Free a synth's ID and auto-reassign it to a reserved value (the node is not freed!).
s_noid :: MonadIdAllocator m => Synth -> Request m ()
s_noid s = do
  sendOSC (C.s_noid [fromIntegral nid])
  M.free M.nodeIdAllocator nid
  where nid = nodeId s

-- ====================================================================
-- Group

newtype Group = Group NodeId deriving (Eq, Ord, Show)

instance Node Group where
    nodeId (Group nid) = nid

-- | Return the server's root group.
rootNode :: MonadServer m => m Group
rootNode = liftM Group M.rootNodeId

-- | Create a new group.
g_new :: MonadIdAllocator m => AddAction -> Group -> Request m Group
g_new a p = do
    nid <- M.alloc M.nodeIdAllocator
    sendOSC $ C.g_new [(fromIntegral nid, a, fromIntegral (nodeId p))]
    return $ Group nid

-- | Create a new group in the top level group.
g_new_ :: (MonadServer m, MonadIdAllocator m) => AddAction -> Request m Group
g_new_ a = rootNode >>= g_new a

-- | Free all synths in this group and all its sub-groups.
g_deepFree :: Monad m => Group -> Request m ()
g_deepFree g = sendOSC $ C.g_deepFree [fromIntegral (nodeId g)]

-- | Delete all nodes in a group.
g_freeAll :: Monad m => Group -> Request m ()
g_freeAll g = sendOSC $ C.g_freeAll [fromIntegral (nodeId g)]

-- | Add node to head of group.
g_head :: (Node n, Monad m) => Group -> n -> Request m ()
g_head g n = sendOSC $ C.g_head [(fromIntegral (nodeId g), fromIntegral (nodeId n))]

-- | Add node to tail of group.
g_tail :: (Node n, Monad m) => Group -> n -> Request m ()
g_tail g n = sendOSC $ C.g_tail [(fromIntegral (nodeId g), fromIntegral (nodeId n))]

-- | Post a representation of a group's node subtree, optionally including the current control values for synths.
g_dumpTree :: Monad m => [(Group, Bool)] -> Request m ()
g_dumpTree = sendOSC . C.g_dumpTree . map (first (fromIntegral . nodeId))

-- ====================================================================
-- Plugin Commands

-- | Send a plugin command.
cmd :: Monad m => String -> [Datum] -> Request m ()
cmd s = sendOSC . C.cmd s

-- ====================================================================
-- Unit Generator Commands

-- | Send a command to a unit generator.
u_cmd :: Monad m => AbstractNode -> Int -> String -> [Datum] -> Request m ()
u_cmd n i s = sendOSC . C.u_cmd (fromIntegral (nodeId n)) i s

-- ====================================================================
-- Buffer Commands

newtype Buffer = Buffer { bufferId :: BufferId } deriving (Eq, Ord, Show)

-- | Allocates zero filled buffer to number of channels and samples. (Asynchronous)
b_alloc :: MonadIdAllocator m => Int -> Int -> Request m Buffer
b_alloc n c = mkAsync $ do
    bid <- M.alloc M.bufferIdAllocator
    let f osc = (mkC C.b_alloc C.b_alloc' osc) (fromIntegral bid) n c
    return (Buffer bid, f)

-- | Allocate buffer space and read a sound file. (Asynchronous)
b_allocRead :: MonadIdAllocator m => FilePath -> Maybe Int -> Maybe Int -> Request m Buffer
b_allocRead path fileOffset numFrames = mkAsync $ do
  bid <- M.alloc M.bufferIdAllocator
  let f osc = (mkC C.b_allocRead C.b_allocRead' osc)
                (fromIntegral bid) path
                (maybe 0 id fileOffset)
                (maybe (-1) id numFrames)
  return (Buffer bid, f)

-- | Allocate buffer space and read a sound file, picking specific channels. (Asynchronous)
b_allocReadChannel :: MonadIdAllocator m => FilePath -> Maybe Int -> Maybe Int -> [Int] -> Request m Buffer
b_allocReadChannel path fileOffset numFrames channels = mkAsync $ do
  bid <- M.alloc M.bufferIdAllocator
  let f osc = (mkC C.b_allocReadChannel C.b_allocReadChannel' osc)
                (fromIntegral bid) path
                (maybe 0 id fileOffset)
                (maybe (-1) id numFrames)
                channels
  return (Buffer bid, f)

-- | Read sound file data into an existing buffer. (Asynchronous)
b_read :: Monad m =>
    Buffer
 -> FilePath
 -> Maybe Int
 -> Maybe Int
 -> Maybe Int
 -> Bool
 -> Request m ()
b_read (Buffer bid) path fileOffset numFrames bufferOffset leaveOpen =
  mkAsync_ $ \osc -> (mkC C.b_read C.b_read' osc)
                      (fromIntegral bid) path
                      (maybe 0 id fileOffset)
                      (maybe (-1) id numFrames)
                      (maybe 0 id bufferOffset)
                      leaveOpen

-- | Read sound file data into an existing buffer, picking specific channels. (Asynchronous)
b_readChannel :: MonadIO m =>
    Buffer
 -> FilePath
 -> Maybe Int
 -> Maybe Int
 -> Maybe Int
 -> Bool
 -> [Int]
 -> Request m ()
b_readChannel (Buffer bid) path fileOffset numFrames bufferOffset leaveOpen channels =
  mkAsync_ $ \osc -> (mkC C.b_readChannel C.b_readChannel' osc)
                      (fromIntegral bid) path
                      (maybe 0 id fileOffset)
                      (maybe (-1) id numFrames)
                      (maybe 0 id bufferOffset)
                      leaveOpen
                      channels

-- | Write sound file data. (Asynchronous)
b_write :: MonadIO m =>
    Buffer
 -> FilePath
 -> SoundFileFormat
 -> SampleFormat
 -> Maybe Int
 -> Maybe Int
 -> Bool
 -> Request m ()
b_write (Buffer bid) path
        soundFileFormat sampleFormat
        fileOffset numFrames
        leaveOpen = mkAsync_ f
    where
        f osc = (mkC C.b_write C.b_write' osc)
                    (fromIntegral bid) path
                    soundFileFormat
                    sampleFormat
                    (maybe 0 id fileOffset)
                    (maybe (-1) id numFrames)
                    leaveOpen

-- | Free buffer. (Asynchronous)
b_free :: MonadIdAllocator m => Buffer -> Request m ()
b_free b = mkAsync $ do
    let bid = bufferId b
    M.free M.bufferIdAllocator bid
    let f osc = (mkC C.b_free C.b_free' osc) (fromIntegral bid)
    return ((), f)

-- | Zero sample data. (Asynchronous)
b_zero :: MonadIO m => Buffer -> Request m ()
b_zero buffer = mkAsync_ $ \osc ->
  (mkC C.b_zero C.b_zero' osc)
    (fromIntegral (bufferId buffer))

-- | Set sample values.
b_set :: Monad m => Buffer -> [(Int, Double)] -> Request m ()
b_set buffer = sendOSC . C.b_set (fromIntegral (bufferId buffer))

-- | Set ranges of sample values.
b_setn :: Monad m => Buffer -> [(Int, [Double])] -> Request m ()
b_setn buffer = sendOSC . C.b_setn (fromIntegral (bufferId buffer))

-- | Fill ranges of sample values.
b_fill :: Monad m => Buffer -> [(Int, Int, Double)] -> Request m ()
b_fill buffer = sendOSC . C.b_fill (fromIntegral (bufferId buffer))

-- | Call a command to fill a buffer. (Asynchronous)
b_gen :: MonadIdAllocator m => Buffer -> String -> [Datum] -> Request m ()
b_gen buffer cmd = withSync . C.b_gen (fromIntegral (bufferId buffer)) cmd

-- | Fill a buffer with partials, specifying amplitudes.
b_gen_sine1 :: MonadIdAllocator m => Buffer -> [B_Gen] -> [Double] -> Request m ()
b_gen_sine1 buffer flags = withSync . C.b_gen_sine1 (fromIntegral (bufferId buffer)) flags

-- | Fill a buffer with partials, specifying frequencies (in cycles per buffer) and amplitudes.
b_gen_sine2 :: MonadIdAllocator m => Buffer -> [B_Gen] -> [(Double, Double)] -> Request m ()
b_gen_sine2 buffer flags = withSync . C.b_gen_sine2 (fromIntegral (bufferId buffer)) flags

-- | Fill a buffer with partials, specifying frequencies (in cycles per buffer), amplitudes and phases.
b_gen_sine3 :: MonadIdAllocator m => Buffer -> [B_Gen] -> [(Double, Double, Double)] -> Request m ()
b_gen_sine3 buffer flags = withSync . C.b_gen_sine3 (fromIntegral (bufferId buffer)) flags

-- | Fills a buffer with a series of chebyshev polynomials.

-- Chebychev polynomials can be defined as:
--
-- cheby(n) = amplitude * cos(n * acos(x))
--
-- The first float value specifies the amplitude for n = 1, the second float
-- value specifies the amplitude for n = 2, and so on. To eliminate a DC offset
-- when used as a waveshaper, the wavetable is offset so that the center value
-- is zero.
b_gen_cheby :: MonadIdAllocator m => Buffer -> [B_Gen] -> [Double] -> Request m ()
b_gen_cheby buffer flags = withSync . C.b_gen_cheby (fromIntegral (bufferId buffer)) flags

-- | Copy samples from the source buffer to the destination buffer.
b_gen_copy :: MonadIdAllocator m => Buffer -> Int -> Buffer -> Int -> Maybe Int -> Request m ()
b_gen_copy buffer sampleOffset srcBuffer srcSampleOffset numSamples =
  withSync $ C.b_gen_copy (fromIntegral (bufferId buffer))
                          sampleOffset
                          (fromIntegral (bufferId srcBuffer))
                          srcSampleOffset
                          numSamples

-- | Close attached soundfile and write header information. (Asynchronous)
b_close :: Monad m => Buffer -> Request m ()
b_close buffer = mkAsync_ $ \osc -> mkC C.b_close C.b_close' osc $ fromIntegral (bufferId buffer)

-- | Request 'BufferInfo'.
b_query :: MonadIO m => Buffer -> Request m (Result N.BufferInfo)
b_query (Buffer bid) = do
    sendOSC (C.b_query [fromIntegral bid])
    waitFor (N.b_info bid)

-- | Request 'BufferInfo'.
b_queryM :: (MonadIdAllocator m, RequestOSC m, MonadIO m) => Buffer -> m N.BufferInfo
b_queryM = get . b_query

-- ====================================================================
-- Bus

-- | Abstract interface for control and audio rate buses.
class Bus a where
  -- | Rate of computation.
  rate :: a -> Rate
  -- | Number of channels.
  numChannels :: a -> Int
  -- | Free bus.
  freeBus :: (MonadServer m, MonadIdAllocator m) => a -> m ()

-- | Audio bus.
newtype AudioBus = AudioBus { audioBusIdRange :: Range AudioBusId } deriving (Eq, Show)

-- | Get audio bus id.
audioBusId :: AudioBus -> AudioBusId
audioBusId = Range.begin . audioBusIdRange

instance Bus AudioBus where
    rate _ = AR
    numChannels = Range.size . audioBusIdRange
    freeBus b = do
        hw <- isHardwareBus b
        unless hw $ M.freeRange M.audioBusIdAllocator (audioBusIdRange b)

-- | Allocate audio bus with the specified number of channels.
newAudioBus :: MonadIdAllocator m => Int -> m AudioBus
newAudioBus = liftM AudioBus . M.allocRange M.audioBusIdAllocator

-- | Return 'True' if bus is a hardware output or input bus.
isHardwareBus :: MonadServer m => AudioBus -> m Bool
isHardwareBus b = do
    no <- serverOption numberOfOutputBusChannels
    ni <- serverOption numberOfInputBusChannels
    return $ audioBusId b >= 0 && audioBusId b < fromIntegral (no + ni)

-- | Get hardware input bus.
inputBus :: (MonadServer m, Failure AllocFailure m) => Int -> Int -> m AudioBus
inputBus n i = do
    k <- serverOption numberOfOutputBusChannels
    m <- serverOption numberOfInputBusChannels
    let r = Range.sized n (fromIntegral (k+i))
    if Range.begin r < fromIntegral k || Range.end r > fromIntegral (k+m)
        then failure InvalidId
        else return (AudioBus r)

-- | Get hardware output bus.
outputBus :: (MonadServer m, Failure AllocFailure m) => Int -> Int -> m AudioBus
outputBus n i = do
    k <- serverOption numberOfOutputBusChannels
    let r = Range.sized n (fromIntegral i)
    if Range.begin r < 0 || Range.end r > fromIntegral k
        then failure InvalidId
        else return (AudioBus r)

-- | Control bus.
newtype ControlBus = ControlBus { controlBusIdRange :: Range ControlBusId } deriving (Eq, Show)

-- | Get control bus ID.
controlBusId :: ControlBus -> ControlBusId
controlBusId = Range.begin . controlBusIdRange

instance Bus ControlBus where
    rate _ = KR
    numChannels = Range.size . controlBusIdRange
    freeBus = M.freeRange M.controlBusIdAllocator . controlBusIdRange

-- | Allocate control bus with the specified number of channels.
newControlBus :: MonadIdAllocator m => Int -> m ControlBus
newControlBus = liftM ControlBus . M.allocRange M.controlBusIdAllocator
