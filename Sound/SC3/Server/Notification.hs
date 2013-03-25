-- | Server notification processors.
module Sound.SC3.Server.Notification (
    Notification(..)
  , hasAddress
  , waitFor
  , waitForAll
  , Status(..)
  , status_reply
  , tr
  , synced
  , done
  , NodeNotification(nodeId, parentGroupId, previousNodeId, nextNodeId)
  , headNodeId, tailNodeId
  , n_go , n_end , n_off , n_on , n_move , n_info
  , n_go_, n_end_, n_off_, n_on_
  , n_set, n_setn
  , BufferInfo(..)
  , b_info
) where

import           Control.Applicative (pure, (<*>))
import qualified Data.ByteString.Char8 as C
import           Data.Int
import           Data.List
import qualified Data.List.Zipper as Zipper
import           Sound.SC3.Server.State (BufferId, NodeId, SyncId)
import           Sound.OSC.Type
import           Sound.OSC.Transport.Monad (RecvOSC(..), SendOSC(..), recvMessage)

-- | A notification transformer, extracting a value from a matching OSC message.
newtype Notification a = Notification { match :: Message -> Maybe a }

instance Functor Notification where
    fmap f = Notification . (.) (fmap f) . match

-- | Wait for an OSC message matching a specific address.
--
-- Returns the matched OSC message.
hasAddress :: String -> Notification Message
hasAddress a = Notification f
    where
        f p@(Message a' _) | a == a' = Just p
        f _ = Nothing

-- | Send an OSC packet and wait for a notification.
--
-- Returns the transformed value.
waitFor :: (RecvOSC m, SendOSC m) => Notification a -> m a
waitFor n = go
  where
    go = do
      msg <- recvMessage
      case match n =<< msg of
        Nothing -> go
        Just a -> return a

-- | Send an OSC packet and wait for a list of notifications.
--
-- Returns the transformed values, in unspecified order.
waitForAll :: (RecvOSC m, SendOSC m) => [Notification a] -> m [a]
waitForAll = go []
  where
    go as [] = return as
    go as ns = do
      msg <- recvMessage
      case msg of
        Nothing -> go as ns
        Just msg ->
          case findMatch msg ns of
            Nothing -> go as ns
            Just (a, ns') -> go (a:as) ns'
    findMatch msg = go . Zipper.fromList
      where
        go z
          | Zipper.endp z = Nothing
          | otherwise =
              let n = Zipper.cursor z
              in case match n msg of
                  Nothing -> go (Zipper.right z)
                  Just a -> Just (a, Zipper.toList (Zipper.delete z))

data Status = Status {
    numUGens          :: Int32
  , numSynths         :: Int32
  , numGroups         :: Int32
  , numSynthDefs      :: Int32
  , avgCPU            :: Float
  , peakCPU           :: Float
  , nominalSampleRate :: Double
  , actualSampleRate  :: Double
} deriving (Eq, Show)

status_reply :: Notification Status
status_reply = Notification f
    where
        f (Message "/status.reply" [Int32 _, Int32 u, Int32 s, Int32 g, Int32 d, Float a, Float p, Double sr, Double sr'])
            = Just $ Status u s g d (realToFrac a) (realToFrac p) sr sr'
        f _ = Nothing

tr :: NodeId -> Maybe Int32 -> Notification Float
tr n = Notification . f
    where
        f (Just i) (Message "/tr" [Int32 n', Int32 i', Float r])
            | fromIntegral n == n' && i == i' = Just r
        f Nothing (Message "/tr" [Int32 n', Int32 _, Float r])
            | fromIntegral n == n' = Just r
        f _ _ = Nothing

synced :: SyncId -> Notification SyncId
synced i = Notification f
    where
        f (Message "/synced" [Int32 j]) | fromIntegral j == i = Just i
        f _ = Nothing

normalize :: String -> String
normalize ('/':s) = s
normalize s       = s

done :: String -> Notification [Datum]
done c = Notification f
    where
        f (Message "/done" (ASCII_String s : xs)) | normalize c == normalize (C.unpack s) = Just xs
        f _ = Nothing

data NodeNotification =
    SynthNotification {
        nodeId :: NodeId
      , parentGroupId :: NodeId
      , previousNodeId :: Maybe NodeId
      , nextNodeId :: Maybe NodeId
      }
  | GroupNotification {
        nodeId :: NodeId
      , parentGroupId :: NodeId
      , previousNodeId :: Maybe NodeId
      , nextNodeId :: Maybe NodeId
      , _headNodeId :: Maybe NodeId
      , _tailNodeId :: Maybe NodeId
      }
  deriving (Eq, Show)

isSynthNotification :: NodeNotification -> Bool
isSynthNotification (SynthNotification _ _ _ _) = True
isSynthNotification _ = False

headNodeId :: NodeNotification -> Maybe NodeId
headNodeId n | isSynthNotification n = Nothing
             | otherwise             = _headNodeId n

tailNodeId :: NodeNotification -> Maybe NodeId
tailNodeId n | isSynthNotification n = Nothing
             | otherwise             = _tailNodeId n

n_notification :: String -> NodeId -> Notification NodeNotification
n_notification s nid = Notification f
    where
        nodeIdToMaybe (-1) = Nothing
        nodeIdToMaybe i    = Just (fromIntegral i)
        f osc =
            case osc of
                Message s' (Int32 nid':xs) ->
                    if s == s' && fromIntegral nid == nid'
                    then case xs of
                            (Int32 g:Int32 p:Int32 n:Int32 b:rest) ->
                                let group = fromIntegral g
                                    prev = nodeIdToMaybe p
                                    next = nodeIdToMaybe n
                                in case b of
                                    1 -> case rest of
                                        [Int32 h, Int32 t] -> Just $ GroupNotification nid group prev next (nodeIdToMaybe h) (nodeIdToMaybe t)
                                        _              -> Nothing
                                    _ -> Just $ SynthNotification nid group prev next
                            _ -> Nothing
                    else Nothing
                _ -> Nothing

n_go :: NodeId -> Notification NodeNotification
n_go = n_notification "/n_go"

n_end :: NodeId -> Notification NodeNotification
n_end = n_notification "/n_end"

n_off :: NodeId -> Notification NodeNotification
n_off = n_notification "/n_off"

n_on :: NodeId -> Notification NodeNotification
n_on = n_notification "/n_on"

n_move :: NodeId -> Notification NodeNotification
n_move = n_notification "/n_move"

n_info :: NodeId -> Notification NodeNotification
n_info = n_notification "/n_info"

n_notification_ :: String -> NodeId -> Notification ()
n_notification_ s nid = Notification f
    where
        f (Message s' (Int32 nid':_))
            | s == s' && fromIntegral nid == nid' = Just ()
        f _ = Nothing

n_go_ :: NodeId -> Notification ()
n_go_ = n_notification_ "/n_go"

n_end_ :: NodeId -> Notification ()
n_end_ = n_notification_ "/n_end"

n_off_ :: NodeId -> Notification ()
n_off_ = n_notification_ "/n_off"

n_on_ :: NodeId -> Notification ()
n_on_ = n_notification_ "/n_on"

n_set :: NodeId -> Notification [(Either Int32 String, Float)]
n_set nid = Notification f
  where
    f (Message "/n_set" (Int32 nid':cs))
      | nid == fromIntegral nid' = mapM ctrl (pairs cs)
    f _ = Nothing
    pairs (a:a':as) = (a, a') : pairs as
    pairs _ = []
    ctrl (Int32 k, Float v) = Just (Left k, v)
    ctrl (ASCII_String k, Float v) = Just (Right (C.unpack k), v)
    ctrl _ = Nothing

n_setn :: NodeId -> Notification [(Either Int32 String, [Float])]
n_setn nid = Notification f
  where
    f (Message "/n_setn" (Int32 nid':cs))
      | nid == fromIntegral nid' = sequence (conv cs)
    f _ = Nothing
    value (Float v) = Just (realToFrac v)
    value _ = Nothing
    conv (Int32 k:Int32 n:xs)    = (pure (,) <*> pure (Left k) <*> mapM value (genericTake n xs)) : conv (genericDrop n xs)
    conv (ASCII_String k:Int32 n:xs) = (pure (,) <*> pure (Right (C.unpack k)) <*> mapM value (genericTake n xs)) : conv (genericDrop n xs)
    conv _ = []

data BufferInfo = BufferInfo {
    numFrames :: Int32
  , numChannels :: Int32
  , sampleRate :: Float
  } deriving (Eq, Show)

b_info :: BufferId -> Notification BufferInfo
b_info bid = Notification f
    where
        f (Message "/b_info" [Int32 bid', Int32 f, Int32 c, Float r])
            | fromIntegral bid == bid' = Just $ BufferInfo f c r
        f _ = Nothing
