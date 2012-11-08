-- | Server notification processors.
module Sound.SC3.Server.Notification (
    Notification(..)
  , hasAddress
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

import Control.Applicative (pure, (<*>))
import Sound.SC3.Server.State (BufferId, NodeId, SyncId)
import Sound.OpenSoundControl (OSC(..), Datum(..))

-- | A notification transformer, extracting a value from a matching OSC packet.
newtype Notification a = Notification { match :: OSC -> Maybe a }

instance Functor Notification where
    fmap f = Notification . (.) (fmap f) . match

-- | Wait for an OSC message matching a specific address.
--
-- Returns the matched OSC message.
hasAddress :: String -> Notification OSC
hasAddress a = Notification f
    where
        f m@(Message a' _) = if a == a' then Just m else Nothing
        f _                = Nothing

data Status = Status {
    numUGens          :: Int
  , numSynths         :: Int
  , numGroups         :: Int
  , numSynthDefs      :: Int
  , avgCPU            :: Double
  , peakCPU           :: Double
  , nominalSampleRate :: Double
  , actualSampleRate  :: Double
} deriving (Eq, Show)

status_reply :: Notification Status
status_reply = Notification f
    where
        f (Message "/status.reply" [Int _, Int u, Int s, Int g, Int d, Float a, Float p, Double sr, Double sr'])
            = Just $ Status u s g d a p sr sr'
        f _ = Nothing

tr :: NodeId -> Maybe Int -> Notification Double
tr n = Notification . f
    where
        f (Just i) (Message "/tr" [Int n', Int i', Float r])
            | fromIntegral n == n' && i == i' = Just r
        f Nothing  (Message "/tr" [Int n', Int _, Float r])
            | fromIntegral n == n' = Just r
        f _ _ = Nothing

synced :: SyncId -> Notification SyncId
synced i = Notification f
    where
        f (Message "/synced" [Int j]) | fromIntegral j == i = Just i
        f _                                                 = Nothing

normalize :: String -> String
normalize ('/':s) = s
normalize s       = s

done :: String -> Notification [Datum]
done c = Notification f
    where
        f (Message "/done" (String s:xs)) | normalize c == normalize s = Just xs
        f _                                                            = Nothing

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
                (Message s' (Int nid':xs)) ->
                    if s == s' && fromIntegral nid == nid'
                    then case xs of
                            (Int g:Int p:Int n:Int b:rest) ->
                                let group = fromIntegral g
                                    prev = nodeIdToMaybe p
                                    next = nodeIdToMaybe n
                                in case b of
                                    1 -> case rest of
                                        [Int h, Int t] -> Just $ GroupNotification nid group prev next (nodeIdToMaybe h) (nodeIdToMaybe t)
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
        f (Message s' (Int nid':_))
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

n_set :: NodeId -> Notification [(Either Int String, Double)]
n_set nid = Notification f
  where
    f (Message "/n_set" (Int nid':cs))
      | nid == fromIntegral nid' = mapM ctrl (pairs cs)
    f _ = Nothing
    pairs (a:a':as) = (a, a') : pairs as
    pairs _ = []
    ctrl (Int k, Float v) = Just (Left k, v)
    ctrl (String k, Float v) = Just (Right k, v)
    ctrl _ = Nothing

n_setn :: NodeId -> Notification [(Either Int String, [Double])]
n_setn nid = Notification f
  where
    f (Message "/n_setn" (Int nid':cs))
      | nid == fromIntegral nid' = sequence (conv cs)
    f _ = Nothing
    value (Float v) = Just v
    value _ = Nothing
    conv (Int k:Int n:xs)    = (pure (,) <*> pure (Left k) <*> mapM value (take n xs)) : conv (drop n xs)
    conv (String k:Int n:xs) = (pure (,) <*> pure (Right k) <*> mapM value (take n xs)) : conv (drop n xs)
    conv _ = []

data BufferInfo = BufferInfo {
    numFrames :: Int
  , numChannels :: Int
  , sampleRate :: Double
  } deriving (Eq, Show)

b_info :: BufferId -> Notification BufferInfo
b_info bid = Notification f
    where
        f (Message "/b_info" [Int bid', Int f, Int c, Float r])
            | fromIntegral bid == bid' = Just $ BufferInfo f c r
        f _ = Nothing
