-- | Server notification processors.
module Sound.SC3.Server.Notification (
    Notification
  , hasAddress
  , Status(..)
  , status_reply
  , tr
  , synced
  , done
  , NodeNotification(..)
  , n_go , n_end , n_off , n_on , n_move , n_info
  , BufferInfo(..)
  , b_info
) where

import Sound.SC3.Server.State (BufferId, NodeId, SyncId)
import Sound.OpenSoundControl (OSC(..), Datum(..))

-- | A notification transformer, extracting a value from a matching OSC packet.
type Notification a = OSC -> Maybe a

-- | Wait for an OSC message matching a specific address.
--
-- Returns the matched OSC message.
hasAddress :: String -> Notification OSC
hasAddress a m@(Message a' _) = if a == a' then Just m else Nothing
hasAddress _ _                = Nothing

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
status_reply (Message "/status.reply" [Int _, Int u, Int s, Int g, Int d, Float a, Float p, Double sr, Double sr']) =
    Just $ Status u s g d a p sr sr'
status_reply _ = Nothing

tr :: NodeId -> Maybe Int -> Notification Double
tr n (Just i) (Message "/tr" [Int n', Int i', Float r]) | fromIntegral n == n' && i == i' = Just r
tr n Nothing  (Message "/tr" [Int n', Int _, Float r])  | fromIntegral n == n' = Just r
tr _ _        _                                         = Nothing

synced :: SyncId -> Notification SyncId
synced i (Message "/synced" [Int j]) | fromIntegral j == i = Just i
synced _ _                                                 = Nothing

normalize :: String -> String
normalize ('/':s) = s
normalize s       = s

done :: String -> Notification [Datum]
done c (Message "/done" (String s:xs)) | normalize c == normalize s = Just xs
done _ _                                                            = Nothing

data NodeNotification =
    SynthNotification NodeId NodeId NodeId NodeId
  | GroupNotification NodeId NodeId NodeId NodeId NodeId NodeId

n_notification :: String -> NodeId -> Notification NodeNotification
n_notification s nid (Message s' (Int nid':Int g:Int p:Int n:Int b:r))
    | s == s' && fromIntegral nid == nid' =
        case b of
            0 -> Just $ SynthNotification nid (fromIntegral g) (fromIntegral p) (fromIntegral n)
            _ -> case r of
                    [Int h, Int t] -> Just $ GroupNotification nid (fromIntegral g) (fromIntegral p) (fromIntegral n) (fromIntegral h) (fromIntegral t)
                    _              -> Just $ GroupNotification nid (fromIntegral g) (fromIntegral p) (fromIntegral n) (fromIntegral (-1 :: Int)) (fromIntegral (-1 :: Int))
n_notification _ _ _ = Nothing

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

data BufferInfo = BufferInfo {
    numFrames :: Int
  , numChannels :: Int
  , sampleRate :: Double
  } deriving (Eq, Show)

b_info :: BufferId -> Notification BufferInfo
b_info bid (Message "/b_info" [Int bid', Int f, Int c, Float r])
    | fromIntegral bid == bid' = Just $ BufferInfo f c r
b_info _ _ = Nothing
