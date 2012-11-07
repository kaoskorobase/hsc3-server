module Sound.SC3.Server.Allocator.Range (
    Range
  , range
  , sized
  , empty
  , begin
  , end
  , last
  , size
  , null
  , toList
  -- *Interval operations
  , within
  , adjoins
  , overlaps
  , contains
  , split
  , join
) where

import Control.DeepSeq (NFData(..))
import Prelude hiding (last, null)

-- | Open ended interval [begin, end).
data Range a = Range {
    begin :: !a
  , end :: !a
  } deriving (Eq, Show)

instance NFData a => NFData (Range a) where
    rnf (Range x1 x2) = rnf x1 `seq` rnf x2 `seq` ()

mkRange :: a -> a -> Range a
mkRange a b = Range a b

range :: Ord a => a -> a -> Range a
range a b | a <= b    = mkRange a b
          | otherwise = mkRange b a

sized :: Num a => Int -> a -> Range a
sized n a = mkRange a (a + fromIntegral n)

empty :: Num a => a -> Range a
empty a = mkRange a a

last :: Enum a => Range a -> a
last = pred . end

size :: Integral a => Range a -> Int
size a = fromIntegral (end a - begin a)

null :: Eq a => Range a -> Bool
null a = begin a == end a

toList :: Enum a => Range a -> [a]
toList a = [begin a..last a]

within :: Ord a => a -> Range a -> Bool
x `within` a = x >= begin a && x < end a

adjoins :: Eq a => Range a -> Range a -> Bool
a `adjoins` b = (end a == begin b) || (end b == begin a)

overlaps :: Ord a => Range a -> Range a -> Bool
a `overlaps` b = (end a > begin b) || (end b > begin a)

contains :: Ord a => Range a -> Range a -> Bool
a `contains` b = begin b >= begin a && end b <= end a

split :: Integral a => Int -> Range a -> (Range a, Range a)
split n r@(Range l u)
    | n <= 0 = (empty l, r)
    | n >= size r = (r, empty u)
    | otherwise = (mkRange l (l+k), mkRange (l+k) u)
    where k = fromIntegral n

join :: Ord a => Range a -> Range a -> Range a
join a b = mkRange (min (begin a) (begin b)) (max (end a) (end b))
