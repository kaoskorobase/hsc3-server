module Sound.SC3.Server.Allocator.Range (
    Range
  , range
  , empty
  , lowerBound
  , upperBound
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
import Prelude hiding (null)

-- |Model intervals [a, b[
data Range a = Range !a !a deriving (Eq, Show)

instance NFData a => NFData (Range a) where
    rnf (Range x1 x2) = rnf x1 `seq` rnf x2 `seq` ()

mkRange :: a -> a -> Range a
mkRange a b = Range a b

range :: Ord a => a -> a -> Range a
range a b | a <= b    = mkRange a b
          | otherwise = mkRange b a

empty :: Num a => a -> Range a
empty a = mkRange a a

lowerBound :: Range a -> a
lowerBound (Range a _) = a

upperBound :: Range a -> a
upperBound (Range _ a) = a

size :: Integral a => Range a -> Int
size a = fromIntegral (upperBound a - lowerBound a)

null :: Eq a => Range a -> Bool
null a = lowerBound a == upperBound a

toList :: Enum a => Range a -> [a]
toList a = [lowerBound a..pred (upperBound a)]

within :: Ord a => a -> Range a -> Bool
x `within` a = x >= lowerBound a && x < upperBound a

adjoins :: Eq a => Range a -> Range a -> Bool
a `adjoins` b = (upperBound a == lowerBound b) || (upperBound b == lowerBound a)

overlaps :: Ord a => Range a -> Range a -> Bool
a `overlaps` b = (upperBound a > lowerBound b) || (upperBound b > lowerBound a)

contains :: Ord a => Range a -> Range a -> Bool
a `contains` b = lowerBound b >= lowerBound a && upperBound b <= upperBound a

split :: Integral a => Int -> Range a -> (Range a, Range a)
split n r@(Range l u)
    | n <= 0 = (empty l, r)
    | n >= size r = (r, empty u)
    | otherwise = (mkRange l (l+k), mkRange (l+k) u)
    where k = fromIntegral n

join :: Ord a => Range a -> Range a -> Range a
join a b = mkRange (min (lowerBound a) (lowerBound b)) (max (upperBound a) (upperBound b))
