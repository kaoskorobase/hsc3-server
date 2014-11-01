{-# LANGUAGE ScopedTypeVariables #-}
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

import Prelude hiding (join, last, null)

-- $setup
-- >>> import Control.Applicative ((<$>), (<*>))
-- >>> import Control.Monad (liftM)
-- >>> import System.Random (Random)
-- >>> import Test.QuickCheck (Arbitrary(..))
-- >>> instance (Arbitrary i, Ord i) => Arbitrary (Range i) where arbitrary = Range <$> arbitrary <*> arbitrary

-- | Open ended interval [begin, end).
data Range a = Range {
    begin :: !a
  , end :: !a
  } deriving (Eq)

instance Show a => Show (Range a) where
  show r = unwords ["Range", show (begin r), show (end r)]

mkRange :: a -> a -> Range a
mkRange a b = Range a b

-- | Construct a range from a lower bound (included) and an upper bound (excluded).
--
-- prop> \(r :: Range Int) -> begin r == end r
range :: Ord a => a -> a -> Range a
range a b | a <= b    = mkRange a b
          | otherwise = mkRange b a

-- | Construct a range from a size and a lower bound.
--
-- >>> sized 20 10
-- Range 10 30
sized :: Num a => Int -> a -> Range a
sized n a = mkRange a (a + fromIntegral n)

-- | The empty range starting at some value.
--
-- >>> empty 10
-- Range 10 10
--
-- null (empty 10)
-- True
--
-- size (empty 10)
-- 0
empty :: Num a => a -> Range a
empty a = mkRange a a

-- | The last value in the range.
--
-- >>> last (range 10 20)
-- 19
last :: Enum a => Range a -> a
last = pred . end

-- | The size of the range.
--
-- >>> size (range 10 20)
-- 10
--
-- >>> size (sized 100 10)
-- 100
size :: Integral a => Range a -> Int
size a = fromIntegral (end a - begin a)

-- | True if range is empty.
--
-- >>> null (range 10 10)
-- True
--
-- >>> null (range 10 20)
-- False
null :: Eq a => Range a -> Bool
null a = begin a == end a

-- | Convert range to a list of its values.
toList :: Enum a => Range a -> [a]
toList a = [begin a..last a]

-- | Return true if a given value is contained within the range.
--
-- >>> within 12 (sized 3 10)
-- True
--
-- >>> within 20 (range 10 20)
-- False
--
-- >>> within 30 (range 30 30)
-- False
within :: Ord a => a -> Range a -> Bool
x `within` a = x >= begin a && x < end a

-- | Return true if two ranges adjoin each other.
--
-- >>> range 10 20 `adjoins` range 20 30
-- True
--
-- >>> range 10 20 `adjoins` range 21 30
-- False
adjoins :: Eq a => Range a -> Range a -> Bool
a `adjoins` b = (end a == begin b) || (end b == begin a)

-- | Return true if two ranges overlap each other.
overlaps :: Ord a => Range a -> Range a -> Bool
a `overlaps` b = (end a > begin b) || (end b > begin a)

-- | Return true if the second range lies completely within the first range.
contains :: Ord a => Range a -> Range a -> Bool
a `contains` b = begin b >= begin a && end b <= end a

-- | Split a range at an index.
--
-- >>> let (r1, r2) = split 6 (range 10 20)
-- >>> size r1
-- 6
-- >>> size r2
-- 4
--
-- >>> let (r1, r2) = split 6 (empty 6)
-- >>> null r1 && null r2
-- True
--
-- >>> let (r1, r2) = split 10 (sized 4 10)
-- >>> size r1
-- 4
-- >>> size r2
-- 0
split :: Integral a => Int -> Range a -> (Range a, Range a)
split n r@(Range l u)
    | n <= 0 = (empty l, r)
    | n >= size r = (r, empty u)
    | otherwise = (mkRange l (l+k), mkRange (l+k) u)
    where k = fromIntegral n

join :: Ord a => Range a -> Range a -> Range a
join a b = mkRange (min (begin a) (begin b)) (max (end a) (end b))
