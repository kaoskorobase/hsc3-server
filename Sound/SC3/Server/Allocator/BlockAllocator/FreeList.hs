module Sound.SC3.Server.Allocator.BlockAllocator.FreeList
  (
    FreeList
  , Sorting(..)
  , empty
  , singleton
  , fromList
  , insert
  , alloc
  , coalesce
  ) where

import           Control.DeepSeq (NFData(..))
import           Data.Ord (comparing)
import qualified Data.List as List
import           Sound.SC3.Server.Allocator.Range (Range)
import qualified Sound.SC3.Server.Allocator.Range as Range

data Sorting = Address | IncreasingSize | DecreasingSize deriving (Enum, Eq, Show)

data FreeList i = FreeList Sorting [Range i] deriving (Eq, Show)

instance NFData i => NFData (FreeList i) where
    rnf (FreeList x1 x2) = x1 `seq` rnf x2 `seq` ()

type SortFunc i = Range i -> Range i -> Ordering

sortFunc :: Integral i => Sorting -> SortFunc i
sortFunc s = f
    where f = case s of
                Address -> comparing Range.lowerBound
                IncreasingSize -> comparing Range.size
                DecreasingSize -> flip (comparing Range.size)

sortBy :: Integral i => Sorting -> [Range i] -> [Range i]
sortBy s = List.sortBy (sortFunc s)

insertBy :: Integral i => Sorting -> Range i -> [Range i] -> [Range i]
insertBy s = List.insertBy (sortFunc s)

empty :: Sorting -> FreeList i
empty s = FreeList s []

singleton :: Sorting -> Range i -> FreeList i
singleton s r = FreeList s [r]

fromList :: Integral i => Sorting -> [Range i] -> FreeList i
fromList s = FreeList s . sortBy s

insert :: Integral i => Range i -> FreeList i -> FreeList i
insert r (FreeList s l) = FreeList s (insertBy s r l)

alloc :: (Range i -> Bool) -> FreeList i -> Maybe (Range i, FreeList i)
alloc p (FreeList s l) =
    case List.break p l of
        (_, []) -> Nothing
        (l1, (r:l2)) -> Just (r, FreeList s (l1 ++ l2))

coalescel :: Ord i => [Range i] -> [Range i]
coalescel [] = []
coalescel l@(_:[]) = l
coalescel (r1:r2:l)
    | Range.adjoins r1 r2 = coalescel (Range.join r1 r2 : l)
    | otherwise           = r1 : coalescel (r2 : l)

sortedByAddress :: Integral i => ([Range i] -> [Range i]) -> FreeList i -> FreeList i
sortedByAddress f (FreeList s l) =
    case s of
        Address -> FreeList s (f l)
        _       -> FreeList s (sortBy s (f (sortBy Address l)))

coalesce :: Integral i => FreeList i -> FreeList i
coalesce = sortedByAddress coalescel
