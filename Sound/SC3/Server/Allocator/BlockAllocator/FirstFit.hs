{-# LANGUAGE FlexibleContexts
           , TypeFamilies #-}
module Sound.SC3.Server.Allocator.BlockAllocator.FirstFit
  (
    FirstFitAllocator
  , Sorting(..)
  , Coalescing(..)
  , cons
  , addressFit
  , bestFit
  , worstFit
  ) where

import           Control.DeepSeq (NFData(..))
import           Control.Failure (Failure, failure)
-- import qualified Data.BitSet as Set
import           Sound.SC3.Server.Allocator
import qualified Sound.SC3.Server.Allocator.Range as Range
import           Sound.SC3.Server.Allocator.BlockAllocator.FreeList (FreeList, Sorting(..))
import qualified Sound.SC3.Server.Allocator.BlockAllocator.FreeList as FreeList

data Coalescing = NoCoalescing | LazyCoalescing deriving (Enum, Eq, Show)

data FirstFitAllocator i = FirstFitAllocator {
    coalescing :: Coalescing
  , freeList :: !(FreeList i)
  -- , usedList :: !(Set.BitSet i)
  } deriving (Eq, Show)

instance NFData i => NFData (FirstFitAllocator i) where
    rnf (FirstFitAllocator x1 x2) = x1 `seq` rnf x2 `seq` ()

cons :: Sorting -> Coalescing -> Range i -> FirstFitAllocator i
cons s c = FirstFitAllocator c . FreeList.singleton s

addressFit :: Coalescing -> Range i -> FirstFitAllocator i
addressFit = cons Address

bestFit :: Coalescing -> Range i -> FirstFitAllocator i
bestFit = cons IncreasingSize

worstFit :: Coalescing -> Range i -> FirstFitAllocator i
worstFit = cons DecreasingSize

ba_alloc :: (Integral i, Failure AllocFailure m) => Int -> FirstFitAllocator i -> m (Range i, FirstFitAllocator i)
ba_alloc n a =
    case FreeList.alloc fits (freeList a) of
        Nothing -> case coalescing a of
                    NoCoalescing -> failure NoFreeIds
                    LazyCoalescing -> case FreeList.alloc fits (FreeList.coalesce (freeList a)) of
                                        Nothing -> failure NoFreeIds
                                        Just (r, l) -> alloc r l
        Just (r, l) -> alloc r l
    where
        fits r = Range.size r >= n
        alloc r l =
            if Range.size r == n
            then return (r, a { freeList = l})
            else let (r1, r2) = Range.split n r
                 in return (r1, a { freeList = FreeList.insert r2 l })

ba_free :: (Integral i, Failure AllocFailure m) => Range i -> FirstFitAllocator i -> m (FirstFitAllocator i)
ba_free r a = return a { freeList = FreeList.insert r (freeList a) }

instance (Integral i) => IdAllocator (FirstFitAllocator i) where
    type Id (FirstFitAllocator i) = i
    alloc a = do
        (r, a') <- ba_alloc 1 a
        return (Range.lowerBound r, a')
    free i = ba_free (range i (i+1))

instance (Integral i) => RangeAllocator (FirstFitAllocator i) where
    allocRange = ba_alloc
    freeRange = ba_free
