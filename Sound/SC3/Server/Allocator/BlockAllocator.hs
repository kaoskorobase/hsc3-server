{-# LANGUAGE FlexibleContexts
           , TypeFamilies #-}
module Sound.SC3.Server.Allocator.BlockAllocator
  (
    BlockAllocator
  , Fit(..)
  , cons
  ) where

import           Control.Failure (Failure, failure)
-- import qualified Data.BitSet as Set
import           Data.Ord (comparing)
import           Data.Sequence ((><), ViewL((:<)), (<|))
import qualified Data.Sequence as Seq
import           Sound.SC3.Server.Allocator
import           Sound.SC3.Server.Allocator.Range (Range)
import qualified Sound.SC3.Server.Allocator.Range as Range

type FreeList i = Seq.Seq (Range i)

data Fit = AddressFit | BestFit | WorstFit deriving (Enum, Eq, Show)

data BlockAllocator i = BlockAllocator {
    fit :: Fit
  , freeList :: !(FreeList i)
  -- , usedList :: !(Set.BitSet i)
  } deriving (Eq, Show)

-- TODO: There must be a cleverer way to do this.
coalesce :: Ord i => Range i -> FreeList i -> FreeList i
coalesce r l =
    let (prefix, suffix) = Seq.breakl (Range.adjoins r) l
    in case Seq.viewl suffix of
        Seq.EmptyL -> r <| l
        (r' :< suffix') -> join r r' <| (prefix >< suffix')

insert :: Integral i => Fit -> Range i -> FreeList i -> FreeList i
insert fit r l =
    let f = case fit of 
                AddressFit -> comparing Range.lowerBound
                BestFit    -> comparing Range.size
                WorstFit   -> flip (comparing Range.size)
    in Seq.unstableSortBy f (coalesce r l)

cons :: Fit -> Range i -> BlockAllocator i
cons f = BlockAllocator f . Seq.singleton

ba_alloc :: (Integral i, Failure AllocFailure m) => Int -> BlockAllocator i -> m (Range i, BlockAllocator i)
ba_alloc n a
    | Seq.null (freeList a) = failure NoFreeIds
    | otherwise =
        let fits r = Range.size r >= n
            (prefix, suffix) = Seq.breakl fits (freeList a)
        in case Seq.viewl suffix of
            Seq.EmptyL -> failure NoFreeIds
            r :< suffix' -> let (r', r'') = if Range.size r == n
                                            then (r, Range.empty 0)
                                            else Range.split n r
                           in return (r', a { freeList = insert (fit a) r'' (prefix >< suffix') })

ba_free :: (Integral i, Failure AllocFailure m) => Range i -> BlockAllocator i -> m (BlockAllocator i)
ba_free r a = return a { freeList = insert (fit a) r (freeList a) }

instance (Integral i) => IdAllocator (BlockAllocator i) where
    type Id (BlockAllocator i) = i
    alloc a = do
        (r, a') <- ba_alloc 1 a
        return (Range.lowerBound r, a')
    free i = ba_free (range i (i+1))

instance (Integral i) => RangeAllocator (BlockAllocator i) where
    allocRange = ba_alloc
    freeRange = ba_free
