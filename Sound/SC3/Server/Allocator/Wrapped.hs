{-# LANGUAGE FlexibleContexts #-}
-- | Helper functions for newtype wrappers.
module Sound.SC3.Server.Allocator.Wrapped
  (
    alloc
  , free
  , statistics
  , allocRange
  , freeRange
  ) where

import           Control.Arrow (second)
import           Control.Failure (Failure)
import           Control.Monad (liftM)
import           Sound.SC3.Server.Allocator (AllocFailure, Id, IdAllocator, Range, RangeAllocator, Statistics)
import qualified Sound.SC3.Server.Allocator as Alloc

alloc :: (Failure AllocFailure m, IdAllocator a) =>
    (a -> a') -> a -> m (Id a, a')
alloc f = liftM (second f) . Alloc.alloc

free :: (Failure AllocFailure m, IdAllocator a) =>
    (a -> a') -> Id a -> a -> m a'
free f i = liftM f . Alloc.free i

statistics :: IdAllocator a => a -> Statistics
statistics = Alloc.statistics

allocRange :: (Failure AllocFailure m, RangeAllocator a) =>
    (a -> a') -> Int -> a -> m (Range (Id a), a')
allocRange f n = liftM (second f) . Alloc.allocRange n

freeRange :: (Failure AllocFailure m, RangeAllocator a) =>
    (a -> a') -> Range (Id a) -> a -> m a'
freeRange f r = liftM f . Alloc.freeRange r
