{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
module Sound.SC3.Server.Allocator.Test (
  tests
) where

import           Sound.SC3.Server.Allocator
import qualified Sound.SC3.Server.Allocator.Wrapped as Wrapped
import qualified Sound.SC3.Server.Allocator.SimpleAllocator
import qualified Sound.SC3.Server.Allocator.SetAllocator
import           Sound.SC3.Server.Allocator.BlockAllocator.FirstFit (Coalescing(..), Sorting(..))
import qualified Sound.SC3.Server.Allocator.BlockAllocator.FirstFit

import Sound.SC3.Server.Allocator.Range.Test ()

import Control.Applicative
import Test.Framework (Test, testGroup)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.QuickCheck

import Debug.Trace

data AnyIdAllocator = forall a . (IdAllocator a, Id a ~ Id AnyIdAllocator, Show a) => AnyIdAllocator a

instance Show AnyIdAllocator where
    show (AnyIdAllocator a) = show a

instance IdAllocator AnyIdAllocator where
  type Id AnyIdAllocator = Int
  alloc (AnyIdAllocator a) = Wrapped.alloc AnyIdAllocator a
  free i (AnyIdAllocator a) = Wrapped.free AnyIdAllocator i a
  statistics (AnyIdAllocator a) = Wrapped.statistics a

instance Arbitrary Sorting where
  arbitrary = elements (enumFromTo Address DecreasingSize)

instance Arbitrary Coalescing where
  --arbitrary = elements (enumFromTo NoCoalescing LazyCoalescing)
  arbitrary = return LazyCoalescing

instance Arbitrary AnyIdAllocator where
  arbitrary = oneof [
      AnyIdAllocator . Sound.SC3.Server.Allocator.SimpleAllocator.cons
        <$> arbitrary
    , AnyIdAllocator . Sound.SC3.Server.Allocator.SetAllocator.cons
        <$> arbitrary
    , AnyIdAllocator <$>
        (Sound.SC3.Server.Allocator.BlockAllocator.FirstFit.cons
          <$> arbitrary
          <*> arbitrary
          <*> arbitrary) ]

data AnyRangeAllocator =
  forall a . (RangeAllocator a, Id a ~ Id AnyRangeAllocator, Show a) =>
    AnyRangeAllocator a

instance Show AnyRangeAllocator where
  show (AnyRangeAllocator a) = show a

instance IdAllocator AnyRangeAllocator where
  type Id AnyRangeAllocator = Int
  alloc (AnyRangeAllocator a) = Wrapped.alloc AnyRangeAllocator a
  free i (AnyRangeAllocator a) = Wrapped.free AnyRangeAllocator i a
  statistics (AnyRangeAllocator a) = Wrapped.statistics a

instance RangeAllocator AnyRangeAllocator where
  allocRange i (AnyRangeAllocator a) = Wrapped.allocRange AnyRangeAllocator i a
  freeRange r (AnyRangeAllocator a) = Wrapped.freeRange AnyRangeAllocator r a

instance Arbitrary AnyRangeAllocator where
  arbitrary = AnyRangeAllocator
                <$> (Sound.SC3.Server.Allocator.BlockAllocator.FirstFit.cons
                      <$> arbitrary
                      <*> pure LazyCoalescing
                      <*> arbitrary)

allocAll !a rs =
  if numFree (statistics a) == 0
  then return (a, rs)
  else do
    n <- choose (1, min 4 (numFree (statistics a)))
    let Just (r, a') = allocRange n a
    allocAll a' (r:rs)

freeAll !a rs = do
  case rs of
    [] -> return a
    (r:rs) -> do
      let Just a' = freeRange r a
      freeAll a' (reverse rs)

tests :: [Test]
tests =
  [ testGroup "Sound.SC3.Server.Allocator"
    [ testGroup "IdAllocator"
      [ testProperty "initial statistics" $ \(a :: AnyIdAllocator) ->
        let s = statistics a
        in numFree s == numAvailable s && numUsed s == 0
      , testProperty "statistics after allocating something" $ \(a :: AnyIdAllocator) (n :: Int) ->
        let n' = max 0 (min n (numAvailable (statistics a)))
            Just (_, a') = allocMany n' a
            s = statistics a'
        in numFree s == (numAvailable s - n') && numUsed s == n'
      , testProperty "statistics after allocating everything" $ \(a :: AnyIdAllocator) ->
        let Just (_, a') = allocMany (numAvailable (statistics a)) a
            s = statistics a'
        in numFree s == 0 && numUsed s == numAvailable s
      , testProperty "statistics stay the same after allocating and freeing everything (RangeAllocator)" $ \(a :: AnyRangeAllocator) -> do
        let s = statistics a
        (a, rs) <- allocAll a []
        a <- freeAll a rs
        return $ statistics a == s
      ]
    ]
  ]
