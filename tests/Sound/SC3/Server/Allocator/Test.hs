{-# LANGUAGE ExistentialQuantification
           , FlexibleContexts
           , GADTs
           , ScopedTypeVariables
           , TypeFamilies #-}
module Sound.SC3.Server.Allocator.Test
    (
        tests
    ) where

import           Sound.SC3.Server.Allocator
import qualified Sound.SC3.Server.Allocator.Wrapped as Wrapped
import qualified Sound.SC3.Server.Allocator.SimpleAllocator
import qualified Sound.SC3.Server.Allocator.SetAllocator
import           Sound.SC3.Server.Allocator.BlockAllocator.FirstFit (Coalescing(LazyCoalescing), Sorting(..))
import qualified Sound.SC3.Server.Allocator.BlockAllocator.FirstFit

import Sound.SC3.Server.Allocator.Range.Test ()

import Test.Framework (Test, testGroup)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.QuickCheck

data WrappedIdAllocator = forall a . (IdAllocator a, Id a ~ Id WrappedIdAllocator, Show a) => WrappedIdAllocator a

instance Show WrappedIdAllocator where
    show (WrappedIdAllocator a) = show a

instance IdAllocator WrappedIdAllocator where
    type Id WrappedIdAllocator = Int
    alloc (WrappedIdAllocator a) = Wrapped.alloc WrappedIdAllocator a
    free i (WrappedIdAllocator a) = Wrapped.free WrappedIdAllocator i a
    statistics (WrappedIdAllocator a) = Wrapped.statistics a

instance Arbitrary Sorting where
    arbitrary = elements (enumFromTo Address DecreasingSize)

instance Arbitrary WrappedIdAllocator where
    arbitrary = do
        r <- arbitrary
        s <- arbitrary
        elements [ WrappedIdAllocator (Sound.SC3.Server.Allocator.SimpleAllocator.cons r)
                 , WrappedIdAllocator (Sound.SC3.Server.Allocator.SetAllocator.cons r)
                 , WrappedIdAllocator (Sound.SC3.Server.Allocator.BlockAllocator.FirstFit.cons s LazyCoalescing r) ]

tests :: [Test]
tests = [ testGroup "Sound.SC3.Server.Allocator"
            [ testGroup "IdAllocator"
                [ testProperty "initial statistics" $ \(a :: WrappedIdAllocator) ->
                    let s = statistics a
                    in numFree s == numAvailable s && numUsed s == 0
                , testProperty "statistics after allocating something" $ \(a :: WrappedIdAllocator) (n :: Int) ->
                    let n' = max 0 (min n (numAvailable (statistics a)))
                        Just (_, a') = allocMany n' a
                        s = statistics a'
                    in numFree s == (numAvailable s - n') && numUsed s == n'
                , testProperty "statistics after allocating everything" $ \(a :: WrappedIdAllocator) ->
                    let Just (_, a') = allocMany (numAvailable (statistics a)) a
                        s = statistics a'
                    in numFree s == 0 && numUsed s == numAvailable s
                ]
            ]
        ]
