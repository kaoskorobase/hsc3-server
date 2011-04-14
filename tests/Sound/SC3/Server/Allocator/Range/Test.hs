{-# LANGUAGE ScopedTypeVariables #-}
module Sound.SC3.Server.Allocator.Range.Test
    (
        tests
    ) where

import Sound.SC3.Server.Allocator.Range

import Control.Monad (liftM)
import System.Random (Random)
import Test.Framework (Test, testGroup)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.QuickCheck

instance (Integral i, Arbitrary i, Random i) => Arbitrary (Range i) where
    arbitrary = do
        l <- liftM fromIntegral (arbitrary :: Gen (NonNegative i))
        h <- choose (l, l + 2048)
        return $ range l h

tests :: [Test]
tests =
    [ testGroup "Sound.SC3.Server.Allocator.Range"
        [ testProperty "bounds" $ \(r :: Range Int) -> begin r <= end r
        , testProperty "split/join" $ \(n :: Int) (r :: Range Int) -> uncurry join (split n r) == r
        ]
    ]
