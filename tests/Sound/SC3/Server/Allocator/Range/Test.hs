{-# LANGUAGE ScopedTypeVariables #-}
module Sound.SC3.Server.Allocator.Range.Test
    (
        tests
    ) where

import Sound.SC3.Server.Allocator.Range

import Test.Framework (Test, testGroup)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.QuickCheck

instance (Integral i, Arbitrary i) => Arbitrary (Range i) where
    arbitrary = do
        l <- arbitrary :: Gen (NonNegative i)
        h <- (arbitrary :: Gen (NonNegative i)) `suchThat` (>=l)
        return $ range (fromIntegral l) (fromIntegral h)

tests :: [Test]
tests =
    [ testGroup "properties"
        [ testProperty "bounds" $ \(r :: Range Int) -> lowerBound r <= upperBound r
        , testProperty "split/join" $ \(n :: Int) (r :: Range Int) -> uncurry join (split n r) == r
        ]
    ]

