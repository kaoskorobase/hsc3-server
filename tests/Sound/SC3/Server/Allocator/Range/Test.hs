{-# LANGUAGE ScopedTypeVariables #-}
module Sound.SC3.Server.Allocator.Range.Test
    (
        tests
    ) where

import Sound.SC3.Server.Allocator.Range
import Prelude hiding (null)

import Control.Applicative ((<$>), (<*>))
import Test.Framework (Test, testGroup)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.QuickCheck (Arbitrary(..), NonNegative)

instance (Integral i, Arbitrary i) => Arbitrary (Range i) where
    arbitrary = sized <$> ((`mod`10000) <$> arbitrary) <*> arbitrary

tests :: [Test]
tests =
    [ testGroup "Sound.SC3.Server.Allocator.Range"
        [ testProperty "bounds" $ \(r :: Range Int) -> begin r <= end r
        , testProperty "split" $ \(n :: NonNegative Int) (r :: Range Int) ->
            let n' = fromIntegral n
                (r1, r2) = split n' r in
            if n' > size r
                then r1 == r && null r2
                else size r1 == n' && size r2 == (size r - n')
        , testProperty "split/join" $ \(n :: Int) (r :: Range Int) -> uncurry join (split n r) == r
        ]
    ]
