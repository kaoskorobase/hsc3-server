{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
module Sound.SC3.Server.Allocator.SetAllocator (
    SetAllocator
  , cons
) where

import           Control.Failure (Failure, failure)
import           Control.DeepSeq (NFData(..))
import qualified Data.BitSet as Set
import           Sound.SC3.Server.Allocator (AllocFailure(..), IdAllocator(..), Statistics(..))
import           Sound.SC3.Server.Allocator.Range (Range)
import qualified Sound.SC3.Server.Allocator.Range as Range

data SetAllocator i =
    SetAllocator
        {-# UNPACK #-} !(Range i)
        {-# UNPACK #-} !(Set.BitSet i)
                       !i
        deriving (Eq, Show)

instance NFData i => NFData (SetAllocator i) where
    rnf (SetAllocator x1 x2 x3) =
        rnf x1 `seq`
            x2 `seq`
        rnf x3 `seq` ()

cons :: Range i -> SetAllocator i
cons r = SetAllocator r Set.empty (Range.begin r)

-- | Convert an id to a bit index.
--
-- This is necessary to keep the BitSet size bounded between [0, numIds[.
toBit :: Integral i => Range i -> i -> i
toBit r i = i - Range.begin r

findNext :: (Integral i) => SetAllocator i -> Maybe i
findNext (SetAllocator r u i)
    | fromIntegral (Range.size r) == Set.size u = Nothing
    | otherwise = loop i
    where
        wrap i = if i >= Range.end r
                    then Range.begin r
                    else i
        loop !i = let i' = wrap (i+1)
                  in if Set.member (toBit r i') u
                     then loop i'
                     else Just i'

_alloc :: (Integral i, Failure AllocFailure m) => SetAllocator i -> m (i, SetAllocator i)
_alloc a@(SetAllocator r u i) =
    case findNext a of
        Nothing -> failure NoFreeIds
        Just i' -> return (i, SetAllocator r (Set.insert (toBit r i) u) i')

_free :: (Integral i, Failure AllocFailure m) => i -> SetAllocator i -> m (SetAllocator i)
_free i (SetAllocator r u n) =
    if Set.member (toBit r i) u
    then let u' = Set.delete (toBit r i) u
         in return (SetAllocator r u' n)
    else failure InvalidId

_statistics :: (Integral i) => SetAllocator i -> Statistics
_statistics (SetAllocator r u _) =
    let k = fromIntegral (Range.size r)
        n = Set.size u
    in Statistics {
        numAvailable = k
      , numFree = k - n
      , numUsed = n }

instance (Integral i) => IdAllocator (SetAllocator i) where
    type Id (SetAllocator i) = i
    alloc                    = _alloc
    free                     = _free
    statistics               = _statistics
