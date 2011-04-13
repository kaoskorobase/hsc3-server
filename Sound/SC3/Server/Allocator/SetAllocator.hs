{-# LANGUAGE BangPatterns
           , FlexibleContexts
           , TypeFamilies #-}
module Sound.SC3.Server.Allocator.SetAllocator (
    SetAllocator
  , cons
) where

import Control.Failure (Failure, failure)
import Control.DeepSeq (NFData(..))
import qualified Data.BitSet as Set
import Sound.SC3.Server.Allocator

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
cons r = SetAllocator r Set.empty (lowerBound r)

-- | Convert an id to a bit index.
--
-- This is necessary to keep the BitSet size bounded between [0, numIds[.
toBit :: Integral i => Range i -> i -> i
toBit r i = i - lowerBound r

findNext :: (Integral i) => SetAllocator i -> Maybe i
findNext (SetAllocator r u n)
    | fromIntegral (size r) == Set.size u = Nothing
    | otherwise = loop n
    where
        wrap i = if i >= upperBound r
                    then lowerBound r
                    else i
        loop !i = let i' = wrap (i+1)
                  in if Set.member (toBit r i') u
                     then loop i'
                     else Just i'

_alloc :: (Integral i, Failure AllocFailure m) => SetAllocator i -> m (i, SetAllocator i)
_alloc a@(SetAllocator r u n) =
    case findNext a of
        Nothing -> failure NoFreeIds
        Just n' -> return (n, SetAllocator r (Set.insert (toBit r n) u) n')

_free :: (Integral i, Failure AllocFailure m) => i -> SetAllocator i -> m (SetAllocator i)
_free i (SetAllocator r u n) =
    if Set.member i u
    then let u' = Set.delete (toBit r i) u
         in return (SetAllocator r u' n)
    else failure InvalidId

_statistics :: (Integral i) => SetAllocator i -> Statistics
_statistics (SetAllocator r u _) =
    let k = fromIntegral (size r)
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
