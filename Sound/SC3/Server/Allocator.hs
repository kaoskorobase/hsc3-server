{-# LANGUAGE
    DeriveDataTypeable
  , FlexibleContexts
  , TypeFamilies #-}

module Sound.SC3.Server.Allocator
    ( -- *Allocation errors
      AllocFailure(..)
      -- * Allocator statistics
    , Statistics(..)
    , percentFree
    , percentUsed
      -- * Allocator classes
    , IdAllocator(..)
    , RangeAllocator(..)
      -- * Identifier ranges
    , module Sound.SC3.Server.Allocator.Range
    ) where

import           Control.Exception (Exception)
import           Control.Failure (Failure)
import           Control.Monad (foldM, replicateM)
import qualified Control.Monad.Trans.Class as State
import qualified Control.Monad.Trans.State.Strict as State
import           Data.Typeable (Typeable)
import           Sound.SC3.Server.Allocator.Range

-- | Failure type for allocators.
data AllocFailure =
    NoFreeIds   -- ^ There are no free ids left in the allocator.
  | InvalidId   -- ^ The id being released has not been allocated by this allocator.
  deriving (Show, Typeable)

instance Exception AllocFailure

-- | Simple allocator usage statistics.
data Statistics = Statistics {
    numAvailable :: Int     -- ^ Total number of available identifiers
  , numFree :: Int          -- ^ Number of currently available identifiers
  , numUsed :: Int          -- ^ Number of identifiers currently in use
  } deriving (Eq, Show)

-- | Percentage of currently available identifiers.
--
-- > percentFree s = numFree s / numAvailable s
-- > percentFree s + percentUsed s = 1
percentFree :: Statistics -> Double
percentFree s = fromIntegral (numFree s) / fromIntegral (numAvailable s)

-- | Percentage of identifiers currently in use.
--
-- > percentUsed s = numUsed s / numAvailable s
-- > percentUsed s + percentFree s = 1
percentUsed :: Statistics -> Double
percentUsed s = fromIntegral (numUsed s) / fromIntegral (numAvailable s)

-- | IdAllocator provides an interface for allocating and releasing
-- identifiers that correspond to server resources, such as node, buffer and
-- bus ids.
class IdAllocator a where
    type Id a
    -- | Allocate a new identifier and return the changed allocator.
    alloc :: Failure AllocFailure m => a -> m (Id a, a)

    -- | Free a previously allocated identifier and return the changed allocator.
    --
    -- Freeing an identifier that hasn't been allocated with this allocator may trigger a failure.
    free  :: Failure AllocFailure m => Id a -> a -> m a

    -- | Allocate a number of - not necessarily consecutive - identifiers and return the changed allocator.
    allocMany :: Failure AllocFailure m => Int -> a -> m ([Id a], a)
    allocMany n = State.runStateT (replicateM n (modifyM alloc))
        where
            modifyM f = do
                (a, s') <- State.get >>= State.lift . f
                State.put $! s'
                return a

    -- | Free a list of previously allocated identifiers.
    freeMany :: Failure AllocFailure m => [Id a] -> a -> m a
    freeMany = flip (foldM (flip free))

    -- | Return usage statistics.
    statistics :: a -> Statistics

-- | RangeAllocator provides an interface for allocating and releasing ranges
-- of consecutive identifiers.
class IdAllocator a => RangeAllocator a where
    -- | Allocate n consecutive identifiers and return the changed allocator.
    allocRange :: Failure AllocFailure m => Int -> a -> m (Range (Id a), a)
    -- | Free a range of previously allocated identifiers and return the changed allocator.
    freeRange  :: Failure AllocFailure m => Range (Id a) -> a -> m a
