{-# LANGUAGE
    DeriveDataTypeable
  , FlexibleContexts
  , TypeFamilies #-}

module Sound.SC3.Server.Allocator
    ( AllocFailure(..)
    , IdAllocator(..)
    , RangeAllocator(..)
    , module Sound.SC3.Server.Allocator.Range
    ) where

import Control.Exception (Exception)
import Control.Failure (Failure)
import Control.Monad (replicateM)
import qualified Control.Monad.Trans.Class as State
import qualified Control.Monad.Trans.State.Strict as State
import Data.Typeable (Typeable)
import Sound.SC3.Server.Allocator.Range

data AllocFailure =
    NoFreeIds
  | InvalidId
  deriving (Show, Typeable)

instance Exception AllocFailure

class IdAllocator a where
    type Id a
    alloc :: Failure AllocFailure m => a -> m (Id a, a)
    free  :: Failure AllocFailure m => Id a -> a -> m a
    
    allocMany :: Failure AllocFailure m => Int -> a -> m ([Id a], a)
    allocMany n = State.runStateT (replicateM n (modifyM alloc))
        where
            modifyM f = do
                (a, s') <- State.get >>= State.lift . f
                State.put $! s'
                return a

class IdAllocator a => RangeAllocator a where
    allocRange :: Failure AllocFailure m => Int -> a -> m (Range (Id a), a)
    freeRange  :: Failure AllocFailure m => Range (Id a) -> a -> m a
