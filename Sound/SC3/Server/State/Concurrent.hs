{-# LANGUAGE FlexibleContexts #-}
module Sound.SC3.Server.State.Concurrent (
    fromState
  , new
  , alloc
  , allocMany
  , free
  , allocRange
  , freeRange
) where

import           Control.Arrow (second)
import           Control.Failure (Failure)
-- import           Control.Concurrent.MVar (MVar, newMVar)
import           Control.Concurrent.MVar.Strict (MVar, modifyMVar, modifyMVar_, newMVar)
import           Data.Accessor
import           Sound.SC3.Server.Allocator (AllocFailure, IdAllocator, Id, RangeAllocator, Range)
import qualified Sound.SC3.Server.Allocator as Alloc
import           Sound.SC3.Server.State (State)
import qualified Sound.SC3.Server.State as State
import           Sound.SC3.Server.Options (ServerOptions)

type IOState = MVar State

fromState :: State -> IO (MVar State)
fromState = newMVar

new :: ServerOptions -> IO (MVar State)
new = fromState . State.new

swap :: (a, b) -> (b, a)
swap (a, b) = (b, a)

alloc :: (IdAllocator a, Failure AllocFailure IO) => (Accessor State a) -> MVar State -> IO (Id a)
alloc f ios = modifyMVar ios (\s -> fmap (swap . second (flip (setVal f) s)) . Alloc.alloc . getVal f $ s)

allocMany :: (IdAllocator a, Failure AllocFailure IO)  => (Accessor State a) -> MVar State -> Int -> IO [Id a]
allocMany f m n = modifyMVar m (\s -> fmap (swap . second (flip (setVal f) s)) . Alloc.allocMany n . getVal f $ s)

free :: IdAllocator a => (Accessor State a) -> IOState -> Id a -> IO ()
free f ios i = modifyMVar_ ios $ \s -> do
    let a  = s ^. f
    a' <- Alloc.free i a
    return $ f ^= a' $ s

allocRange :: (RangeAllocator a, Failure AllocFailure IO) => (Accessor State a) -> MVar State -> Int -> IO (Range (Id a))
allocRange f ios n = modifyMVar ios (\s -> fmap (swap . second (flip (setVal f) s)) . Alloc.allocRange n . getVal f $ s)

freeRange :: (RangeAllocator a, Failure AllocFailure IO) => (Accessor State a) -> MVar State -> Range (Id a) -> IO ()
freeRange f ios r = modifyMVar_ ios $ \s -> do
    let a  = s ^. f
    a' <- Alloc.freeRange r a
    return $ f ^= a' $ s
