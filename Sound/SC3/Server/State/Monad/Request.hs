{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies #-}
module Sound.SC3.Server.State.Monad.Request (
  Request
, runRequest
, exec
, exec_
, Result
, extract
, AllocT
, after
, after_
, waitFor
, finally
, mkAsync
, mkAsync_
, mkSync
) where

import           Control.Applicative (Applicative)
import           Control.Monad.IO.Class (MonadIO(..))
import qualified Control.Monad.Trans.Class as Trans
import qualified Control.Monad.Trans.State as State
import           Data.IORef (newIORef, readIORef, writeIORef)
import qualified Sound.SC3.Server.Command as C
import           Sound.SC3.Server.Notification (Notification)
import qualified Sound.SC3.Server.Notification as N
import           Sound.SC3.Server.State.Monad.Class (MonadIdAllocator(..), MonadRecvOSC, MonadSendOSC, MonadServer)
import qualified Sound.SC3.Server.State.Monad.Class as M
import           Sound.OpenSoundControl (Bundle(..), Message(..), OSC(..), Time, immediately, packetMessages)

data Builder =
    BuildDone
  | BuildSync Message Builder
  | BuildAsync (Maybe Bundle -> Message) Builder

compile :: Time -> Builder -> Bundle
compile t rs = go t rs []
  where
    go t BuildDone ps = Bundle t ps
    go t (BuildSync osc rs') ps = go t rs' (osc : ps)
    go t (BuildAsync f rs') ps =
      case ps of
        [] -> let ps' = [f Nothing]
              in go t rs' ps'
        _  -> let ps' = [f (Just (Bundle t ps))]
              in go immediately rs' ps'

-- | Internal state used for constructing bundles from 'Request' actions.
data State m = State {
    requests      :: Builder                -- ^ Current list of OSC messages.
  , notifications :: [Notification (m ())]  -- ^ Current list of notifications to synchronise on.
  , cleanup       :: m ()                   -- ^ Cleanup action to deallocate resources.
  , needsSync     :: Bool                   -- ^ Whether last bundle needs a synchronisation barrier.
  }

-- | The empty state.
emptyState :: Monad m => State m
emptyState = State BuildDone [] (return ()) False

-- | Server-side action (or sequence of actions).
newtype Request m a = Request (State.StateT (State m) m a)
                        deriving (Applicative, Functor, Monad)

-- | Lift a ServerT action into Request.
--
-- This is potentially unsafe and should only be used for the allocation of
-- server resources. Lifting actions that rely on communication and
-- synchronisation primitives will not work as expected.
lift :: Monad m => m a -> Request m a
lift = Request . Trans.lift

-- | Get a value from the state.
gets :: Monad m => (State m -> a) -> Request m a
gets = Request . State.gets

-- | Modify the state in a Request action.
modify :: Monad m => (State m -> State m) -> Request m ()
modify = Request . State.modify

instance MonadServer m => MonadServer (Request m) where
  serverOptions = lift M.serverOptions
  rootNodeId = lift M.rootNodeId

instance MonadIdAllocator m => MonadIdAllocator (Request m) where
  newtype Allocator (Request m) a = Request_Allocator (Allocator m a)

  nodeIdAllocator       = Request_Allocator nodeIdAllocator
  syncIdAllocator       = Request_Allocator syncIdAllocator
  bufferIdAllocator     = Request_Allocator bufferIdAllocator
  audioBusIdAllocator   = Request_Allocator audioBusIdAllocator
  controlBusIdAllocator = Request_Allocator controlBusIdAllocator

  alloc (Request_Allocator a)      = lift $ M.alloc a
  free (Request_Allocator a)       = lift . M.free a
  statistics (Request_Allocator a) = lift $ M.statistics a
  allocRange (Request_Allocator a) = lift . M.allocRange a
  freeRange (Request_Allocator a)  = lift . M.freeRange a

-- | Bundles are flattened into the resulting bundle because @scsynth@ doesn't
-- support nested bundles.
instance Monad m => MonadSendOSC (Request m) where
  send osc = modify $ \s ->
              s { requests = build
                              (requests s)
                              (packetMessages (toPacket osc)) }
    where build bs [] = bs
          build bs (a:as) = build (BuildSync a bs) as

-- | Allocation action newtype wrapper.
newtype AllocT m a = AllocT (m a)
                     deriving (Applicative, Functor, Monad)

instance MonadIdAllocator m => MonadIdAllocator (AllocT m) where
  newtype Allocator (AllocT m) a = AllocT_Allocator (Allocator m a)

  nodeIdAllocator       = AllocT_Allocator nodeIdAllocator
  syncIdAllocator       = AllocT_Allocator syncIdAllocator
  bufferIdAllocator     = AllocT_Allocator bufferIdAllocator
  audioBusIdAllocator   = AllocT_Allocator audioBusIdAllocator
  controlBusIdAllocator = AllocT_Allocator controlBusIdAllocator

  alloc (AllocT_Allocator a)      = AllocT $ M.alloc a
  free (AllocT_Allocator a)       = AllocT . M.free a
  statistics (AllocT_Allocator a) = AllocT $ M.statistics a
  allocRange (AllocT_Allocator a) = AllocT . M.allocRange a
  freeRange (AllocT_Allocator a)  = AllocT . M.freeRange a

-- | Representation of a deferred server resource.
--
-- Resource resource values can only be observed with 'extract' after the
-- surrounding 'Request' action has been executed with 'exec'.
newtype Result a = Result (IO a)
                   deriving (Applicative, Functor, Monad)

-- | Extract a 'Result'\'s value.
extract :: MonadIO m => Result a -> m a
extract (Result a) = liftIO a

-- | Register a cleanup action that is executed after the notification has been
-- received and return the notification result.
after :: MonadIO m => Notification a -> AllocT m () -> Request m (Result a)
after n (AllocT m) = do
  v <- lift $ liftIO $ newIORef (error "BUG: after: uninitialized IORef")
  modify $ \s -> s { notifications = fmap (liftIO . writeIORef v) n : notifications s
                   , cleanup = cleanup s >> m }
  return $ Result (readIORef v)

-- | Register a cleanup action, to be executed after a notification has been
-- received and ignore the notification result.
after_ :: Monad m => Notification a -> AllocT m () -> Request m ()
after_ n (AllocT m) =
  modify $ \s -> s { notifications = fmap (const (return ())) n : notifications s
                   , cleanup = cleanup s >> m }

-- | Wait for a notification and return the result.
waitFor :: MonadIO m => Notification a -> Request m (Result a)
waitFor n = after n (return ())

-- | Register a cleanup action that is executed after all asynchronous commands
-- and notifications have been performed.
finally :: Monad m => AllocT m () -> Request m ()
finally (AllocT m) = modify $ \s -> s { cleanup = cleanup s >> m }

-- | Create an asynchronous command from an allocation action.
--
-- The first return value should be a server resource allocated on the client,
-- the second a function that, given a completion packet, returns an OSC packet
-- that asynchronously allocates the resource on the server.
mkAsync :: Monad m => AllocT m (a, (Maybe Bundle -> Message)) -> Request m a
mkAsync (AllocT m) = do
  (a, f) <- lift m
  modify $ \s -> s { requests = BuildAsync f (requests s)
                   , needsSync = True }
  return a

-- | Create an asynchronous command from an OSC function that has side effects
--   only on the server.
mkAsync_ :: Monad m => (Maybe Bundle -> Message) -> Request m ()
mkAsync_ f = mkAsync $ return ((), f)

-- | Create a synchronisation barrier message.
mkSync :: MonadIdAllocator m => Request m Message
mkSync = do
  sid <- lift $ M.alloc M.syncIdAllocator
  after_ (N.synced sid) (M.free M.syncIdAllocator sid)
  return $ C.sync (fromIntegral sid)

-- | Add a synchronisation barrier to a request if needed.
finish :: MonadIdAllocator m => Request m a -> Request m a
finish r = do
  b <- gets needsSync
  if b
    then do
      a <- r
      mkSync >>= M.send
      return a
    else r

-- | Run a request, returning the action's result, an OSC packet,
--   a list of notifications to synchronise on and a cleanup action.
runRequest :: (MonadIdAllocator m, MonadRecvOSC m) => Time -> Request m a -> m (a, Maybe (Bundle, [Notification (m ())]), m ())
runRequest t r = do
  let Request m = finish r
  (a, s) <- State.runStateT m emptyState
  let osc = case requests s of
              BuildDone -> Nothing
              rs -> Just (compile t rs, notifications s)
  return (a, osc, cleanup s)

-- | Execute a request.
--
-- The commands after the last asynchronous command will be scheduled at the given time.
exec :: (MonadIdAllocator m, MonadRecvOSC m) => Time -> Request m a -> m a
exec t r = do
  let Request m = finish r
  (a, s) <- State.runStateT m emptyState
  case requests s of
    BuildDone -> return ()
    rs -> let osc = compile t rs
              ns = notifications s
          in M.waitForAll osc ns >>= sequence_
  cleanup s
  return a

-- | Execute a request immediately.
exec_ :: (MonadIdAllocator m, MonadRecvOSC m) => Request m a -> m a
exec_ = exec immediately
