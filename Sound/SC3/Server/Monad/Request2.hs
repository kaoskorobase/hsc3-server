{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Sound.SC3.Server.Monad.Request2 (
  RequestT
, exec
, exec_
, Value
, value
, AllocT
, after
, after_
, finally
, mkAsync
, mkAsync_
, mkSync
) where

import           Control.Applicative (Applicative)
import           Control.Arrow (second)
import           Control.Monad (liftM)
import           Control.Monad.IO.Class (MonadIO(..))
import qualified Control.Monad.Trans.Class as Trans
import qualified Control.Monad.Trans.State as State
import           Data.IORef
import qualified Sound.SC3.Server.Command as C
import           Sound.SC3.Server.Monad (MonadIdAllocator, MonadRecvOSC, MonadSendOSC, MonadServer)
import qualified Sound.SC3.Server.Monad as M
import           Sound.SC3.Server.Notification (Notification)
import qualified Sound.SC3.Server.Notification as N
import           Sound.OpenSoundControl (OSC(..), Time, immediately)

data Request =
    Sync OSC
  | Async (Maybe OSC -> OSC)

compile :: Time -> [Request] -> OSC
compile t rs = go t rs []
  where
    go t [] ps = Bundle t ps
    go t (r:rs) ps =
      case r of
        Sync osc -> go t rs (osc : ps)
        Async f  -> case ps of
                      [] -> let ps' = [f Nothing]
                            in go t rs ps'
                      _  -> let ps' = [f (Just (Bundle t ps))]
                            in go immediately rs ps'

-- | Internal state used for constructing bundles from 'RequestT' actions.
data State m = State {
    requests      :: [Request]                     -- ^ Current list of OSC messages.
  , notifications :: [Notification (m ())] -- ^ Current list of notifications to synchronise on.
  , cleanup       :: m ()                  -- ^ Cleanup action to deallocate resources.
  , needsSync     :: Bool
  --, timeTag       :: Time                          -- ^ Time tag.
  --, syncState     :: SyncState                     -- ^ Synchronisation barrier state.
  }

-- | Representation of a server-side action (or sequence of actions).
newtype RequestT m a = RequestT (State.StateT (State m) m a)
                        deriving (Applicative, Functor, Monad)

instance MonadServer m => MonadServer (RequestT m) where
    serverOptions = lift M.serverOptions

instance MonadIdAllocator m => MonadIdAllocator (RequestT m) where
    rootNodeId = lift M.rootNodeId
    alloc = lift . M.alloc
    free a = lift . M.free a
    allocMany a = lift . M.allocMany a
    freeMany a = lift . M.freeMany a
    allocRange a = lift . M.allocRange a
    freeRange a = lift . M.freeRange a

-- | Bundles are flattened into the resulting bundle because @scsynth@ doesn't
-- support nested bundles.
instance Monad m => MonadSendOSC (RequestT m) where
    send osc@(Message _ _) = modify $ \s -> s { requests = Sync osc : requests s }
    send (Bundle _ xs)     = mapM_ M.send xs

-- | Lift a ServerT action into RequestT.
--
-- This is potentially unsafe and should only be used for the allocation of
-- server resources. Lifting actions that rely on communication and
-- synchronisation primitives will not work as expected.
lift :: Monad m => m a -> RequestT m a
lift = RequestT . Trans.lift

-- | Get a value from the state.
gets :: Monad m => (State m -> a) -> RequestT m a
gets = RequestT . State.gets

-- | Modify the state in a RequestT action.
modify :: Monad m => (State m -> State m) -> RequestT m ()
modify = RequestT . State.modify

--newtype AsyncT m a = AsyncT (RequestT m (Maybe OSC -> OSC, a))

-- | Allocation action newtype wrapper.
newtype AllocT m a = AllocT (m a)
                     deriving (Applicative, Functor, MonadIdAllocator, Monad)

-- | Representation of a deferred server resource.
--
-- Resource resource values can only be observed with 'extract' after the
-- surrounding 'RequestT' action has been executed with 'exec'.
newtype Value a = Value (IO a)
                  deriving (Applicative, Functor, Monad)

value :: MonadIO m => Value a -> m a
value (Value extract) = liftIO extract

-- | Register a cleanup action that is executed after the notification has been
-- received and return the deferred notification result.
after :: MonadIO m => Notification a -> AllocT m () -> RequestT m (Value a)
after n (AllocT m) = do
    v <- lift $ liftIO $ newIORef (error "BUG: after: uninitialized IORef")
    modify $ \s -> s { notifications = fmap (liftIO . writeIORef v) n : notifications s
                     , cleanup = cleanup s >> m }
    return $ Value (readIORef v)

-- | Register a cleanup action, to be executed after a notification has been
-- received and ignore the notification result.
after_ :: Monad m => Notification a -> AllocT m () -> RequestT m ()
after_ n (AllocT m) =
    modify $ \s -> s { notifications = fmap (const (return ())) n : notifications s
                     , cleanup = cleanup s >> m }

-- | Register a cleanup action that is executed after all asynchronous commands
-- and notifications have been performed.
finally :: Monad m => AllocT m () -> RequestT m ()
finally (AllocT m) = modify $ \s -> s { cleanup = cleanup s >> m }

---- | Execute an asynchronous command, discarding the result.
--sync_ :: MonadIO m => AsyncT m a -> RequestT m ()
--sync_ (AsyncT m) = do
--  (f, _) <- m
--  msg <- mkSync
--  modify $ \s -> s { requests = Sync (f (Just msg)) : requests s }
--  return ()

---- | Execute an asynchronous command and return the result.
--sync :: Monad m => AsyncT m a -> RequestT m a
--sync (AsyncT m) = do
--  (f, a) <- m
--  modify $ \s -> s { requests = Async f : requests s
--                   , needsSync = True }
--  return a

-- | Create an asynchronous command from an allocation action.
--
-- The first return value should be a server resource allocated on the client,
-- the second a function that, given a completion packet, returns an OSC packet
-- that asynchronously allocates the resource on the server.
mkAsync :: Monad m => AllocT m (a, (Maybe OSC -> OSC)) -> RequestT m a
mkAsync (AllocT m) = do
  (a, f) <- lift m
  modify $ \s -> s { requests = Async f : requests s
                   , needsSync = True }
  return a

-- | Create an asynchronous command from an OSC function that has side effects only on the server.
mkAsync_ :: Monad m => (Maybe OSC -> OSC) -> RequestT m ()
mkAsync_ f = mkAsync $ return ((), f)

---- | Create an asynchronous command.
----
---- The completion message will be appended at the end of the returned message.
--mkAsyncCM :: Monad m => AllocT m (a, OSC) -> RequestT m a
--mkAsyncCM = mkAsync . liftM (second f)
--    where
--        f msg Nothing   = msg
--        f msg (Just cm) = C.withCM msg cm

-- | Create a synchronisation barrier message.
mkSync :: MonadIdAllocator m => RequestT m OSC
mkSync = do
  sid <- lift $ M.alloc M.syncIdAllocator
  after_ (N.synced sid) (M.free M.syncIdAllocator sid)
  return $ C.sync (fromIntegral sid)

-- | Execute a request.
--
-- The commands after the last asynchronous command will be schedule at the given time.
exec :: (MonadIdAllocator m, MonadRecvOSC m) => Time -> RequestT m a -> m a
exec t r = do
  let RequestT m = do
        b <- gets needsSync
        if b
          then do
            a <- r
            mkSync >>= M.send
            return a
          else r
  (a, s) <- State.runStateT m (State [] [] (return ()) False)
  case requests s of
    [] -> return ()
    rs -> let osc = compile t rs
              ns = notifications s
          in M.waitForAll osc ns >>= sequence_
  cleanup s
  return a

-- | Execute a request immediately.
exec_ :: (MonadIdAllocator m, MonadRecvOSC m) => RequestT m a -> m a
exec_ = exec immediately
