{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Sound.SC3.Server.Monad.Request2 where

import           Control.Applicative (Applicative)
import           Control.Monad.IO.Class (MonadIO(..))
import qualified Control.Monad.Trans.Class as Trans
import qualified Control.Monad.Trans.State as State
import           Data.IORef
import qualified Sound.SC3.Server.Command as C
import           Sound.SC3.Server.Monad (MonadIdAllocator, MonadSendOSC(..), MonadServer, ServerT)
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
  , notifications :: [Notification (ServerT m ())] -- ^ Current list of notifications to synchronise on.
  , cleanup       :: ServerT m ()                  -- ^ Cleanup action to deallocate resources.
  --, timeTag       :: Time                          -- ^ Time tag.
  --, syncState     :: SyncState                     -- ^ Synchronisation barrier state.
  }

-- | Representation of a server-side action (or sequence of actions).
newtype RequestT m a = RequestT (State.StateT (State m) (ServerT m) a)
                        deriving (Applicative, Functor, Monad)

-- | Lift a ServerT action into RequestT.
--
-- This is potentially unsafe and should only be used for the allocation of
-- server resources. Lifting actions that rely on communication and
-- synchronisation primitives will not work as expected.
lift :: Monad m => ServerT m a -> RequestT m a
lift = RequestT . Trans.lift

-- | Modify the state in a RequestT action.
modify :: Monad m => (State m -> State m) -> RequestT m ()
modify = RequestT . State.modify

newtype AsyncT m a = AsyncT (RequestT m (Maybe OSC -> OSC, a))

-- | Allocation action newtype wrapper.
newtype AllocT m a = AllocT (ServerT m a)
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

exec :: Time -> RequestT m a -> ServerT m a
exec = undefined

-- | Add a synchronisation barrier.
mkSync :: MonadIO m => RequestT m OSC
mkSync = do
  sid <- lift $ M.alloc M.syncIdAllocator
  after_ (N.synced sid) (M.free M.syncIdAllocator sid)
  return $ C.sync (fromIntegral sid)

sync_ :: MonadIO m => AsyncT m a -> RequestT m ()
sync_ (AsyncT m) = do
  (f, _) <- m
  msg <- mkSync
  modify $ \s -> s { requests = Sync (f (Just msg)) : requests s }
  return ()

sync :: Monad m => AsyncT m a -> RequestT m a
sync (AsyncT m) = do
  (f, a) <- m
  modify $ \s -> s { requests = Async f : requests s }
  return a
