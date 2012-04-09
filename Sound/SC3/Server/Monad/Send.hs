{-# LANGUAGE GeneralizedNewtypeDeriving
           , MultiParamTypeClasses #-}

-- | This module provides abstractions for constructing bundles for server
-- resource allocation in a type safe manner. In particular, the exposed types
-- and functions make sure that asynchronous command results cannot be used
-- before they have been allocated on the server.
--
-- TODO: Real usage example
--
-- > (b0, (g, ig, b)) <- immediately !> do
-- > b0 <- async $ b_alloc 1024 1
-- > x <- b_alloc 1024 1 `whenDone` immediately $ \b -> do
-- >     b_free b `whenDone` OSC.UTCr t' $ \() -> do
-- >         g <- g_new_ AddToTail
-- >         ig <- g_new AddToTail g
-- >         return $ pure (g, ig, b)
-- > return $ (,) <$> b0 <*> x
module Sound.SC3.Server.Monad.Send
  ( SendT
  , AllocT
  -- * Deferred values
  , Deferred
  , after
  , after_
  , finally
  -- * Asynchronous commands
  , Async
  , module Control.Applicative
  , mkAsync
  , mkAsync_
  , mkAsyncCM
  , whenDone
  , asyncM
  , async
  -- * Command execution
  , run
  , exec
  , (!>)
  , execPure
  , (~>)
  ) where

import           Control.Applicative
import           Control.Arrow (second)
import           Control.Monad (ap, liftM, when)
import           Control.Monad.IO.Class (MonadIO(..))
import qualified Control.Monad.Trans.Class as Trans
import           Control.Monad.Trans.State (StateT(..))
import qualified Control.Monad.Trans.State as State
import           Data.IORef
import           Sound.SC3.Server.Monad (MonadIdAllocator, MonadSendOSC(..), MonadServer, ServerT)
import qualified Sound.SC3.Server.Monad as M
import qualified Sound.SC3.Server.State as State
import qualified Sound.SC3.Server.Command as C
import           Sound.SC3.Server.Notification (Notification)
import qualified Sound.SC3.Server.Notification as N
import           Sound.OpenSoundControl (OSC(..), Time, immediately)

{-

goals:

* after executing action and synchronizing, all server actions have been executed
* server actions are consistent, i.e. asynchronous resources are not used before they are allocated (Deferred)

async sets sync state to "needs sync"
whenDone overrides sync state to "has sync"
whenDone adds a sync barrier to the completion packet when its subaction didn't add one (syncIds empty); the subaction always needs to sync!
exec adds a sync barrier when sync state is "needs sync"
-}

-- | Synchronisation state.
data SyncState =
    NoSync      -- ^ No synchronisation barrier needed.
  | NeedsSync   -- ^ Need to add a synchronisation barrier to the current context.
  | HasSync     -- ^ Synchronisation barrier already present in the current context.
  deriving (Eq, Ord, Show)

-- | Internal state used for constructing bundles from 'SendT' actions.
data State m = State {
    buildOSC      :: [OSC]                         -- ^ Current list of OSC messages.
  , notifications :: [Notification (ServerT m ())] -- ^ Current list of notifications to synchronise on.
  , cleanup       :: ServerT m ()                  -- ^ Cleanup action to deallocate resources.
  , timeTag       :: Time                          -- ^ Time tag.
  , syncState     :: SyncState                     -- ^ Synchronisation barrier state.
  }

-- | Construct a 'SendT' state with a given synchronisation state.
mkState :: Monad m => Time -> SyncState -> State m
mkState = State [] [] (return ())

-- | Push an OSC packet.
pushOSC :: OSC -> State m -> State m
pushOSC osc s = s { buildOSC = osc : buildOSC s }

-- | Return 'True' if the current context contains OSC messages.
hasOSC :: State m -> Bool
hasOSC = not . null . buildOSC

-- | Get the list of OSC packets.
getOSC :: State m -> [OSC]
getOSC = reverse . buildOSC

-- | Update the synchronisation state.
setSyncState :: SyncState -> State m -> State m
setSyncState ss s | ss > syncState s = s { syncState = ss }
                  | otherwise        = s

-- | Representation of a server-side action (or sequence of actions).
newtype SendT m a = SendT (StateT (State m) (ServerT m) a)
                    deriving (Applicative, Functor, Monad)

instance MonadIO m => MonadServer (SendT m) where
    serverOptions = liftServer M.serverOptions

instance MonadIO m => MonadIdAllocator (SendT m) where
    rootNodeId = liftServer M.rootNodeId
    alloc = liftServer . M.alloc
    free a = liftServer . M.free a
    allocMany a = liftServer . M.allocMany a
    freeMany a = liftServer . M.freeMany a
    allocRange a = liftServer . M.allocRange a
    freeRange a = liftServer . M.freeRange a

-- | Bundles are flattened into the resulting bundle because @scsynth@ doesn't
-- support nested bundles.
instance Monad m => MonadSendOSC (SendT m) where
    send osc@(Message _ _) = modify (pushOSC osc)
    send (Bundle _ xs)     = mapM_ send xs

-- | Execute a SendT action, returning the result and the final state.
runSendT :: Monad m => Time -> SyncState -> SendT m a -> ServerT m (a, State m)
runSendT t s (SendT m) = State.runStateT m (mkState t s)

-- | Get a value from the state.
gets :: Monad m => (State m -> a) -> SendT m a
gets = SendT . State.gets

-- | Modify the state in a SendT action.
modify :: Monad m => (State m -> State m) -> SendT m ()
modify = SendT . State.modify

-- | Lift a ServerT action into SendT.
--
-- This is potentially unsafe and should only be used for the allocation of
-- server resources. Lifting actions that rely on communication and
-- synchronisation primitives will not work as expected.
liftServer :: Monad m => ServerT m a -> SendT m a
liftServer = SendT . Trans.lift

-- | Allocation action newtype wrapper.
newtype AllocT m a = AllocT (ServerT m a)
                     deriving (Applicative, MonadIdAllocator, Functor, Monad)

-- | Representation of a deferred server resource.
--
-- Deferred resource values can only be observed a return value of the 'SendT'
-- action after 'exec' has been called.
--
-- Deferred has 'Applicative' and 'Functor' instances, so that complex values
-- can be built from simple ones.
newtype Deferred m a = Deferred { unDefer :: ServerT m a } deriving (Monad)

instance Monad m => Functor (Deferred m) where
    fmap f (Deferred a) = Deferred (liftM f a)

instance Monad m => Applicative (Deferred m) where
    pure = Deferred . return
    (<*>) (Deferred f) (Deferred a) = Deferred (f `ap` a)

-- | Construct a deferred value from an IO action.
deferredIO :: MonadIO m => IO a -> Deferred m a
deferredIO = Deferred . liftIO

-- | Register a cleanup action, to be executed after a notification has been
-- received and return the deferred notification result.
after :: MonadIO m => Notification a -> AllocT m () -> SendT m (Deferred m a)
after n (AllocT m) = do
    v <- liftServer $ liftIO $ newIORef (error "BUG: after: uninitialized IORef")
    modify $ \s -> s { notifications = fmap (liftIO . writeIORef v) n : notifications s
                     , cleanup = cleanup s >> m }
    return $ deferredIO (readIORef v)

-- | Register a cleanup action, to be executed after a notification has been
-- received and ignore the notification result.
after_ :: Monad m => Notification a -> AllocT m () -> SendT m ()
after_ n (AllocT m) = modify $ \s -> s { notifications = fmap (const (return ())) n : notifications s
                                       , cleanup = cleanup s >> m }

-- | Register a cleanup action, to be executed after all asynchronous commands
-- and notification have finished.
finally :: Monad m => AllocT m () -> SendT m ()
finally (AllocT m) = modify $ \s -> s { cleanup = cleanup s >> m }

-- | Representation of an asynchronous server command. Asynchronous commands
-- are executed asynchronously with respect to other server commands.
--
-- There are two different ways of synchronising with an asynchronous command:
--
-- * using 'whenDone' for server-side synchronisation, or
--
-- * using 'async' and observing the result of a 'SendT' action after calling
-- 'exec'.
newtype Async m a = Async (SendT m (a, (Maybe OSC -> OSC)))

-- | Create an asynchronous command from an allocation action.
--
-- The first return value should be a server resource allocated on the client,
-- the second a function that, given a completion packet, returns an OSC packet
-- that asynchronously allocates the resource on the server.
mkAsync :: Monad m => AllocT m (a, (Maybe OSC -> OSC)) -> Async m a
mkAsync (AllocT m) = Async (liftServer m)

-- | Create an asynchronous command from a side effecting OSC function.
mkAsync_ :: Monad m => (Maybe OSC -> OSC) -> Async m ()
mkAsync_ f = mkAsync $ return ((), f)

-- | Create an asynchronous command.
--
-- The completion message will be appended at the end of the returned message.
mkAsyncCM :: Monad m => AllocT m (a, OSC) -> Async m a
mkAsyncCM = mkAsync . liftM (second f)
    where
        f msg Nothing   = msg
        f msg (Just cm) = C.withCM msg cm

-- | Add a synchronisation barrier.
addSync :: MonadIO m => SendT m a -> SendT m a
addSync m = do
    a <- m
    b <- gets hasOSC
    when b $ do
        s <- gets syncState
        case s of
            NeedsSync -> do
                sid <- liftServer $ M.alloc State.syncIdAllocator
                send (C.sync (fromIntegral sid))
                after_ (N.synced sid) (M.free State.syncIdAllocator sid)
            _ -> return ()
    return a

-- | Execute an server-side action after the asynchronous command has
-- finished.
whenDone :: MonadIO m => Async m a -> (a -> SendT m b) -> Async m b
whenDone (Async m) f = Async $ do
    (a, g) <- m
    b <- f a
    return (b, g)

-- | Execute an asynchronous command asynchronously.
asyncM :: MonadIO m => Async m (Deferred m a) -> SendT m (Deferred m a)
asyncM (Async m) = do
    t <- gets timeTag
    ((a, g), s) <- liftServer $ runSendT t NeedsSync $ addSync m
    case getOSC s of
        [] -> do
            send (g Nothing)
            modify $ \s' -> (setSyncState NeedsSync s') {
                notifications = notifications s' ++ notifications s
              , cleanup = cleanup s' >> cleanup s }
        osc -> do
            let t' = case syncState s of
                        HasSync -> immediately
                        _       -> t
            send $ g (Just (Bundle t' osc))
            modify $ \s' -> (setSyncState HasSync s') {
                notifications = notifications s' ++ notifications s
              , cleanup = cleanup s' >> cleanup s }
    return a

-- | Execute an asynchronous command asynchronously.
async :: MonadIO m => Async m a -> SendT m (Deferred m a)
async = asyncM . flip whenDone (return . pure)

{-
-- | Execute an server-side action after the asynchronous command has
-- finished. The corresponding server commands are scheduled at a time @t@ in
-- the future.
whenDone :: MonadIO m => Async m a -> (a -> SendT m b) -> SendT m (Deferred b)
whenDone (Async m) f = do
    t <- gets timeTag
    (a, g) <- m
    (b, s) <- liftServer $ runSendT t NeedsSync $ addSync (f a)
    let t' = case syncState s of
                HasSync -> immediately
                _       -> t
    send $ g (Just (Bundle t' (getOSC s)))
    modify $ \s' -> s' {
        notifications = notifications s' ++ notifications s
      , cleanup = cleanup s' >> cleanup s
      , syncState = HasSync }
    return b

-- | Execute an asynchronous command asynchronously.
async :: MonadIO m => Async m a -> SendT m (Deferred a)
async (Async m) = do
    (a, g) <- m
    send (g Nothing)
    modify $ setSyncState NeedsSync
    return $ pure a
-}

run :: MonadIO m => Time -> SendT m (Deferred m a) -> ServerT m (ServerT m a, Maybe (OSC, [Notification (ServerT m ())]))
run t m = do
    (a, s) <- runSendT t NoSync $ addSync m
    let result = cleanup s >> unDefer a
    case getOSC s of
        [] -> return (result, Nothing)
        osc -> let t' = case syncState s of
                            HasSync -> immediately
                            _ -> t
               in return (result, Just (Bundle t' osc, notifications s))

-- | Run the 'SendT' action and return the result.
--
-- All asynchronous commands and notifications are guaranteed to have finished
-- when this function returns.
exec :: MonadIO m => Time -> SendT m (Deferred m a) -> ServerT m a
exec t m = do
    -- (a, s) <- runSendT t NoSync $ addSync m
    -- case getOSC s of
    --     [] -> return ()
    --     osc -> do
    --         -- liftIO $ print osc
    --         let t' = case syncState s of
    --                     HasSync -> immediately
    --                     _ -> t
    (action, sync) <- run t m
    case sync of
        Nothing -> return ()
        Just (osc, ns) -> M.waitForAll osc ns >>= sequence_
    action

-- | Infix operator version of 'exec'.
(!>) :: MonadIO m => Time -> SendT m (Deferred m a) -> ServerT m a
(!>) = exec

-- | Run a 'SendT' action that returns a pure result.
--
-- All asynchronous commands and notifications are guaranteed to have finished
-- when this function returns.
execPure :: MonadIO m => Time -> SendT m a -> ServerT m a
execPure t m = exec t (m >>= return . pure)

-- | Infix operator version of 'execPure'.
(~>) :: MonadIO m => Time -> SendT m a -> ServerT m a
(~>) = execPure
