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
module Sound.SC3.Server.Monad.Request
  ( RequestT
  , AllocT
  -- * Deferred values
  , Deferred
  , extract
  , after
  , after_
  , finally
  -- * Asynchronous commands
  , Async
  , mkAsync
  , mkAsync_
  , mkAsyncCM
  , whenDone
  {-, asyncM-}
  , async
  -- * Command execution
  , runRequestT
  , exec
  , (!>)
  {-, execPure-}
  {-, (~>)-}
  ) where

import           Control.Applicative
import           Control.Arrow (second)
import           Control.Monad (liftM, when)
import           Control.Monad.IO.Class (MonadIO(..))
import qualified Control.Monad.Trans.Class as Trans
import           Control.Monad.Trans.State (StateT(..))
import qualified Control.Monad.Trans.State as State
import           Data.IORef
import           Sound.SC3.Server.Monad (MonadIdAllocator, MonadSendOSC(..), MonadServer, ServerT)
import qualified Sound.SC3.Server.Monad as M
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

-- | Internal state used for constructing bundles from 'RequestT' actions.
data State m = State {
    buildOSC      :: [OSC]                         -- ^ Current list of OSC messages.
  , notifications :: [Notification (ServerT m ())] -- ^ Current list of notifications to synchronise on.
  , cleanup       :: ServerT m ()                  -- ^ Cleanup action to deallocate resources.
  , timeTag       :: Time                          -- ^ Time tag.
  , syncState     :: SyncState                     -- ^ Synchronisation barrier state.
  }

-- | Construct a 'RequestT' state with a given synchronisation state.
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

-- | Representation of a server-side action (or sequence of actions).
newtype RequestT m a = RequestT (StateT (State m) (ServerT m) a)
                    deriving (Applicative, Functor, Monad)

instance Monad m => MonadServer (RequestT m) where
    serverOptions = liftServer M.serverOptions

instance MonadIO m => MonadIdAllocator (RequestT m) where
    rootNodeId = liftServer M.rootNodeId
    alloc = liftServer . M.alloc
    free a = liftServer . M.free a
    allocMany a = liftServer . M.allocMany a
    freeMany a = liftServer . M.freeMany a
    allocRange a = liftServer . M.allocRange a
    freeRange a = liftServer . M.freeRange a

-- | Bundles are flattened into the resulting bundle because @scsynth@ doesn't
-- support nested bundles.
instance Monad m => MonadSendOSC (RequestT m) where
    send osc@(Message _ _) = modify (pushOSC osc)
    send (Bundle _ xs)     = mapM_ send xs

-- | Execute a RequestT action, returning the result and the final state.
runRequestT_ :: Monad m => Time -> SyncState -> RequestT m a -> ServerT m (a, State m)
runRequestT_ t s (RequestT m) = State.runStateT m (mkState t s)

-- | Get a value from the state.
gets :: Monad m => (State m -> a) -> RequestT m a
gets = RequestT . State.gets

-- | Modify the state in a RequestT action.
modify :: Monad m => (State m -> State m) -> RequestT m ()
modify = RequestT . State.modify

-- | Lift a ServerT action into RequestT.
--
-- This is potentially unsafe and should only be used for the allocation of
-- server resources. Lifting actions that rely on communication and
-- synchronisation primitives will not work as expected.
liftServer :: Monad m => ServerT m a -> RequestT m a
liftServer = RequestT . Trans.lift

-- | Allocation action newtype wrapper.
newtype AllocT m a = AllocT (ServerT m a)
                     deriving (Applicative, Functor, MonadIdAllocator, Monad)

-- | Representation of a deferred server resource.
--
-- Deferred resource values can only be observed with 'extract' after the
-- surrounding 'RequestT' action has been executed with 'exec'.
newtype Deferred m a = Deferred { extract :: ServerT m a -- ^ Extract result from deferred resource.
                                }
                       deriving (Applicative, Functor, Monad)

-- | Register a cleanup action that is executed after the notification has been
-- received and return the deferred notification result.
after :: MonadIO m => Notification a -> AllocT m () -> RequestT m (Deferred m a)
after n (AllocT m) = do
    v <- liftServer $ liftIO $ newIORef (error "BUG: after: uninitialized IORef")
    modify $ \s -> s { notifications = fmap (liftIO . writeIORef v) n : notifications s
                     , cleanup = cleanup s >> m }
    return $ Deferred $ liftIO $ readIORef v

-- | Register a cleanup action, to be executed after a notification has been
-- received and ignore the notification result.
after_ :: Monad m => Notification a -> AllocT m () -> RequestT m (Deferred m ())
after_ n (AllocT m) = do
    modify $ \s -> s { notifications = fmap (const (return ())) n : notifications s
                     , cleanup = cleanup s >> m }
    return $ Deferred $ return ()

-- | Register a cleanup action that is executed after all asynchronous commands
-- and notifications have been performed.
finally :: Monad m => AllocT m () -> RequestT m ()
finally (AllocT m) = modify $ \s -> s { cleanup = cleanup s >> m }

-- | Representation of an asynchronous server command. Asynchronous commands
-- are executed asynchronously with respect to other server commands.
--
-- There are two different ways of synchronising with an asynchronous command:
--
-- * using 'whenDone' for server-side synchronisation, or
--
-- * using 'async' and observing the result of a 'RequestT' action after calling
-- 'exec'.
newtype Async m a = Async (RequestT m (a, (Maybe OSC -> OSC)))

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
addSync :: MonadIO m => RequestT m a -> RequestT m a
addSync m = do
    a <- m
    b <- gets hasOSC
    when b $ do
        s <- gets syncState
        case s of
            NeedsSync -> do
                sid <- liftServer $ M.alloc M.syncIdAllocator
                send (C.sync (fromIntegral sid))
                after_ (N.synced sid) (M.free M.syncIdAllocator sid)
                return ()
            _ -> return ()
    return a

-- | Execute an server-side action after the asynchronous command has
-- finished.
whenDone :: MonadIO m => Async m a -> (a -> RequestT m b) -> Async m b
whenDone (Async m) f = Async $ do
    (a, g) <- m
    b <- f a
    return (b, g)

-- | Execute an asynchronous command asynchronously.
async :: MonadIO m => Async m a -> RequestT m (Deferred m a)
async (Async m) = do
    t <- gets timeTag
    ((a, g), s) <- liftServer $ runRequestT_ t NeedsSync $ addSync m
    case getOSC s of
        [] -> do
            send (g Nothing)
            modify $ \s' -> s' {
                syncState = max NeedsSync (syncState s')
              , notifications = notifications s' ++ notifications s
              , cleanup = cleanup s' >> cleanup s }
        osc -> do
            let t' = case syncState s of
                        HasSync -> immediately
                        _       -> t
            send $ g (Just (Bundle t' osc))
            modify $ \s' -> s' {
                syncState = max HasSync (syncState s')
              , notifications = notifications s' ++ notifications s
              , cleanup = cleanup s' >> cleanup s }
    return $ return a

-- | Execute an asynchronous command asynchronously.
{-async :: MonadIO m => Async m a -> RequestT m (Deferred m a)-}
{-async = asyncM . flip whenDone (return . return)-}

{-
-- | Execute an server-side action after the asynchronous command has
-- finished. The corresponding server commands are scheduled at a time @t@ in
-- the future.
whenDone :: MonadIO m => Async m a -> (a -> RequestT m b) -> RequestT m (Deferred b)
whenDone (Async m) f = do
    t <- gets timeTag
    (a, g) <- m
    (b, s) <- liftServer $ runRequestT_ t NeedsSync $ addSync (f a)
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
async :: MonadIO m => Async m a -> RequestT m (Deferred a)
async (Async m) = do
    (a, g) <- m
    send (g Nothing)
    modify $ setSyncState NeedsSync
    return $ pure a
-}

runRequestT :: MonadIO m => Time -> RequestT m a -> ServerT m (ServerT m a, Maybe (OSC, [Notification (ServerT m ())]))
runRequestT t m = do
    (a, s) <- runRequestT_ t NoSync $ addSync m
    let result = cleanup s >> return a
    case getOSC s of
        [] -> return (result, Nothing)
        osc -> let t' = case syncState s of
                            HasSync -> immediately
                            _ -> t
               in return (result, Just (Bundle t' osc, notifications s))

-- | Run the 'RequestT' action and return the result.
--
-- All asynchronous commands and notifications are guaranteed to have finished
-- when this function returns.
exec :: MonadIO m => Time -> RequestT m a -> ServerT m a
exec t m = do
    -- (a, s) <- runRequestT_ t NoSync $ addSync m
    -- case getOSC s of
    --     [] -> return ()
    --     osc -> do
    --         -- liftIO $ print osc
    --         let t' = case syncState s of
    --                     HasSync -> immediately
    --                     _ -> t
    (result, sync) <- runRequestT t m
    case sync of
        Nothing -> return ()
        Just (osc, ns) -> M.waitForAll osc ns >>= sequence_
    result

-- | Infix operator version of 'exec'.
(!>) :: MonadIO m => Time -> RequestT m a -> ServerT m a
(!>) = exec

-- | Run a 'RequestT' action that returns a pure result.
--
-- All asynchronous commands and notifications are guaranteed to have finished
-- when this function returns.
{-execPure :: MonadIO m => Time -> RequestT m a -> ServerT m a-}
{-execPure t m = exec t (m >>= return . return)-}

-- | Infix operator version of 'execPure'.
{-(~>) :: MonadIO m => Time -> RequestT m a -> ServerT m a-}
{-(~>) = execPure-}
