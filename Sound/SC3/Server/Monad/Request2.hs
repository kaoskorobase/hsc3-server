module Sound.SC3.Server.Monad.Request2 where

import qualified Control.Monad.State as State
import           Sound.SC3.Server.Monad (MonadIdAllocator, MonadSendOSC(..), MonadServer, ServerT)
import           Sound.OpenSoundControl (OSC(..), Time, immediately)

data Request =
    Sync OSC
  | Async (Maybe OSC -> OSC)

flatten :: Time -> [Request] -> [OSC] -> OSC
flatten t [] ps = Bundle t ps
flatten t (r:rs) ps =
  case r of
    Sync osc -> flatten t rs (osc : ps)
    Async f  -> case ps of
                  [] -> let ps' = [f Nothing]
                        in flatten t rs ps'
                  _  -> let ps' = [f (Just (Bundle t ps))]
                        in flatten immediately rs ps'

-- | Representation of a server-side action (or sequence of actions).
newtype RequestT m a = RequestT (State.StateT [Request] (ServerT m) a)
                        deriving (Applicative, Functor, Monad)

newtype AsyncT m a = AsyncT (RequestT m (Maybe OSC -> OSC, a))


exec :: Time -> RequestT m a -> ServerT m a
exec = undefined

sync_ :: AsyncT m a -> RequestT m ()
sync_ (AsyncT m) = do
  (f, _) <- m
  State.modify_ ((:)(Sync (f Nothing)))
  return ()

sync :: AsyncT m a -> RequestT m a
sync (AsyncT m) = do
  (f, a) <- m
  State.modify_ ((:)(Async f))
  return a

