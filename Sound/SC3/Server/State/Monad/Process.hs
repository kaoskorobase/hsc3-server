module Sound.SC3.Server.State.Monad.Process (
  withTransport
, withSynth
, withDefaultSynth
-- * Re-exported for convenience
, module Sound.SC3.Server.Process
) where

import           Data.Default (def)
import qualified Sound.SC3.Server.Connection as Conn
import           Sound.SC3.Server.Process hiding (withSynth, withTransport)
import qualified Sound.SC3.Server.Process as Process
import           Sound.SC3.Server.State.Monad (Server)
import qualified Sound.SC3.Server.State.Monad as Server

-- | Open a transport to an existing @scsynth@ process determined by
--   'networkPort' and run the supplied 'Server' action.
withTransport ::
    ServerOptions     -- ^ General server options
 -> RTOptions         -- ^ Realtime server options
 -> Maybe String      -- ^ Host to connect to (defaults to localhost)
 -> Server a          -- ^ Action to execute
 -> IO a              -- ^ Action result
withTransport serverOptions rtOptions host action =
  Process.withTransport
    serverOptions
    rtOptions
    host
    $ \t -> Conn.open t >>= Server.runServer action serverOptions

-- | Start an @scsynth@ instance and run the supplied 'Server' action.
--
-- When the action returns, @scsynth@ will quit.
withSynth ::
    ServerOptions     -- ^ General server options
 -> RTOptions         -- ^ Realtime server options
 -> OutputHandler     -- ^ Output handler
 -> Server a          -- ^ Action to execute
 -> IO a              -- ^ Action result
withSynth serverOptions rtOptions outputHandler action =
  Process.withSynth
    serverOptions
    rtOptions
    outputHandler
    $ \t -> Conn.open t >>= Server.runServer action serverOptions

-- | Start an @scsynth@ instance with default options and run the supplied 'Server' action.
withDefaultSynth :: Server a -> IO a
withDefaultSynth = withSynth def def def
