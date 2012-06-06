module Sound.SC3.Server.Monad.Process (
    withSynth
  , withDefaultSynth
  -- * Re-exported for convenience
  , module Sound.SC3.Server.Process.Options
  , OutputHandler(..)
  , defaultOutputHandler
) where

import qualified Sound.SC3.Server.Connection as Conn
import           Sound.SC3.Server.Monad (Server)
import qualified Sound.SC3.Server.Monad as Server
import           Sound.SC3.Server.Process (OutputHandler(..), defaultOutputHandler)
import qualified Sound.SC3.Server.Process as Process
import           Sound.SC3.Server.Process.Options

withSynth :: ServerOptions -> RTOptions -> OutputHandler -> Server a -> IO a
withSynth serverOptions rtOptions outputHandler action =
    Process.withSynth
        serverOptions
        rtOptions
        outputHandler
        $ \t -> Conn.open t >>= Server.runServer action serverOptions

withDefaultSynth :: Server a -> IO a
withDefaultSynth = withSynth defaultServerOptions defaultRTOptions defaultOutputHandler
