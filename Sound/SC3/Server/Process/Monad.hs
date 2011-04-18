module Sound.SC3.Server.Process.Monad (
    withSynth
  , withDefaultSynth
  , withInternal
  , withDefaultInternal
  -- * Re-exported for convenience
  , module Sound.SC3.Server.Options
  , OutputHandler(..)
  , defaultOutputHandler
) where

import qualified Sound.SC3.Server.Connection as Conn
import qualified Sound.SC3.Server.Internal as Process
import           Sound.SC3.Server.Monad (Server)
import qualified Sound.SC3.Server.Monad as Server
import           Sound.SC3.Server.Options
import           Sound.SC3.Server.Process ( OutputHandler(..), defaultOutputHandler )
import qualified Sound.SC3.Server.Process as Process
import qualified Sound.SC3.Server.State as State

withSynth :: ServerOptions -> RTOptions -> OutputHandler -> Server a -> IO a
withSynth serverOptions rtOptions outputHandler action =
    Process.withSynth
        serverOptions
        rtOptions
        outputHandler
        $ \t -> Conn.new (State.new serverOptions) t >>= Server.runServer action

withDefaultSynth :: Server a -> IO a
withDefaultSynth = withSynth defaultServerOptions defaultRTOptions defaultOutputHandler

withInternal :: ServerOptions -> RTOptions -> OutputHandler -> Server a -> IO a
withInternal serverOptions rtOptions outputHandler action =
    Process.withInternal
        serverOptions
        rtOptions
        outputHandler
        $ \t -> Conn.new (State.new serverOptions) t >>= Server.runServer action

withDefaultInternal :: Server a -> IO a
withDefaultInternal = withInternal defaultServerOptions defaultRTOptions defaultOutputHandler
