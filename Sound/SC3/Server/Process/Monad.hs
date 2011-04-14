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

import qualified Sound.OpenSoundControl as OSC
import qualified Sound.SC3.Server.Connection as Conn
import qualified Sound.SC3.Server.Internal as Process
import           Sound.SC3.Server.Monad (Server)
import qualified Sound.SC3.Server.Monad as Server
import           Sound.SC3.Server.Options
import           Sound.SC3.Server.Process ( OutputHandler(..), defaultOutputHandler )
import qualified Sound.SC3.Server.Process as Process
import qualified Sound.SC3.Server.State as State

withSynth :: OSC.Transport t => (ServerOptions -> RTOptions -> IO t) -> ServerOptions -> RTOptions -> OutputHandler -> Server a -> IO a
withSynth openTransport serverOptions rtOptions outputHandler action =
    Process.withSynth
        openTransport
        serverOptions
        rtOptions
        outputHandler
        $ \t -> Conn.new (State.new serverOptions) t >>= Server.runServer action

withDefaultSynth :: OSC.Transport t => (ServerOptions -> RTOptions -> IO t) -> Server a -> IO a
withDefaultSynth openTransport action = withSynth openTransport defaultServerOptions defaultRTOptions defaultOutputHandler action

withInternal :: ServerOptions -> RTOptions -> OutputHandler -> Server a -> IO a
withInternal serverOptions rtOptions outputHandler action =
    Process.withInternal
        serverOptions
        rtOptions
        outputHandler
        $ \t -> Conn.new (State.new serverOptions) t >>= Server.runServer action

withDefaultInternal :: Server a -> IO a
withDefaultInternal = withInternal defaultServerOptions defaultRTOptions defaultOutputHandler
