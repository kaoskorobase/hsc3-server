module Sound.SC3.Server.Monad.Process (
    withSynth
  , withDefaultSynth
  -- * Re-exported for convenience
  , module Sound.SC3.Server.Process
) where

import           Data.Default (def)
import qualified Sound.SC3.Server.Connection as Conn
import           Sound.SC3.Server.Monad (Server)
import qualified Sound.SC3.Server.Monad as Server
import           Sound.SC3.Server.Process hiding (withSynth)
import qualified Sound.SC3.Server.Process as Process

withSynth :: ServerOptions -> RTOptions -> OutputHandler -> Server a -> IO a
withSynth serverOptions rtOptions outputHandler action =
    Process.withSynth
        serverOptions
        rtOptions
        outputHandler
        $ \t -> Conn.open t >>= Server.runServer action serverOptions

withDefaultSynth :: Server a -> IO a
withDefaultSynth = withSynth def def def
