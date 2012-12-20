import           Control.Monad.IO.Class (liftIO)
import           Sound.SC3.UGen
import           Sound.SC3.Server.State.Monad
import           Sound.SC3.Server.State.Monad.Command
-- You need the hsc3-server-internal package in order to use the internal server
--import           Sound.SC3.Server.Monad.Process.Internal (withDefaultInternal)
import           Sound.SC3.Server.State.Monad.Process (withDefaultSynth)
import qualified Sound.OpenSoundControl as OSC

sine = out 0 $ pan2 x (sinOsc KR 1 0) 1
    where x = sinOsc AR (control KR "freq" 440) 0
                * control KR "amp" 1
                * envGen KR (control KR "gate" 1) 1 0 1 RemoveSynth (envASR 1 1 1 EnvLin)

pauseThread = liftIO . OSC.pauseThread

statusLoop = do
    statusM >>= liftIO . print
    pauseThread 1
    statusLoop

-- You need the hsc3-server-internal package in order to use the internal server
--run = withDefaultInternal
run = withDefaultSynth
 
main = run $ do
    --exec_ $ dumpOSC TextPrinter
    r <- rootNode
    synth <- exec_ $ do
        sd <- d_recv "hsc3-server:sine" sine
        s_new sd AddToTail r [("freq", 440), ("amp", 0.2)]
    fork statusLoop
    pauseThread 10
    exec_ $ s_release 0 synth
    pauseThread 2
