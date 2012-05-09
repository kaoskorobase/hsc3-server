import           Control.Monad.IO.Class (liftIO)
import           Sound.SC3.Server.Process.Monad
import           Sound.SC3.UGen
import           Sound.SC3.Server.Monad
import           Sound.SC3.Server.Monad.Command
import           Sound.SC3.Server.Monad.Request ((!>), async, extract, whenDone)
import           Sound.SC3.Server.Notification
import           Sound.OpenSoundControl (immediately)
import qualified Sound.OpenSoundControl as OSC

sine = out 0 $ pan2 x (sinOsc KR 1 0) 1
    where x = sinOsc AR (control KR "freq" 440) 0
                * control KR "amp" 1
                * envGen KR (control KR "gate" 1) 1 0 1 RemoveSynth (envASR 1 1 1 EnvLin)

pauseThread = liftIO . OSC.pauseThread

statusLoop = do
    immediately !> status >>= extract >>= liftIO . print
    pauseThread 1
    statusLoop

--run = withDefaultSynth
run = withDefaultInternal

latency = 0.03
 
main = run $ do
    immediately !> dumpOSC TextPrinter
    r <- rootNode
    synth <- extract =<< immediately !> do
        async $ d_recv "hsc3-server:sine" sine `whenDone`
            \sd -> s_new sd AddToTail r [("freq", 440), ("amp", 0.2)]
    fork statusLoop
    pauseThread 10
    immediately !> s_release 0 synth
    pauseThread 2
