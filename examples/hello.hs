import           Control.Monad.IO.Class (liftIO)
import           Sound.SC3.Server.Process.Monad
import           Sound.SC3.UGen
import           Sound.SC3.Server.Monad
import           Sound.SC3.Server.Monad.Command
import           Sound.SC3.Server.Monad.Send ((~>), (!>), async, whenDone)
import           Sound.SC3.Server.Notification
import           Sound.OpenSoundControl (immediately)
import qualified Sound.OpenSoundControl as OSC

sine = out 0 $ pan2 x (sinOsc KR 1 0) 1
    where x = sinOsc AR (control KR "freq" 440) 0
                * control KR "amp" 1
                * envGen KR (control KR "gate" 1) 1 0 1 RemoveSynth (envASR 1 1 1 EnvLin)

pauseThread = liftIO . OSC.pauseThread

statusLoop = do
    immediately !> status >>= liftIO . print
    pauseThread 1
    statusLoop

--run = withDefaultSynth
run = withDefaultInternal

latency = 0.03
 
main = run $ do
    r <- rootNode
    liftIO $ putStrLn "starting synth"
    synth <- immediately !> do
        dumpOSC TextPrinter
        async $ d_recv "hsc3-server:sine" sine `whenDone`
            \sd -> s_new sd AddToTail r [("freq", 440), ("amp", 0.2)]
    liftIO $ putStrLn "started synth"
    fork statusLoop
    liftIO $ putStrLn "forked statusLoop"
    pauseThread 10
    immediately ~> s_release 0 synth
    liftIO $ putStrLn "released synth"
    pauseThread 2
