import           Control.Monad.IO.Class (liftIO)
import           Sound.SC3.Server.Process.Monad
import           Sound.SC3.UGen
import           Sound.SC3.Server.Monad
import           Sound.SC3.Server.Monad.Command
import           Sound.SC3.Server.Monad.Send ((~>), (!>), async)
import           Sound.SC3.Server.Notification
import           Sound.OpenSoundControl (immediately)
import qualified Sound.OpenSoundControl as OSC
import           System.Random

sine = out 0 $ pan2 x (sinOsc KR 1 0 * 0.6) 1
    where x = sinOsc AR (control KR "freq" 440) 0
                * control KR "amp" 1
                * envGen KR (control KR "gate" 1) 1 0 1 RemoveSynth (envASR 0.02 1 0.1 EnvLin)

pauseThread = liftIO . OSC.pauseThread
pauseThreadUntil = liftIO . OSC.pauseThreadUntil

statusLoop = do
    immediately !> status >>= liftIO . print
    pauseThread 1
    statusLoop

grainLoop synthDef delta sustain t = do
    f <- liftIO $ randomRIO (100,800)
    a <- liftIO $ randomRIO (0.1,0.3)
    r <- rootNode
    synth <- OSC.UTCr (t + latency) ~> s_new synthDef AddToTail r [("freq", f), ("amp", a)]
    fork $ do
        let t' = t + sustain
        pauseThreadUntil t'
        OSC.UTCr (t' + latency) !> s_release 0 synth
    let t' = t + delta
    pauseThreadUntil t'
    grainLoop synthDef delta sustain t'

--run = withDefaultSynth
run = withDefaultInternal

latency = 0.03
 
main = run $ do
    sd <- immediately !> async (d_recv "hsc3-server:sine" sine)
    fork statusLoop
    grainLoop sd 0.03 0.06 =<< liftIO OSC.utcr
