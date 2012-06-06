import           Control.Concurrent.MVar
import           Control.Monad (void, when)
import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Sound.SC3.UGen
import           Sound.SC3.Server.Monad
import           Sound.SC3.Server.Monad.Command
-- You need the hsc3-server-internal package in order to use the internal server
--import           Sound.SC3.Server.Monad.Process.Internal (withDefaultInternal)
import           Sound.SC3.Server.Monad.Process (withDefaultSynth)
import           Sound.SC3.Server.Monad.Request
import           Sound.SC3.Server.Notification
import           Sound.OpenSoundControl (immediately)
import qualified Sound.OpenSoundControl as OSC
import           System.Posix.Signals (installHandler, keyboardSignal, Handler(Catch))
import           System.Random

sine = out 0 $ pan2 x (sinOsc KR 1 0 * 0.6) 1
    where x = sinOsc AR (control KR "freq" 440) 0
                * control KR "amp" 1
                * envGen KR (control KR "gate" 1) 1 0 1 RemoveSynth (envASR 0.02 1 0.1 EnvLin)

pauseThread :: MonadIO m => Double -> m ()
pauseThread = liftIO . OSC.pauseThread
pauseThreadUntil = liftIO . OSC.pauseThreadUntil

statusLoop = do
    immediately !> status >>= extract >>= liftIO . print
    pauseThread 1
    statusLoop

keepRunning = liftIO . isEmptyMVar

grainLoop quit synthDef delta sustain t = do
    f <- liftIO $ randomRIO (100,800)
    a <- liftIO $ randomRIO (0.1,0.3)
    r <- rootNode
    synth <- OSC.UTCr (t + latency) !> s_new synthDef AddToTail r [("freq", f), ("amp", a)]
    fork $ do
        let t' = t + sustain
        pauseThreadUntil t'
        OSC.UTCr (t' + latency) !> s_release 0 synth
        return ()
    let t' = t + delta
    pauseThreadUntil t'
    b <- keepRunning quit
    when b $ grainLoop quit synthDef delta sustain t'

-- You need the hsc3-server-internal package in order to use the internal server
--run = withDefaultInternal
run = withDefaultSynth

latency = 0.03
 
newBreakHandler :: IO (MVar ())
newBreakHandler = do
    quit <- newEmptyMVar
    void $ installHandler keyboardSignal
            (Catch $ putStrLn "Quitting..." >> putMVar quit ())
            Nothing
    return quit
 
main :: IO ()
main = do
    quit <- newBreakHandler
    run $ do
        sd <- immediately !> async (d_recv "hsc3-server:sine" sine) >>= extract
        fork statusLoop
        grainLoop quit sd 0.03 0.06 =<< liftIO OSC.utcr
    takeMVar quit
