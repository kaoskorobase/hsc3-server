import           Control.Concurrent.MVar
import           Control.Monad (void, when)
import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Sound.SC3.UGen
import           Sound.SC3.Server.State.Monad
import           Sound.SC3.Server.State.Monad.Command
-- You need the hsc3-server-internal package in order to use the internal server
--import           Sound.SC3.Server.Monad.Process.Internal (withDefaultInternal)
import           Sound.SC3.Server.State.Monad.Process (withDefaultSynth)
import           Sound.OSC (pauseThread, pauseThreadUntil)
import qualified Sound.OSC as OSC
import           System.Posix.Signals (installHandler, keyboardSignal, Handler(Catch))
import           System.Random

-- Simple sine grain synthdef with frequency and amplitude controls and an ASR envelope.
sine :: UGen
sine = out 0 $ pan2 x (sinOsc KR 1 0 * 0.6) 1
    where x = sinOsc AR (control KR "freq" 440) 0
                * control KR "amp" 1
                * envGen KR (control KR "gate" 1) 1 0 1 RemoveSynth (envASR 0.02 1 0.1 EnvLin)

-- | Once a second ask for the server status and print it.
statusLoop :: Server ()
statusLoop = do
  statusM >>= liftIO . print
  pauseThread 1
  statusLoop

-- | Latency imposed on packets sent to the server.
latency :: Double
latency = 0.03

-- | Random sine grain generator loop.
grainLoop :: MVar a -> SynthDef -> Double -> Double -> Double -> Server ()
grainLoop quit synthDef delta sustain t = do
  -- Get a random frequency between 100 and 800 Hz
  f <- liftIO $ randomRIO (100,800)
  -- Get a random amplitude between 0.1 and 0.3
  a <- liftIO $ randomRIO (0.1,0.3)
  -- Get the root node
  r <- rootNode
  -- Create a synth of the sine grain SynthDef with the random freqyency and amplitude from above
  -- Schedule the synth for execution in 'latency' seconds in order to avoid jitter
  synth <- (t + latency) `exec` s_new synthDef AddToTail r [("freq", f), ("amp", a)]
  -- Fork a thread for releasing the synth after 'sustain' seconds
  fork $ do
    -- Calculate the time at which to release the synth and pause
    let t' = t + sustain
    pauseThreadUntil t'
    -- Release the synth, taking latency into account
    (t' + latency) `exec` s_release 0 synth
  -- Calculate the time for the next iteration and pause
  let t' = t + delta
  pauseThreadUntil t'
  -- Check whether to exit the loop and recurse
  b <- liftIO $ isEmptyMVar quit
  when b $ grainLoop quit synthDef delta sustain t'

newBreakHandler :: IO (MVar ())
newBreakHandler = do
  quit <- newEmptyMVar
  void $ installHandler keyboardSignal
          (Catch $ putStrLn "Quitting..." >> putMVar quit ())
          Nothing
  return quit

main :: IO ()
main = do
  -- Install keyboard break handler
  quit <- newBreakHandler
  -- Run an scsynth process
  -- You need the hsc3-server-internal package in order to use the internal server
  -- withDefaultInternal $ do
  withDefaultSynth $ do
    -- Create a new SynthDef
    sd <- exec_ $ d_recv "hsc3-server:sine" sine
    -- Fork the status display loop
    fork statusLoop
    -- Enter the grain loop
    grainLoop quit sd 0.03 0.06 =<< liftIO OSC.time
  takeMVar quit
