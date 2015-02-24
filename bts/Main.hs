module Main where

import BTS.RadioDevice
import BTS.RadioDevice.NullDevice
import BTS.RadioDevice.BladeRFDevice

import Control.Monad.Trans
import Control.Monad.Trans.State
import Control.Monad.IO.Class

-- ------------------------------------------------------ --
--   My little pony (a boring test program in reality..)
-- ------------------------------------------------------ --

doit :: (MonadIO a) => RadioDevice a -> IO ()
doit backend = withRadioDevice backend 99999 $ do
--  writeSamples backend 3
  -- grab all the internal states of the device for testing..
  let initwts = initialWriteTimestamp backend
  let initrts = initialReadTimestamp backend
  let fsiv    = fullScaleInputValue backend
  let fsov    = fullScaleOutputValue backend
  let txfreq  = getTxFreq backend
  let rxfreq  = getRxFreq backend
  sr      <- getSampleRate backend
--  nor     <- numberRead backend
--  now     <- numberWritten backend
  -- ok now print them..
  liftIO $ putStrLn $ "initialWriteTimestamp " ++ (show initwts)
  liftIO $ putStrLn $ "initialReadTimestamp " ++ (show initrts)
  liftIO $ putStrLn $ "fullScaleInputValue " ++ (show fsiv   )
  liftIO $ putStrLn $ "fullScaleOutputValue " ++ (show fsov   )
  liftIO $ putStrLn $ "getTxFreq " ++ (show txfreq )
  liftIO $ putStrLn $ "getRxFreq " ++ (show rxfreq )
  liftIO $ putStrLn $ "getSampleRate " ++ (show sr     )
--  liftIO $ putStrLn $ "numberRead " ++ (show nor    )
--  liftIO $ putStrLn $ "numberWritten " ++ (show now    )
  -- try some other shit
--  cts <- NullDevice $ gets currentStamp
--  liftIO $ putStrLn $ "cts = " ++ (show cts)
--  writeSamples backend 3
--  writeSamples backend 3
--  writeSamples backend 3
--  writeSamples backend 3
--  writeSamples backend 3
--  cts <- NullDevice $ gets currentStamp
--  liftIO $ putStrLn $ "cts = " ++ (show cts)
  --
  liftIO $ putStrLn "=============================================="
  -- grab all the internal states of the device for testing..
  let initwts = initialWriteTimestamp backend
  let initrts = initialReadTimestamp backend
  let fsiv    = fullScaleInputValue backend
  let fsov    = fullScaleOutputValue backend
  let txfreq  = getTxFreq backend
  let rxfreq  = getRxFreq backend
  sr      <- getSampleRate backend
--  nor     <- numberRead backend
--  now     <- numberWritten backend
  -- ok now print them..
  liftIO $ putStrLn $ "initialWriteTimestamp " ++ (show initwts)
  liftIO $ putStrLn $ "initialReadTimestamp " ++ (show initrts)
  liftIO $ putStrLn $ "fullScaleInputValue " ++ (show fsiv   )
  liftIO $ putStrLn $ "fullScaleOutputValue " ++ (show fsov   )
  liftIO $ putStrLn $ "getTxFreq " ++ (show txfreq )
  liftIO $ putStrLn $ "getRxFreq " ++ (show rxfreq )
  liftIO $ putStrLn $ "getSampleRate " ++ (show sr     )
--  liftIO $ putStrLn $ "numberRead " ++ (show nor    )
--  liftIO $ putStrLn $ "numberWritten " ++ (show now    )


main :: IO ()
main = do
  doit =<< constructBladeRFDevice
  doit =<< constructNullDevice
