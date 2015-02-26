module Main where

import BTS.RadioDevice
import BTS.RadioInterface
import BTS.RadioDevice.NullDevice
import BTS.RadioDevice.BladeRFDevice

import Control.Monad.Trans
import Control.Monad.Trans.State
import Control.Monad.IO.Class

-- ------------------------------------------------------ --
--   My little pony (a boring test program in reality..)
-- ------------------------------------------------------ --

doit ri = do
  backend <- getRadioDevice ri
  withRadioDevice backend $ do
    liftIO $ putStrLn "=========== NullDevice ==========="
    -- grab all the internal states of the device for testing..
    let initwts = radioDeviceInitialWriteTimestamp backend
    let initrts = radioDeviceInitialReadTimestamp backend
    fsiv       <- fullScaleInputValue ri
    fsov       <- fullScaleOutputValue ri
    let txfreq  = radioDeviceGetTxFreq backend
    let rxfreq  = radioDeviceGetRxFreq backend
    sr         <- radioDeviceGetSampleRate backend
    -- ok now print them..
    liftIO $ putStrLn $ "initialWriteTimestamp " ++ (show initwts)
    liftIO $ putStrLn $ "initialReadTimestamp " ++ (show initrts)
    liftIO $ putStrLn $ "fullScaleInputValue " ++ (show fsiv   )
    liftIO $ putStrLn $ "fullScaleOutputValue " ++ (show fsov   )
    liftIO $ putStrLn $ "getTxFreq " ++ (show txfreq )
    liftIO $ putStrLn $ "getRxFreq " ++ (show rxfreq )
    liftIO $ putStrLn $ "getSampleRate " ++ (show sr     )
  --
  --
  brf <- constructBladeRFDevice 99999 -- 99999 is the desired sampleRate
  attachRadio ri brf 111111
  backend <- getRadioDevice ri
  withRadioDevice backend $ do
    liftIO $ putStrLn "========== BladeRFDevice ==========="
    -- grab all the internal states of the device for testing..
    let initwts = radioDeviceInitialWriteTimestamp backend
    let initrts = radioDeviceInitialReadTimestamp backend
    fsiv       <- fullScaleInputValue ri
    fsov       <- fullScaleOutputValue ri
    let txfreq  = radioDeviceGetTxFreq backend
    let rxfreq  = radioDeviceGetRxFreq backend
    sr         <- radioDeviceGetSampleRate backend
    -- ok now print them..
    liftIO $ putStrLn $ "initialWriteTimestamp " ++ (show initwts)
    liftIO $ putStrLn $ "initialReadTimestamp " ++ (show initrts)
    liftIO $ putStrLn $ "fullScaleInputValue " ++ (show fsiv   )
    liftIO $ putStrLn $ "fullScaleOutputValue " ++ (show fsov   )
    liftIO $ putStrLn $ "getTxFreq " ++ (show txfreq )
    liftIO $ putStrLn $ "getRxFreq " ++ (show rxfreq )
    liftIO $ putStrLn $ "getSampleRate " ++ (show sr     )

main :: IO ()
main = do
  doit =<< constructRadioInterface
