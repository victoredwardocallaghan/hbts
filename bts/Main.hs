module Main where

import BTS.RadioDevice
import BTS.RadioInterface
import BTS.RadioDevice.NullDevice
import BTS.RadioDevice.BladeRFDevice

import Control.Monad (forever)
import Control.Monad.Trans
import Control.Monad.Trans.State
import Control.Monad.IO.Class

import Control.Concurrent.ParallelIO.Global

import Pipes
import qualified Data.ByteString as BS

syncTx :: RadioDevice -> IO ()
syncTx rd = runEffect $ do
  input <- lift $ BS.readFile "testsig.bin"
  (yield (input, 0)) >-> (radioInterfaceSink rd)

syncRx :: RadioDevice -> IO ()
syncRx rd = runEffect $ for (radioInterfaceSource rd 111) $ \stream -> do
  lift $ BS.appendFile "out.bin" stream

-- ------------------------------------------------------ --
--   My little pony (a boring test program in reality..)
-- ------------------------------------------------------ --

doit ri = do
  backend <- getRadioDevice ri
  withRadio ri $ do
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
  liftIO $ putStrLn "========== BladeRFDevice ==========="
  brf <- constructBladeRFDevice 99999 -- 99999 is the desired sampleRate
  attachRadio ri brf 111111
  backend <- getRadioDevice ri
  withRadio ri $ do
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

    -- XXX
    parallel_ [syncTx backend, syncRx backend] >> stopGlobalPool

main :: IO ()
main = do
  doit =<< constructRadioInterface
