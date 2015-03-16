module Main where

import Transceiver

import BTS.RadioDevice
import BTS.RadioInterface
import BTS.RadioDevice.NullDevice
import BTS.RadioDevice.BladeRFDevice

import Control.Monad (forever)
import Control.Monad.Trans
import Control.Monad.Trans.State
import Control.Monad.IO.Class

import Control.Concurrent.ParallelIO.Global

import System.IO

import Pipes
import qualified Pipes.ByteString as P
import qualified Data.ByteString as BS

syncTx :: RadioDevice -> IO ()
syncTx rd = withFile "testsig.bin" ReadMode $ \hIn ->
  runEffect $ P.fromHandle hIn >-> radioInterfaceSink rd

syncRx :: RadioDevice -> IO ()
syncRx rd = withFile "out.bin" WriteMode $ \hOut -> do
  runEffect $ (radioInterfaceSource rd 111) >-> P.toHandle hOut
--  runEffect $ for (radioInterfaceSource rd 111) $ \stream -> do
--    yield stream >-> P.toHandle hOut

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
    parallel_ [syncRx backend, syncTx backend] >> stopGlobalPool


main :: IO ()
main = do
  parallel_ [controlLoop, dataLoop] >> stopGlobalPool
--  doit =<< constructRadioInterface
