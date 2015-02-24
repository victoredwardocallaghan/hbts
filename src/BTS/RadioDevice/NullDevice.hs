{-|
  Module      : $Header$
  Copyright   : (c) 2014 Edward O'Callaghan
  License     : GPL-2
  Maintainer  : eocallaghan@alterapraxis.com
  Stability   : provisional
  Portability : portable

  Diagnostic soft-hardware.
-}

module BTS.RadioDevice.NullDevice where

import Data.Time.Clock
import Data.IORef

import BTS.RadioDevice


--data HwState = HwState { sampleRate     :: Double    -- ^ the desired sampling rate
--                       , currentStamp   :: TimeStamp -- ^ internal timestamp clock state
--                       , startTime      :: UTCTime   -- ^ ..
--                       , underRun       :: Bool      -- ^ ..
--                       }


--updateTime :: NullDevice ()
--updateTime  = do
--  sr <- NullDevice $ gets sampleRate
--  st <- NullDevice $ gets startTime
--  ct <- liftIO getCurrentTime
--  mods s4 (const $ floor (sr * realToFrac (diffUTCTime ct st)))

-- | Internal initial state-object constructor
--runNullDevice sr m = getCurrentTime >>= \curTime -> evalStateT (unNullDevice m) (HwState sr 0 curTime False)

bracket start stop body = do
     start
     body
     stop

-- | Object constructor
withNullDevice :: IO () -> IO ()
withNullDevice stuff = do
  putStrLn "INFO creating SDR device..."
  bracket nullDeviceStart nullDeviceStop stuff


-- | Construct an instance of the RadioDevice interface type with NullDevice
constructNullDevice :: Double -> IO RadioDevice
constructNullDevice s = do
  --
  -- Return internal status values
  sampleRateRef     <- newIORef s -- XXX !!! should be a param
  samplesReadRef    <- newIORef 0 -- number of samples read from SDR
  samplesWrittenRef <- newIORef 0 -- number of samples sent to SDR
  startTimeRef      <- getCurrentTime >>= newIORef
  --
  --
  let nulldev = RadioDevice { withRadioDevice       = withNullDevice
                            , setVCTCXO             = undefined
                            , setTxFreq             = nullDeviceSetTxFreq
                            , setRxFreq             = nullDeviceSetRxFreq
                            -- ..
                            , setRxGain             = undefined
                            , setTxGain             = undefined
                            , getRxGain             = return 0.0
                            -- ..
                            , getMaxRxGain          = 0.0
                            , getMinRxGain          = 0.0
                            , getMaxTxGain          = 0.0
                            , getMinTxGain          = 0.0
                            -- ..
                            , getTxFreq             = 0
                            , getRxFreq             = 0
                            , getSampleRate         = readIORef sampleRateRef
                            , numberRead            = readIORef samplesReadRef
                            , numberWritten         = readIORef samplesWrittenRef
                            -- ..
                            , updateAlignment       = nullDeviceUpdateAlignment
                            -- ..
                            , initialWriteTimestamp = 20000 -- Returns the starting write Timestamp
                            , initialReadTimestamp  = 20000 -- Returns the starting read Timestamp
                            -- ..
                            , fullScaleInputValue   = 13500.0 -- returns the full-scale transmit amplitude
                            , fullScaleOutputValue  = 9450.0  -- returns the full-scale receive amplitude
                            -- ..
                            , readSamples           = \_ -> return ()
                            , writeSamples          = \_ -> return ()
                            }


  return nulldev


-- public:

-- int loadBurst(short *wDummyBurst, int len);
--  dummyBurst = wDummyBurst;
--  dummyBurstSz = len;
--  return 0;

-- | Start the SDR
nullDeviceStart :: IO ()
nullDeviceStart  = putStrLn $ "INFO " ++ "starting SDR..."

-- | Stop the SDR
nullDeviceStop :: IO ()
nullDeviceStop  = putStrLn $ "INFO " ++ "stopping SDR..."

-- | ..
--nullDeviceWriteSamples :: TimeStamp -> NullDevice ()
--nullDeviceWriteSamples ts = do
--  updateTime
--  cts <- NullDevice $ gets currentStamp
--  urun <- NullDevice $ gets underRun
--  mods s6 (const $ urun || (cts < ts))

-- | Update the alignment between the read and write timestamps
nullDeviceUpdateAlignment :: TimeStamp -> IO ()
nullDeviceUpdateAlignment t = return ()

-- | Set the transmitter frequency
nullDeviceSetTxFreq :: Double -> Double -> IO ()
nullDeviceSetTxFreq f a = return ()

-- | Set the receiver frequency
nullDeviceSetRxFreq :: Double -> Double -> IO ()
nullDeviceSetRxFreq f a = return ()
