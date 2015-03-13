{-|
  Module      : $Header$
  Copyright   : (c) 2014 Edward O'Callaghan
  License     : GPL-2
  Maintainer  : eocallaghan@alterapraxis.com
  Stability   : provisional
  Portability : portable

  BladeRF hardware backend driver.
-}

module BTS.RadioDevice.BladeRFDevice where

import Foreign hiding (void)

import Control.Exception (throwIO)

import Data.IORef
import Control.Monad (unless, when)

import qualified Data.ByteString as BS

import LibBladeRF.LibBladeRF
import LibBladeRF.Sampling
import LibBladeRF.Sync
import LibBladeRF.Gpio
import LibBladeRF.Gain
import LibBladeRF.Frequency
import LibBladeRF.Utils
import LibBladeRF.Types

--
import BTS.RadioDevice
import BTS.Logger

loggerName :: String
loggerName = "DeviceBladeRF.hs"

-- | ..
choiceFPGA :: BladeRFFPGASize -> String
choiceFPGA FPGA_UNKNOWN = "none"
choiceFPGA FPGA_40KLE   = "hostedx40.rbf"
choiceFPGA FPGA_115KLE  = "hostedx115.rbf"

-- | ..
getFPGAName :: DeviceHandle -> IO String
getFPGAName dev = fmap choiceFPGA (bladeRFGetFPGASize dev)

-- Stream defaults
defaultStreamRXXFERS   =    1
defaultStreamRXBuffers =    8
defaultStreamTXXFERS   =    1
defaultStreamTXBuffers =    8
defaultStreamSamples   = 2048
defaultStreamTimeout   =  500

-- .
shiftTxDC = 4
shiftRxDC = 5
maxRxDCOffset = 63
maxTxDCOffset = 63

gRxOffsetError    = 10
gRxOffsetCoef     = 1.5
gRxAverageDamping = 1024

bandwidth = 1500000

-- .........................
data HwState = HwState { sampleRate        :: Double    -- ^ the desired sampling rate
                       , underRun          :: Bool      -- ^ ..
                       }
-- .........................

-- | Rx DC offsets correction
data RxDCOffsetParams = RxDCOffsetParams { mDCCorrect     :: Bool
                                         , mRxMaxOffset   :: Double
                                         , mRxGain1       :: BladeRFVGAGainBounds
                                         , mRxCorrectionI :: Double
                                         , mRxCorrectionQ :: Double
                                         , mRxAverageI    :: Int
                                         , mRxAverageQ    :: Int
                                         }


-- | Construct an instance of the RadioDevice interface type with BladeRFDevice
constructBladeRFDevice :: Double -> IO RadioDevice
constructBladeRFDevice s = do
  initLogger loggerName

  --
  -- Read internal states
  rxGainRef            <- newIORef ((maxRxDCOffset + 1) :: Double)
  desiredSampleRateRef <- newIORef s   -- the desired bladeRF sampling rate
  actualSampleRateRef  <- newIORef 0.0 -- the actual bladeRF sampling rate
  samplesReadRef       <- newIORef 0   -- number of samples read from SDR
  samplesWrittenRef    <- newIORef 0   -- number of samples sent to SDR
  --
  isSuperSpeed         <- newIORef False
-- initial states
-- setRxOffsets(gConfig.getNum("TRX.RX.OffsetI"), gConfig.getNum("TRX.RX.OffsetQ"));
-- setTxOffsets(gConfig.getNum("TRX.TX.OffsetI"), gConfig.getNum("TRX.TX.OffsetQ"));
  let rxDCOffsetParams = RxDCOffsetParams { mDCCorrect     = True
                                          , mRxMaxOffset   = fromIntegral $ gRxOffsetError * gRxAverageDamping
                                          , mRxGain1       = RXVGA1_GAIN_MAX
                                          , mRxCorrectionI = maxRxDCOffset + 1
                                          , mRxCorrectionQ = maxRxDCOffset + 1
                                          , mRxAverageI    = 0 -- XX??
                                          , mRxAverageQ    = 0 -- XX??
                                          }
  rxDCOffsetRef <- newIORef rxDCOffsetParams

  --
  -- XXX makes this into a record and pass that..
  devRef <- bladeRFOpen isSuperSpeed samplesReadRef samplesWrittenRef rxDCOffsetRef rxGainRef
  --
  --
  let bladerfdev = RadioDevice { radioDeviceStart      = bladeRFDeviceStart devRef isSuperSpeed  -- XXX should be be "open" instead?
                               , radioDeviceStop       = bladeRFDeviceStop devRef
                               , radioDeviceSetVCTCXO  = \vctcxo -> bladeRFSetVCTCXO devRef (fromIntegral vctcxo)
--                               , radioDeviceSetVCTCXO = \d -> liftIO $ (>>= either throwIO return) $ bladeRFSetVCTCXO (fromIntegral d)
                               , radioDeviceSetTxFreq             = \x y -> bladeRFSetTxFreq devRef ((fromIntegral . round) x) ((fromIntegral . round) y)
                               , radioDeviceSetRxFreq             = \x y -> bladeRFSetRxFreq devRef ((fromIntegral . round) x) ((fromIntegral . round) y)
                               -- ..
                               , radioDeviceSetRxGain             = \g -> bladeRFSetRxGain devRef (round g) rxDCOffsetRef rxGainRef
                               , radioDeviceSetTxGain             = \g -> bladeRFSetTxGain devRef (round g)
                               , radioDeviceGetRxGain             = readIORef rxGainRef
                               -- ..
                               , radioDeviceGetMaxRxGain          = fromIntegral bladeRFGetMaxRxGain
                               , radioDeviceGetMinRxGain          = fromIntegral bladeRFGetMinRxGain
                               , radioDeviceGetMaxTxGain          = fromIntegral bladeRFGetMaxTxGain
                               , radioDeviceGetMinTxGain          = fromIntegral bladeRFGetMinTxGain
                               -- ..
                               , radioDeviceGetTxFreq             = 0
                               , radioDeviceGetRxFreq             = 0
                               , radioDeviceGetSampleRate         = readIORef actualSampleRateRef
                               , radioDeviceNumberRead            = readIORef samplesReadRef
                               , radioDeviceNumberWritten         = readIORef samplesWrittenRef
                               -- ..
                               , radioDeviceUpdateAlignment       = undefined -- Assume bladeRF never goes out of alignment
                               -- ..
                               , radioDeviceInitialWriteTimestamp = 1
                               , radioDeviceInitialReadTimestamp  = 1
                               -- ..
                               , radioDeviceFullScaleInputValue   = return 2040.0
                               , radioDeviceFullScaleOutputValue  = return 2040.0
                               -- ..
                               , radioDeviceReadSamples           = bladeRFReadSamples devRef
                               , radioDeviceWriteSamples          = bladeRFWriteSamples devRef
                               }

  infoM loggerName "Creating bladeRF Device..."
  -- sps = oversampling
  --
  return bladerfdev


bladeRFReadSamples :: IORef DeviceHandle -> Int -> TimeStamp -> IO (BS.ByteString, Int)
bladeRFReadSamples devRef n _ = do
  dev <- readIORef devRef
  ret <- bladeRFSyncRx dev n defaultStreamTimeout
  case ret of
    Left e -> throwIO e
    Right (rxSamples, _) -> do
      return (rxSamples, 0)
--      syncRx dev

bladeRFWriteSamples :: IORef DeviceHandle -> BS.ByteString -> Int -> TimeStamp -> Bool -> IO ()
bladeRFWriteSamples devRef bs _ _ _ = do
  dev <- readIORef devRef
  debugM loggerName $ "Tx sample length: " ++ " with timestamp: "
  putStrLn $ "we got this to write..." ++ show bs
  ret <- bladeRFSyncTx dev bs Nothing defaultStreamTimeout
  case ret of
    Left e -> throwIO e
    Right _ -> return ()

-- ..............................................
-- XXXXXXXXXXX wacky types need to use (liftIO . void) everywhere to strip out the Either from the IO () action and then lift it..


-- | ..
bladeRFDeviceStart :: IORef DeviceHandle -> IORef Bool -> IO ()
bladeRFDeviceStart devRef speedref = do
  dev <- readIORef devRef
  --
  rxTimestamp <- newIORef radioDeviceInitialReadTimestamp
  txTimestamp <- newIORef radioDeviceInitialWriteTimestamp
  rxResyncCandidate <- newIORef 0
  txBuffered  <- newIORef 0

  devspeed <- readIORef speedref
  if devspeed then
    noticeM loggerName "starting bladeRF in super speed mode..." else
    noticeM loggerName "starting bladeRF in high speed mode..."

  -- XXXXXXXXX For testing... XXXXXXXXXX
  --
  bladeRFLogSetVerbosity LOG_LEVEL_VERBOSE
  bladeRFSetLoopback dev LB_FIRMWARE
  --
  -- XXXXXXXXX For testing... XXXXXXXXXX

  bladeRFEnableModule dev MODULE_RX True
  bladeRFEnableModule dev MODULE_TX True
  return () -- XXX

-- | ..
bladeRFDeviceStop :: IORef DeviceHandle -> IO ()
bladeRFDeviceStop devRef = do
  dev <- readIORef devRef
  noticeM loggerName "stopping bladeRF"
  bladeRFEnableModule dev MODULE_RX False
  bladeRFEnableModule dev MODULE_TX False
  return () -- XXX


bladeRFOpen :: IORef Bool -> IORef Integer -> IORef Integer -> IORef RxDCOffsetParams -> IORef Double ->  IO (IORef DeviceHandle)
bladeRFOpen speedref samplesRRef samplesWRef rxdcoffset rxgainref = do
  --
  dev <- openBladeRF
  devRef <- newIORef dev
  --
  libVersion <- bladeRFLibVersion
  infoM loggerName $ "libbladeRF version: " ++ show libVersion
  serial <- bladeRFGetSerial dev
  fwVersion <- bladeRFFwVersion dev
  infoM loggerName $ "Opened bladeRF Serial= " ++ show serial ++ " firmware version " ++ show fwVersion

--  fpgaName <- return (choiceFPGA bladeRFGetFPGASize)

  bladeRFLoadFPGA dev =<< getFPGAName dev
  fpgaName <- getFPGAName dev

  fpgaVersion <- bladeRFFPGAVersion dev
  noticeM loggerName $ "bladeRF FPGA  " ++ show fpgaName ++ " is loaded with version " ++ show fpgaVersion

  --
  -- Figure out if we are using USB2 or USB3
  --
  devspeed <- bladeRFDeviceSpeed dev
  case devspeed of
    DEVICE_SPEED_HIGH     -> modifyIORef speedref (const False)
    DEVICE_SPEED_SUPER    -> modifyIORef speedref (const True)
    DEVICE_SPEED_UNKNOWN  -> emergencyM loggerName "Unsupported USB device speed"

  --
  -- Set Sampling Rate
  --
  let sps = 1
  let whole = round $ fromIntegral (sps * 13^8) / 48
  let numer = (sps * 13^8) - (whole * 48)
  let rate = BladeRFRationalRate { integer = whole, num = numer, den = 48 }

  infoM loggerName $ "Setting rate = " ++ show (integer rate)  ++ " + " ++ show (num rate) ++ " / " ++ show (den rate)
  realRate <- bladeRFSetRationalSampleRate dev MODULE_RX rate
  infoM loggerName $ "Actual RX rate = " ++ show (integer realRate)  ++ " + " ++ show (num realRate) ++ " / " ++ show (den realRate)
  realRate <- bladeRFSetRationalSampleRate dev MODULE_TX rate
  infoM loggerName $ "Actual TX rate = " ++ show (integer realRate)  ++ " + " ++ show (num realRate) ++ " / " ++ show (den realRate)

  --
  -- Set Bandwidth
  --
  infoM loggerName $ "Setting bandwidth = " ++ show (fromIntegral bandwidth / 10^6) ++ " Mhz"
  realBandwidth <- bladeRFSetBandwidth dev MODULE_RX bandwidth
  infoM loggerName $ "Actual RX bandwidth = " ++ show (fromIntegral realBandwidth / 10^6) ++ " MHz"
  realBandwidth <- bladeRFSetBandwidth dev MODULE_TX bandwidth
  infoM loggerName $ "Actual TX bandwidth = " ++ show (fromIntegral realBandwidth / 10^6) ++ " MHz"

  --
  -- Set Sync Configuration
  --
  _ <- bladeRFSyncConfig dev MODULE_RX FORMAT_SC16_Q11 defaultStreamRXBuffers defaultStreamSamples defaultStreamRXXFERS defaultStreamTimeout
  _ <- bladeRFSyncConfig dev MODULE_TX FORMAT_SC16_Q11 defaultStreamRXBuffers defaultStreamSamples defaultStreamTXXFERS defaultStreamTimeout

  --
  -- Setup GPIO's for timestamping
  --
  gpios <- bladeRFConfigGPIORead dev
  case gpios of
    Left e -> throwIO e
    Right gpios -> do
      putStrLn "========= GPIO Dump ========="
      mapM_ putStrLn $ debugBladeRFGPIOFlags gpios
      bladeRFConfigGPIOWrite dev $ GPIO_TIMESTAMP : gpios

  gpios <- bladeRFConfigGPIORead dev
  case gpios of
    Left e -> throwIO e
    Right gpios -> do
      putStrLn "========= GPIO Dump ========="
      mapM_ putStrLn $ debugBladeRFGPIOFlags gpios
      if elem GPIO_TIMESTAMP gpios
       then noticeM loggerName "bladeRF timestamping enabled."
       else alertM loggerName "Could not enable timestamping."

  --
  -- Set initial gains to minimum, the transceiver will adjust them later
  --
  _ <- bladeRFSetTxGain devRef bladeRFGetMinTxGain
  _ <- bladeRFSetRxGain devRef bladeRFGetMinRxGain rxdcoffset rxgainref

  modifyIORef samplesRRef (const 0)
  modifyIORef samplesWRef (const 0)

  return devRef


-- | return maximum Rx Gain
bladeRFGetMaxRxGain, bladeRFGetMinRxGain :: Int
bladeRFGetMaxRxGain = fromEnum RXVGA2_GAIN_MAX

-- | return minimum Rx Gain
bladeRFGetMinRxGain = fromEnum RXVGA2_GAIN_MIN

-- | return maximum Tx Gain
bladeRFGetMaxTxGain, bladeRFGetMinTxGain :: Int
bladeRFGetMaxTxGain = fromEnum TXVGA2_GAIN_MAX

-- | return minimum Tx Gain
bladeRFGetMinTxGain = fromEnum TXVGA2_GAIN_MIN


--
-- | Pass gain in dB's
bladeRFSetTxGain :: IORef DeviceHandle -> Int -> IO ()
bladeRFSetTxGain devRef g = do
  dev <- readIORef devRef
  _ <- bladeRFSetTXVGA1 dev TXVGA1_GAIN_MAX
  --
  if g > bladeRFGetMaxTxGain then
   do let g = bladeRFGetMaxTxGain
      bladeRFSetTXVGA2 dev (toEnum g)
      infoM loggerName $ "TX gain set to " ++ show g ++ " dB."
  else if g < bladeRFGetMinTxGain then
   do let g = bladeRFGetMinTxGain
      bladeRFSetTXVGA2 dev (toEnum g)
      infoM loggerName $ "TX gain set to " ++ show g ++ " dB."
  else
   do bladeRFSetTXVGA2 dev (toEnum g)
      infoM loggerName $ "TX gain set to " ++ show g ++ " dB."

--
-- | Pass gain in dB's
bladeRFSetRxGain :: IORef DeviceHandle -> Int -> IORef RxDCOffsetParams -> IORef Double -> IO ()
bladeRFSetRxGain devRef g rxoffsetref rxgainref = do
  dev <- readIORef devRef
  let newRxMaxOffset = (realToFrac g * gRxOffsetCoef + realToFrac gRxOffsetError) * realToFrac gRxAverageDamping
  modifyIORef rxoffsetref (\x -> x { mRxMaxOffset = newRxMaxOffset })
  rxoffsets <- readIORef rxoffsetref
  _ <- bladeRFSetRXVGA1 dev $ mRxGain1 rxoffsets
  _ <- bladeRFSetRXVGA2 dev (toEnum g)
  infoM loggerName $ "RX gain set to " ++ show g ++ " dB."
  modifyIORef rxgainref (const (fromIntegral g))

--
-- | Set the VCTCXO offset
bladeRFSetVCTCXO :: IORef DeviceHandle -> Word16 -> IO ()
bladeRFSetVCTCXO devRef d = do
  dev <- readIORef devRef
  infoM loggerName $ "set VCTCXO: " ++ show d
  bladeRFDACWrite dev $ shiftL d 8
  return () -- XXX

--
-- | Set the transmitter frequency
bladeRFSetTxFreq :: IORef DeviceHandle -> Int -> Word16 -> IO ()
bladeRFSetTxFreq devRef f d = do
  dev <- readIORef devRef
  infoM loggerName $ "set Tx freq: " ++ show f ++ " correction: " ++ show d
  bladeRFSetFrequency dev MODULE_TX f
  bladeRFSetVCTCXO devRef d
  return () -- XXX

--
-- | Set the receiver frequency
bladeRFSetRxFreq :: IORef DeviceHandle -> Int -> Word16 -> IO ()
bladeRFSetRxFreq devRef f d = do
  dev <- readIORef devRef
  infoM loggerName $ "set Rx freq: " ++ show f ++ " correction: " ++ show d
  bladeRFSetFrequency dev MODULE_RX f
  bladeRFSetVCTCXO devRef d
  return () -- XXX

-- XXX (internal function)
-- | Set the TX DAC correction offsets
bladeRFSetTxOffsets :: IORef DeviceHandle -> Int -> Int -> IO ()
bladeRFSetTxOffsets devRef corrI corrQ = do
  dev <- readIORef devRef
--    if ((abs(corrI) > MAX_TX_DC_OFFSET) || (abs(corrQ) > MAX_TX_DC_OFFSET)) return false;
  bladeRFSetCorrection dev MODULE_TX CORR_LMS_DCOFF_I (shiftL shiftTxDC corrI)
  bladeRFSetCorrection dev MODULE_TX CORR_LMS_DCOFF_Q (shiftL shiftTxDC corrQ)
  return () -- XXX

-- XXX (internal function)
-- | Set the RX DAC correction offsets
bladeRFSetRxOffsets :: IORef DeviceHandle -> Int -> Int -> IORef RxDCOffsetParams -> IO ()
bladeRFSetRxOffsets devRef corrI corrQ rxoffsetref = do
  dev <- readIORef devRef
  -- YYY we should throw an exception instead of just return'ing here??
  when ((abs(corrI) > (round maxRxDCOffset)) || (abs(corrQ) > (round maxRxDCOffset))) $ return ()
  --
  rxoffsets <- readIORef rxoffsetref
  let (rxCorrI, rxCorrQ) = (mRxCorrectionI rxoffsets, mRxCorrectionQ rxoffsets)
  unless ((fromIntegral corrI, fromIntegral corrQ) == (rxCorrI, rxCorrQ)) $ do
    modifyIORef rxoffsetref (\x -> x { mRxCorrectionI = fromIntegral corrI })
    modifyIORef rxoffsetref (\x -> x { mRxCorrectionQ = fromIntegral corrQ })
    bladeRFSetCorrection dev MODULE_RX CORR_LMS_DCOFF_I (shiftL shiftRxDC corrI)
    bladeRFSetCorrection dev MODULE_RX CORR_LMS_DCOFF_Q (shiftL shiftRxDC corrQ)
    return () -- XXX
