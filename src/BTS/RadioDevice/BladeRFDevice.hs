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
import Control.Monad (unless)

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
  --
  let bladerfdev = RadioDevice { radioDeviceStart      = bladeRFDeviceStart isSuperSpeed  -- XXX should be be "open" instead?
                               , radioDeviceStop       = bladeRFDeviceStop
                               , radioDeviceSetVCTCXO  = bladeRFSetVCTCXO . fromIntegral
--                               , radioDeviceSetVCTCXO = \d -> liftIO $ (>>= either throwIO return) $ bladeRFSetVCTCXO (fromIntegral d)
                               , radioDeviceSetTxFreq             = \x y -> bladeRFSetTxFreq ((fromIntegral . round) x) ((fromIntegral . round) y)
                               , radioDeviceSetRxFreq             = \x y -> bladeRFSetRxFreq ((fromIntegral . round) x) ((fromIntegral . round) y)
                               -- ..
                               , radioDeviceSetRxGain             = \g -> bladeRFSetRxGain (round g) rxDCOffsetRef rxGainRef
                               , radioDeviceSetTxGain             = bladeRFSetTxGain . round
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
                               , radioDeviceReadSamples           = bladeRFReadSamples
                               , radioDeviceWriteSamples          = bladeRFWriteSamples
                               }

  infoM loggerName "Creating bladeRF Device..."
  -- sps = oversampling
  --
  -- XXX makes this into a record and pass that..
  bladeRFOpen isSuperSpeed samplesReadRef samplesWrittenRef rxDCOffsetRef rxGainRef 
  --
  return bladerfdev


bladeRFReadSamples  = undefined
bladeRFWriteSamples = undefined

-- ..............................................
-- XXXXXXXXXXX wacky types need to use (liftIO . void) everywhere to strip out the Either from the IO () action and then lift it..


-- | ..
bladeRFDeviceStart :: IORef Bool -> IO ()
bladeRFDeviceStart speedref = withBladeRF $ \dev -> do
  --
  rxTimestamp <- newIORef radioDeviceInitialReadTimestamp
  txTimestamp <- newIORef radioDeviceInitialWriteTimestamp
  rxResyncCandidate <- newIORef 0
  txBuffered  <- newIORef 0

  devspeed <- readIORef speedref
  if devspeed then
    noticeM loggerName "starting bladeRF in super speed mode..." else
    noticeM loggerName "starting bladeRF in high speed mode..."

  bladeRFEnableModule dev MODULE_RX True
  bladeRFEnableModule dev MODULE_TX True
  return () -- XXX

-- | ..
bladeRFDeviceStop :: IO ()
bladeRFDeviceStop = withBladeRF $ \dev -> do
  noticeM loggerName "stopping bladeRF"
  bladeRFEnableModule dev MODULE_RX False
  bladeRFEnableModule dev MODULE_TX False
  return () -- XXX


bladeRFOpen :: IORef Bool -> IORef Integer -> IORef Integer -> IORef RxDCOffsetParams -> IORef Double ->  IO ()
bladeRFOpen speedref samplesRRef samplesWRef rxdcoffset rxgainref = withBladeRF $ \dev -> do
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
  _ <- bladeRFSetTxGain' dev bladeRFGetMinTxGain
  _ <- bladeRFSetRxGain' dev bladeRFGetMinRxGain rxdcoffset rxgainref

  modifyIORef samplesRRef (const 0)
  modifyIORef samplesWRef (const 0)


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
bladeRFSetTxGain :: Int -> IO ()
bladeRFSetTxGain g = withBladeRF $ \dev -> bladeRFSetTxGain' dev g

bladeRFSetTxGain' :: DeviceHandle -> Int -> IO ()
bladeRFSetTxGain' dev g = do
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
bladeRFSetRxGain :: Int -> IORef RxDCOffsetParams -> IORef Double -> IO ()
bladeRFSetRxGain g rxoffsetref rxgainref = withBladeRF $ \dev -> bladeRFSetRxGain' dev g rxoffsetref rxgainref

bladeRFSetRxGain' :: DeviceHandle -> Int -> IORef RxDCOffsetParams -> IORef Double -> IO ()
bladeRFSetRxGain' dev g rxoffsetref rxgainref = do
  let newRxMaxOffset = (realToFrac g * gRxOffsetCoef + realToFrac gRxOffsetError) * realToFrac gRxAverageDamping
  modifyIORef rxoffsetref (\x -> x { mRxMaxOffset = newRxMaxOffset })
  rxoffsets <- readIORef rxoffsetref
  _ <- bladeRFSetRXVGA1 dev $ mRxGain1 rxoffsets
  _ <- bladeRFSetRXVGA2 dev (toEnum g)
  infoM loggerName $ "RX gain set to " ++ show g ++ " dB."
  modifyIORef rxgainref (const (fromIntegral g))

--
-- | Set the VCTCXO offset
bladeRFSetVCTCXO :: Word16 -> IO ()
bladeRFSetVCTCXO d = withBladeRF $ \dev -> do
  infoM loggerName $ "set VCTCXO: " ++ show d
  bladeRFDACWrite dev $ shiftL d 8
  return () -- XXX

--
-- | Set the transmitter frequency
bladeRFSetTxFreq :: Int -> Word16 -> IO ()
bladeRFSetTxFreq f d = withBladeRF $ \dev -> do
  infoM loggerName $ "set Tx freq: " ++ show f ++ " correction: " ++ show d
  bladeRFSetFrequency dev MODULE_TX f
  bladeRFSetVCTCXO d
  return () -- XXX

--
-- | Set the receiver frequency
bladeRFSetRxFreq :: Int -> Word16 -> IO ()
bladeRFSetRxFreq f d = withBladeRF $ \dev -> do
  infoM loggerName $ "set Rx freq: " ++ show f ++ " correction: " ++ show d
  bladeRFSetFrequency dev MODULE_RX f
  bladeRFSetVCTCXO d
  return () -- XXX

-- XXX (internal function)
-- | Set the TX DAC correction offsets
bladeRFSetTxOffsets :: Int -> Int -> IO ()
bladeRFSetTxOffsets corrI corrQ = withBladeRF $ \dev -> do
--    if ((abs(corrI) > MAX_TX_DC_OFFSET) || (abs(corrQ) > MAX_TX_DC_OFFSET)) return false;
  bladeRFSetCorrection dev MODULE_TX CORR_LMS_DCOFF_I (shiftL shiftTxDC corrI)
  bladeRFSetCorrection dev MODULE_TX CORR_LMS_DCOFF_Q (shiftL shiftTxDC corrQ)
  return () -- XXX

-- XXX (internal function)
-- | Set the RX DAC correction offsets
bladeRFSetRxOffsets :: Int -> Int -> IORef RxDCOffsetParams -> IO ()
bladeRFSetRxOffsets corrI corrQ rxoffsetref = withBladeRF $ \dev -> do
--    if ((abs(corrI) > MAX_RX_DC_OFFSET) || (abs(corrQ) > MAX_RX_DC_OFFSET)) return false;
  rxoffsets <- readIORef rxoffsetref
  let (rxCorrI, rxCorrQ) = (mRxCorrectionI rxoffsets, mRxCorrectionQ rxoffsets)
  unless ((fromIntegral corrI, fromIntegral corrQ) == (rxCorrI, rxCorrQ)) $ do
    modifyIORef rxoffsetref (\x -> x { mRxCorrectionI = fromIntegral corrI })
    modifyIORef rxoffsetref (\x -> x { mRxCorrectionQ = fromIntegral corrQ })
    bladeRFSetCorrection dev MODULE_RX CORR_LMS_DCOFF_I (shiftL shiftRxDC corrI)
    bladeRFSetCorrection dev MODULE_RX CORR_LMS_DCOFF_Q (shiftL shiftRxDC corrQ)
    return () -- XXX
