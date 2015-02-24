{-|
  Module      : $Header$
  Copyright   : (c) 2014 Edward O'Callaghan
  License     : GPL-2
  Maintainer  : eocallaghan@alterapraxis.com
  Stability   : provisional
  Portability : portable

  BladeRF hardware backend driver.
-}

{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module BTS.RadioDevice.BladeRFDevice where

import Foreign hiding (void)
import Foreign.C.Types
import Foreign.C.String
import Foreign.Ptr

import Control.Applicative (Applicative(..), (<$>))
import Control.Monad (ap, void)
import Control.Monad.Trans
import Control.Monad.Trans.State
import Control.Monad.IO.Class
-- import Control.Exception

import LibBladeRF.LibBladeRF
import LibBladeRF.Sampling
import LibBladeRF.Sync
import LibBladeRF.Gpio
import LibBladeRF.Gain
import LibBladeRF.Frequency
import LibBladeRF.Utils
import LibBladeRF.Types

--
import Bindings.LibBladeRF.Types

--
import BTS.RadioDevice

import BTS.Logger

-- | ..
choiceFPGA :: BladeRFFPGASize -> String
choiceFPGA FPGA_UNKNOWN = "none"
choiceFPGA FPGA_40KLE   = "hostedx40.rbf"
choiceFPGA FPGA_115KLE  = "hostedx115.rbf"

-- | ..
getFPGAName :: DeviceHandle -> IO String
getFPGAName dev = fmap choiceFPGA (bladeRFGetFPGASize dev)


loggerName :: String
loggerName = "DeviceBladeRF.hs"


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

-- | Rx DC offsets correction
data RxDCOffsetParams = RxDCOffsetParams { mDCCorrect     :: Bool
                                         , mRxMaxOffset   :: Int
                                         , mRxCorrectionI :: Int
                                         , mRxCorrectionQ :: Int
                                         , mRxAverageI    :: Int
                                         , mRxAverageQ    :: Int
                                         }

data HwState = HwState { sampleRate        :: Double    -- ^ the desired sampling rate
                       , aSampleRate       :: Double    -- ^ the actual bladeRF sampling rate
                       , samplesRead       :: Integer   -- ^ number of samples read from SDR
                       , samplesWritten    :: Integer   -- ^ number of samples sent to SDR
                       , rxTimestamp       :: TimeStamp -- ^ ..?? 
                       , txTimestamp       :: TimeStamp -- ^ ..??
                       , rxResyncCandidate :: TimeStamp -- ^ ..??
                       , underRun          :: Bool      -- ^ ..
                       , rxGain            :: Double    -- ^ ..??
                       , isSuperSpeed      :: Bool      -- ^ ..??
                       , rxDCOffsets       :: RxDCOffsetParams -- ^ Seriously think of a shorter name??????????????????????????????
                       }

newtype BladeRFDevice a = BladeRFDevice { unBladeRFDevice :: StateT HwState IO a }
                        deriving (Monad, MonadIO)

instance Functor BladeRFDevice where
  {-# INLINE fmap #-}
  fmap f m = BladeRFDevice (f <$> unBladeRFDevice m)

instance Applicative BladeRFDevice where
  {-# INLINE pure #-}
  pure  = return
  {-# INLINE (<*>) #-}
  (<*>) = ap

-- | Internal initial state-object constructor
runBladeRFDevice :: Double -> BladeRFDevice a -> IO a
runBladeRFDevice sr m = evalStateT (unBladeRFDevice m) (HwState sr 0.0 0 0 1 1 0 False 0 False (RxDCOffsetParams True (gRxOffsetError * gRxAverageDamping) (maxRxDCOffset + 1) (maxRxDCOffset + 1) 0 0))

-- initial states
-- setRxOffsets(gConfig.getNum("TRX.RX.OffsetI"), gConfig.getNum("TRX.RX.OffsetQ"));
-- setTxOffsets(gConfig.getNum("TRX.TX.OffsetI"), gConfig.getNum("TRX.TX.OffsetQ"));
-- mRxGain1 = BLADERF_RXVGA1_GAIN_MAX;

bracket start stop body = do
     (liftIO . void) start
     body
     (liftIO . void) stop

-- | Object constructor
withBladeRFDevice :: Double -> BladeRFDevice a -> IO ()
withBladeRFDevice sr stuff = runBladeRFDevice sr $ bracket bladeRFDeviceStart bladeRFDeviceStop stuff

-- | Construct an instance of the RadioDevice interface type with BladeRFDevice
constructBladeRFDevice :: IO (RadioDevice BladeRFDevice)
constructBladeRFDevice = do
  let nulldev = RadioDevice { withRadioDevice       = withBladeRFDevice
                            , setVCTCXO             = liftIO . void . bladeRFSetVCTCXO . fromIntegral
--                            , setVCTCXO             = \d -> liftIO $ (>>= either throwIO return) $ bladeRFSetVCTCXO (fromIntegral d)
                            , setTxFreq             = \x y -> liftIO . void $ bladeRFSetTxFreq ((fromIntegral . round) x) ((fromIntegral . round) y)
                            , setRxFreq             = \x y -> liftIO . void $ bladeRFSetRxFreq ((fromIntegral . round) x) ((fromIntegral . round) y)
                            -- **
                            , setRxGain             = \g _ -> liftIO . void $ bladeRFSetRxGain (round g)
                            , setTxGain             = \g _ -> liftIO . void $ bladeRFSetTxGain (round g)
                            , getRxGain             = bladeRFGetRxGain
                            -- **
                            , getMaxRxGain          = fromIntegral bladeRFGetMaxRxGain
                            , getMinRxGain          = fromIntegral bladeRFGetMinRxGain
                            , getMaxTxGain          = fromIntegral bladeRFGetMaxTxGain
                            , getMinTxGain          = fromIntegral bladeRFGetMinTxGain
                            -- **
                            , getTxFreq             = 0
                            , getRxFreq             = 0
                            , getSampleRate         = bladeRFGetSampleRate
                            , numberRead            = bladeRFNumberRead
                            , numberWritten         = bladeRFNumberWritten
                            -- **
                            , updateAlignment       = undefined -- ^ Assume bladeRF never goes out of alignment
                            -- **
                            , initialWriteTimestamp = 1
                            , initialReadTimestamp  = 1
                            -- **
                            , fullScaleInputValue   = 2040.0
                            , fullScaleOutputValue  = 2040.0
                            -- **
                            , readSamples           = bladeRFDeviceReadSamples
                            , writeSamples          = bladeRFDeviceWriteSamples
                            }
  --
  --
  bladeRFOpen
  --
  return nulldev


bladeRFDeviceReadSamples  = undefined
bladeRFDeviceWriteSamples = undefined


-- ..............................................


-- XXXXXXXXXXX wacky types need to use (liftIO . void) everywhere to strip out the Either from the IO () action and then lift it..
--bladeRFDeviceStart :: BladeRFDevice ()
bladeRFDeviceStart = withBladeRF $ \dev -> do
  speed <- bladeRFDeviceSpeed dev
  noticeM loggerName $ "starting bladeRF in  " ++ show speed ++ " speed mode..."
  bladeRFEnableModule dev MODULE_RX True
  bladeRFEnableModule dev MODULE_TX True

--bladeRFDeviceStop :: BladeRFDevice ()
bladeRFDeviceStop = withBladeRF $ \dev -> do
  noticeM loggerName "stopping bladeRF"
  bladeRFEnableModule dev MODULE_RX False
  bladeRFEnableModule dev MODULE_TX False


-- | Read internal states

bladeRFGetRxGain :: BladeRFDevice Double
bladeRFGetRxGain  = BladeRFDevice $ gets rxGain

bladeRFGetSampleRate :: BladeRFDevice Double
bladeRFGetSampleRate  = BladeRFDevice $ gets aSampleRate

bladeRFNumberRead :: BladeRFDevice Integer
bladeRFNumberRead  = BladeRFDevice $ gets samplesRead

bladeRFNumberWritten :: BladeRFDevice Integer
bladeRFNumberWritten  = BladeRFDevice $ gets samplesWritten


bladeRFOpen = withBladeRF $ \dev -> do
  initLogger loggerName
  infoM loggerName "Creating bladeRF Device..."
  -- sps = oversampling
  --
  --

  libVersion <- liftIO bladeRFLibVersion
  infoM loggerName (" libbladeRF version: " ++ show libVersion)
  serial <- bladeRFGetSerial dev
  fwVersion <- bladeRFFwVersion dev
  infoM loggerName $ " Opened bladeRF Serial= " ++ show serial ++ " firmware version " ++ show fwVersion

--  fpgaName <- liftIO $ return (choiceFPGA bladeRFGetFPGASize)

  bladeRFLoadFPGA dev =<< getFPGAName dev
  fpgaName <- getFPGAName dev

  fpgaVersion <- bladeRFFPGAVersion dev
  noticeM loggerName $ " bladeRF FPGA  " ++ show fpgaName ++ " is loaded with version " ++ show fpgaVersion

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
  bladeRFSyncConfig dev MODULE_RX FORMAT_SC16_Q11 defaultStreamRXBuffers defaultStreamSamples defaultStreamRXXFERS defaultStreamTimeout
  bladeRFSyncConfig dev MODULE_TX FORMAT_SC16_Q11 defaultStreamRXBuffers defaultStreamSamples defaultStreamTXXFERS defaultStreamTimeout

  --
  -- Setup GPIO's for timestamping
  --
  gpios <- bladeRFConfigGPIORead dev
  bladeRFConfigGPIOWrite dev $ gpios .|. c'BLADERF_GPIO_TIMESTAMP
  gpios <- bladeRFConfigGPIORead dev
  if (gpios .&. c'BLADERF_GPIO_TIMESTAMP) == c'BLADERF_GPIO_TIMESTAMP
   then noticeM loggerName "bladeRF timestamping enabled."
   else alertM loggerName "Could not enable timestamping."

  --
  -- Set initial gains to minimum, the transceiver will adjust them later
  --
  -- XXX this breaks this WHY???????????
--  bladeRFSetTxGain bladeRFGetMinTxGain
--  bladeRFSetRxGain bladeRFGetMinRxGain


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
bladeRFSetTxGain g = withBladeRF $ \dev -> do
  bladeRFSetTXVGA1 dev $ fromEnum TXVGA1_GAIN_MAX
  bladeRFSetTXVGA2 dev g
  infoM loggerName $ "TX gain set to " ++ show g ++ " dB."

--
-- | Pass gain in dB's
bladeRFSetRxGain g = withBladeRF $ \dev -> do
  bladeRFSetRXVGA1 dev $ fromEnum RXVGA1_GAIN_MAX
  bladeRFSetRXVGA2 dev g
  infoM loggerName $ "RX gain set to " ++ show g ++ " dB."

--
-- | Set the VCTCXO offset
bladeRFSetVCTCXO d = withBladeRF $ \dev -> do
  infoM loggerName $ "set VCTCXO: " ++ show d
  bladeRFDACWrite dev $ shiftL d 8

--
-- | Set the transmitter frequency
bladeRFSetTxFreq f d = withBladeRF $ \dev -> do
  infoM loggerName $ "set Tx freq: " ++ show f ++ " correction: " ++ show d
  bladeRFSetFrequency dev MODULE_TX f
  bladeRFSetVCTCXO d

--
-- | Set the receiver frequency
bladeRFSetRxFreq f d = withBladeRF $ \dev -> do
  infoM loggerName $ "set Rx freq: " ++ show f ++ " correction: " ++ show d
  bladeRFSetFrequency dev MODULE_RX f
  bladeRFSetVCTCXO d

-- XXX (internal function)
-- | Set the TX DAC correction offsets
bladeRFSetTxOffsets corrI corrQ = withBladeRF $ \dev -> do
  bladeRFSetCorrection dev MODULE_TX CORR_LMS_DCOFF_I (shiftL shiftTxDC corrI)
  bladeRFSetCorrection dev MODULE_TX CORR_LMS_DCOFF_Q (shiftL shiftTxDC corrQ)

-- XXX (internal function)
-- | Set the RX DAC correction offsets
bladeRFSetRxOffsets corrI corrQ = withBladeRF $ \dev -> do
  bladeRFSetCorrection dev MODULE_RX CORR_LMS_DCOFF_I (shiftL shiftRxDC corrI)
  bladeRFSetCorrection dev MODULE_RX CORR_LMS_DCOFF_Q (shiftL shiftRxDC corrQ)
