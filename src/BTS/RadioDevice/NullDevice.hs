{-|
  Module      : $Header$
  Copyright   : (c) 2014 Edward O'Callaghan
  License     : GPL-2
  Maintainer  : eocallaghan@alterapraxis.com
  Stability   : provisional
  Portability : portable

  Diagnostic soft-hardware.
-}

{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module BTS.RadioDevice.NullDevice where

import Control.Applicative (Applicative(..), (<$>))
import Control.Monad (ap)
import Control.Monad.Trans
import Control.Monad.Trans.State
import Control.Monad.IO.Class
import Data.Time.Clock

import BTS.RadioDevice


data HwState = HwState { sampleRate     :: Double    -- ^ the desired sampling rate
                       , samplesRead    :: Integer   -- ^ number of samples read from SDR
                       , samplesWritten :: Integer   -- ^ number of samples sent to SDR
                       , currentStamp   :: TimeStamp -- ^ internal timestamp clock state
                       , startTime      :: UTCTime   -- ^ ..
                       , underRun       :: Bool      -- ^ ..
                       }

newtype NullDevice a = NullDevice { unNullDevice :: StateT HwState IO a }
                     deriving (Monad, MonadIO)

instance Functor NullDevice where
  {-# INLINE fmap #-}
  fmap f m = NullDevice (f <$> unNullDevice m)

instance Applicative NullDevice where
  {-# INLINE pure #-}
  pure  = return
  {-# INLINE (<*>) #-}
  (<*>) = ap

--  short *dummyBurst;
--  int dummyBurstSz;
--  int dummyBurstCursor;
--  bool underrun;


-- Type'ed state transistion structure
type Selector a = (NullDevice a, a -> NullDevice ())

s4 :: Selector TimeStamp
s4 = (NullDevice (gets currentStamp), \x -> NullDevice (modify (\vs -> vs {currentStamp = x})))

s6 :: Selector Bool
s6 = (NullDevice (gets underRun), \x -> NullDevice (modify (\vs -> vs {underRun = x})))

sel :: Selector a -> NullDevice a
sel = fst

mods :: Selector a -> (a -> a) -> NullDevice ()
mods (gf,uf) mfun = do st <- gf
                       uf (mfun st)

updateTime :: NullDevice ()
updateTime  = do
  sr <- NullDevice $ gets sampleRate
  st <- NullDevice $ gets startTime
  ct <- liftIO getCurrentTime
  mods s4 (const $ floor (sr * realToFrac (diffUTCTime ct st)))

-- | Internal initial state-object constructor
runNullDevice :: Double -> NullDevice a -> IO a
runNullDevice sr m = getCurrentTime >>= \curTime -> evalStateT (unNullDevice m) (HwState sr 0 0 0 curTime False)

bracket start stop body = do
     start
     body
     stop

-- | Object constructor
withNullDevice :: Double -> NullDevice a -> IO ()
withNullDevice sr stuff = do
  liftIO $ putStrLn "INFO creating SDR device..."
  runNullDevice sr $ bracket nullDeviceStart nullDeviceStop stuff


-- | Construct an instance of the RadioDevice interface type with NullDevice
constructNullDevice :: IO (RadioDevice NullDevice)
constructNullDevice = do
  let nulldev = RadioDevice { withRadioDevice       = withNullDevice
                            , setVCTCXO             = undefined
                            , setTxFreq             = nullDeviceSetTxFreq
                            , setRxFreq             = nullDeviceSetRxFreq
                            -- **
                            , setRxGain             = undefined
                            , setTxGain             = undefined
                            , getRxGain             = return 0.0
                            -- **
                            , getMaxRxGain          = return 0.0
                            , getMinRxGain          = return 0.0
                            , getMaxTxGain          = return 0.0
                            , getMinTxGain          = return 0.0
                            -- **
                            , getTxFreq             = nullDeviceGetTxFreq
                            , getRxFreq             = nullDeviceGetRxFreq
                            , getSampleRate         = nullDeviceGetSampleRate
                            , numberRead            = nullDeviceNumberRead
                            , numberWritten         = nullDeviceNumberWritten
                            -- **
                            , updateAlignment       = nullDeviceUpdateAlignment
                            -- **
                            , initialWriteTimestamp = nullDeviceInitialWriteTimestamp
                            , initialReadTimestamp  = nullDeviceInitialReadTimestamp
                            -- **
                            , fullScaleInputValue   = nullDeviceFullScaleInputValue
                            , fullScaleOutputValue  = nullDeviceFullScaleOutputValue
                            -- **
                            , readSamples           = nullDeviceReadSamples
                            , writeSamples          = nullDeviceWriteSamples
                            }

  return nulldev


-- public:

-- int loadBurst(short *wDummyBurst, int len);
--  dummyBurst = wDummyBurst;
--  dummyBurstSz = len;
--  return 0;

-- | Start the SDR
nullDeviceStart :: NullDevice ()
nullDeviceStart  = do
 liftIO $ putStrLn $ "INFO " ++ "starting SDR..."

-- | Stop the SDR
nullDeviceStop :: NullDevice ()
nullDeviceStop  = do
 liftIO $ putStrLn $ "INFO " ++ "stopping SDR..."

-- | ..
nullDeviceReadSamples :: TimeStamp -> NullDevice ()
nullDeviceReadSamples ts = undefined -- XXX

-- | ..
nullDeviceWriteSamples :: TimeStamp -> NullDevice ()
nullDeviceWriteSamples ts = do
  updateTime
  cts <- NullDevice $ gets currentStamp
  urun <- NullDevice $ gets underRun
  mods s6 (const $ urun || (cts < ts))

-- | Update the alignment between the read and write timestamps
nullDeviceUpdateAlignment :: TimeStamp -> NullDevice ()
nullDeviceUpdateAlignment t = return ()

-- | Set the transmitter frequency
nullDeviceSetTxFreq :: Double -> Double -> NullDevice ()
nullDeviceSetTxFreq f a = return ()

-- | Set the receiver frequency
nullDeviceSetRxFreq :: Double -> Double -> NullDevice ()
nullDeviceSetRxFreq f a = return ()

-- | Returns the starting write Timestamp
nullDeviceInitialWriteTimestamp :: NullDevice TimeStamp
nullDeviceInitialWriteTimestamp = return 20000

-- | Returns the starting read Timestamp
nullDeviceInitialReadTimestamp :: NullDevice TimeStamp
nullDeviceInitialReadTimestamp = return 20000

-- | returns the full-scale transmit amplitude
nullDeviceFullScaleInputValue :: NullDevice Double
nullDeviceFullScaleInputValue = return 13500.0

-- | returns the full-scale receive amplitude
nullDeviceFullScaleOutputValue :: NullDevice Double
nullDeviceFullScaleOutputValue = return 9450.0

-- | Return internal status values
nullDeviceGetTxFreq :: NullDevice Double
nullDeviceGetTxFreq = return 0

nullDeviceGetRxFreq :: NullDevice Double
nullDeviceGetRxFreq = return 0

nullDeviceGetSampleRate :: NullDevice Double
nullDeviceGetSampleRate = NullDevice $ gets sampleRate

nullDeviceNumberRead :: NullDevice Integer
nullDeviceNumberRead = NullDevice $ gets samplesRead

nullDeviceNumberWritten :: NullDevice Integer
nullDeviceNumberWritten = NullDevice $ gets samplesWritten
