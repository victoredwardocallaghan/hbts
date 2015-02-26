{-|
  Module      : $Header$
  Copyright   : (c) 2014 Edward O'Callaghan
  License     : GPL-2
  Maintainer  : eocallaghan@alterapraxis.com
  Stability   : provisional
  Portability : portable

  This module covers L1 FEC, L2 and L3 message translation.
-}

module BTS.RadioInterface where

import Data.IORef

import BTS.GSMCommon
import BTS.RadioDevice
import BTS.RadioDevice.NullDevice

import BTS.Logger

loggerName :: String
loggerName  = "RadioInterface.hs"

-- | samples per GSM symbol
-- SAMPSPERSYM = 1
inChunk     = 625
outChunk    = 625

-- | Organised GSM bursts by GSM timestamps
data RadioVector = RadioVector { time  :: Time -- ^ the burst's GSM timestamp
                               , arfcn :: Int
                               } deriving (Eq)

-- comparison operator, used for sorting
instance Ord RadioVector where
    rv <  rv' = time rv <  time rv'
    rv >  rv' = time rv >  time rv'
    rv <= rv' = time rv <= time rv'
    rv >= rv' = time rv >= time rv'

-- | timestamp read and write operators
readTime :: RadioVector -> Time
readTime  = time

writeTime :: RadioVector -> Time -> RadioVector
writeTime (RadioVector _ arfcn) time = RadioVector time arfcn

-- | ARFCN read and write operators
readARFCN :: RadioVector -> Int
readARFCN  = arfcn

writeARFCN :: RadioVector -> Int -> RadioVector
writeARFCN (RadioVector time _) = RadioVector time



-- | a priority queue of radioVectors, i.e. GSM bursts, sorted so that earliest element is at top
-- class VectorQueue : public InterthreadPriorityQueue<radioVector>

-- | the top element of the queue
nextTime :: Time
nextTime  = undefined

-- | Get stale burst, if any.
getStaleBurst :: Time              -- ^ The target time.
              -> Maybe RadioVector -- ^ Just burst older than target time, removed from queue, or Nothing
getStaleBurst t = undefined

-- | Get current burst, if any.
getCurrentBurst :: Time              -- ^ The target time.
                -> Maybe RadioVector -- ^ Just burst at the target time, removed from queue, or Nothing
getCurrentBurst t = undefined


-- | a FIFO of RadioVectors

-- XXX stub types
data VectorFIFO = FOO

--
fifoSize :: Int
fifoSize  = undefined
--
put :: RadioVector
put  = undefined
--
get :: RadioVector
get  = undefined


-- | The basestation clock
data RadioClock = RadioClock { rclock :: Time
                             , signal :: Int  -- ^ XXX fix type?? pthread_cond_broadcast() properly dont need this at all??
                             }

-- | Get clock value
getClock :: RadioClock -> Time
getClock  = rclock

-- | Set clock
-- void set(const GSM::Time& wTime) { mClock = wTime; signal.broadcast(); }
setClock :: RadioClock -> Time -> RadioClock
setClock (RadioClock _ sig) t = RadioClock t sig

-- | Increment clock
-- void incTN() { mClock.incTN(); signal.broadcast(); }
incClock :: RadioClock -> RadioClock
incClock (RadioClock clk sig) = RadioClock (incTN clk 1) sig


-- | Interface to abstract the software Transceiver with the SDR hardware backend driver
data RadioInterface = RadioInterface { setSamplesPerSymbol  :: Int -> IO ()
                                     , getSamplesPerSymbol  :: Int
                                     , isUnderrun           :: Bool                      -- ^ check for underrun, resets underrun value
                                     , setRxGain            :: Double -> IO ()           -- ^ set receive gain
                                     , setVCTCXO            :: Int -> IO ()              -- ^ tune VCTCXO
                                     , tuneTx               :: Double -> Double -> IO () -- ^ set transmit frequency
                                     , tuneRx               :: Double -> Double -> IO () -- ^ set receive frequency
                                     , setPowerAttenuation  :: Double -> IO ()
                                     , fullScaleInputValue  :: IO Double                 -- ^ returns the full-scale transmit amplitude
                                     , fullScaleOutputValue :: IO Double                 -- ^ returns the full-scale receive amplitude
                                     , attachRadio          :: RadioDevice -> Int -> IO() -- ^ attach an existing SDR to this interface
                                     , getRadioDevice       :: IO RadioDevice             -- ^ return the radio device
-- ^ return the receive FIFO
-- VectorFIFO* receiveFIFO() { return &mReceiveFIFO;}
-- ^ return the basestation clock
-- RadioClock* getClock(void) { return &mClock;};
--getClock :: RadioClock
-- ^ drive transmission of GSM bursts
-- void driveTransmitRadio(signalVector &radioBurst, bool zeroBurst);
-- driveReceiveRadio :: () -- ^ drive reception of GSM bursts
                                     }

--  RadioInterface(RadioDevice* wRadio = NULL,
--		 int receiveOffset = 3,
--		 int wRadioOversampling = SAMPSPERSYM,
--		 int wTransceiverOversampling = SAMPSPERSYM,
--		 bool wLoadTest = false,
--		 unsigned int wNumARFCNS = 1,
--		 GSM::Time wStartTime = GSM::Time(0));
--
--		  underrun = false;
--		    
--		  sendCursor = 0; 
--		  rcvCursor = 0;
--		  mOn = false;
--		            
--		  mRadio = wRadio;
--		  receiveOffset = wReceiveOffset;
--		  samplesPerSymbol = wRadioOversampling;
--		  mClock.set(wStartTime);
--		  powerScaling = 1.0;
--		  mNumARFCNs = wNumARFCNs;
--
--		  loadTest = wLoadTest;

-- | constructor
constructRadioInterface :: IO RadioInterface
constructRadioInterface  = do
  --
--		 int wRadioOversampling = SAMPSPERSYM,
--		 int wTransceiverOversampling = SAMPSPERSYM,
  radioOnRef   <- newIORef False
  numARFCNRef  <- newIORef 1     -- XXX make into a Maybe argument with a default value of Just 1
  --
  nulldev <- constructNullDevice 0 -- start with stub radio hw and attach hw later once found!
  radioDeviceRef <- newIORef nulldev

  let ri = RadioInterface { setSamplesPerSymbol  = undefined
                          , getSamplesPerSymbol  = undefined
                          , isUnderrun           = undefined -- do flowstate <- readIORef bufferFlowRef ; modifyIORef bufferFlowRef False ; return flowstate
                          , setRxGain            = radioInterfaceSetRxGain radioDeviceRef
                          , setVCTCXO            = radioInterfaceSetVCTCXO radioDeviceRef
                          , tuneTx               = radioInterfaceTuneTx radioDeviceRef
                          , tuneRx               = radioInterfaceTuneRx radioDeviceRef
                          , setPowerAttenuation  = radioInterfaceSetPowerAttenuation radioDeviceRef
                          , fullScaleInputValue  = radioInterfaceFullScaleInputValue radioDeviceRef
                          , fullScaleOutputValue = radioInterfaceFullScaleOutputValue radioDeviceRef
                          , attachRadio          = \r s -> modifyIORef radioDeviceRef (const r) -- ; modifyIORef radioOversamplingRef s
                          , getRadioDevice       = readIORef radioDeviceRef
                          }

-- void setSamplesPerSymbol(int wSamplesPerSymbol) {if (!mOn) samplesPerSymbol = wSamplesPerSymbol;}
-- int getSamplesPerSymbol() { return samplesPerSymbol;}
  return ri

-- | ..
radioInterfaceFullScaleInputValue :: IORef RadioDevice -> IO Double
radioInterfaceFullScaleInputValue r = do
  radio <- readIORef r
  radioDeviceFullScaleInputValue radio

-- | ..
radioInterfaceFullScaleOutputValue :: IORef RadioDevice -> IO Double
radioInterfaceFullScaleOutputValue r = do
  radio <- readIORef r
  radioDeviceFullScaleOutputValue radio

-- | ??
radioInterfaceSetPowerAttenuation :: IORef RadioDevice -> Double -> IO ()
radioInterfaceSetPowerAttenuation r a = do
  radio <- readIORef r
  hwdBAtten <- radioDeviceSetTxGain radio (-a)
  let dBAtten = a -- + hwdBAtten --   dBAtten -= (-HWdBAtten);
  let linearAtten = 10 ** (0.1 * dBAtten)
  let powerScaling | linearAtten < 1.0 = 1
                   | otherwise         = 1 / (sqrt linearAtten)
  infoM loggerName $ "setting HW gain to " ++ show hwdBAtten ++ " and power scaling to " ++ show powerScaling

-- | ..
radioInterfaceSetRxGain :: IORef RadioDevice -> Double -> IO ()
radioInterfaceSetRxGain r g = do
  radio <- readIORef r
  radioDeviceSetRxGain radio g

-- | .. t = tune voltage
radioInterfaceSetVCTCXO :: IORef RadioDevice -> Int -> IO ()
radioInterfaceSetVCTCXO r t = do
  radio <- readIORef r
  radioDeviceSetVCTCXO radio t

-- | .. inputs (freq, adjFreq)
radioInterfaceTuneTx :: IORef RadioDevice -> Double -> Double -> IO ()
radioInterfaceTuneTx r f a = do
  radio <- readIORef r
  radioDeviceSetTxFreq radio f a

-- | .. inputs (freq, adjFreq)
radioInterfaceTuneRx :: IORef RadioDevice -> Double -> Double -> IO ()
radioInterfaceTuneRx r f a = do
  radio <- readIORef r
  radioDeviceSetRxFreq radio f a


--  XXX internal state of the RadioInterface
--
-- { receiveFIFO      :: VectorFIFO  -- ^ FIFO that holds receive bursts
-- , radio            :: RadioDevice -- ^ the SDR hw object XXX DO WE NEED THIS IN HASKELL????
-- , clock            :: RadioClock  -- ^ the basestation clock
--
-- , underrun         :: Bool        -- ^ indicates writes to SDR are too slow
-- , overrun          :: Bool        -- ^ indicates reads from SDR are too slow
-- , writeTimestamp   :: TimeStamp   -- ^ sample timestamp of next packet written to SDR hw
-- , readTimestamp    :: TimeStamp   -- ^ sample timestamp of next packet read from SDR hw
-- , numARFCNs        :: Int
--
-- , samplesPerSymbol :: Int         -- ^ samples per GSM symbol
-- , receiveOffset    :: Int         -- ^ offset b/w transmit and receive GSM timestamps, in timeslots
-- , radioOn          :: Bool        -- ^ indicates radio is on
--
-- , pushBuffer       :: IO ()       -- ^ push GSM bursts into the transmit buffer (XXX fix type??)
-- , popBuffer        :: IO ()       -- ^ pull GSM bursts from the receive buffer (XXX fix type??)
-- }
