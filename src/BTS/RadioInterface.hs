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

import qualified Data.ByteString as BS

import Pipes

import Control.Monad (unless, forever)
import Control.Exception (try, throwIO)
import qualified GHC.IO.Exception as G

import BTS.GSMCommon
import BTS.RadioDevice
import BTS.RadioDevice.NullDevice

import BTS.Logger

radioDeviceLogger :: String
radioDeviceLogger  = "RadioInterface.hs"

-- | samples per GSM symbol
samplesPerSymbol = 1
inChunk          = 625
outChunk         = 625

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
data RadioInterface = RadioInterface { withRadio            :: IO () -> IO ()            -- ^ start/stop RadioInterface wrapper
                                     , setSamplesPerSymbol  :: Int -> IO ()
                                     , getSamplesPerSymbol  :: IO Int
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
-- ^ return the basestation clock
-- RadioClock* getClock(void) { return &mClock;};
--getClock :: RadioClock

-- ^ drive transmission of GSM bursts
-- void driveTransmitRadio(signalVector &radioBurst, bool zeroBurst);
-- driveReceiveRadio :: IO () -- ^ drive reception of GSM bursts
                                     }


bracket start stop body = do
  start
  body
  stop

withRadioInterface rdevref ronref = bracket (radioInterfaceStart rdevref ronref) (radioInterfaceStop ronref)

-- | constructor
constructRadioInterface :: IO RadioInterface
constructRadioInterface  = do
--  initLogger radioDeviceLogger
  --
  samplesPerSymbolRef <- newIORef (samplesPerSymbol :: Int) -- samples per GSM symbol
  radioOnRef   <- newIORef False
  numARFCNRef  <- newIORef 1     -- XXX make into a Maybe argument with a default value of Just 1
  --
  -- Start with stub radio hw and attach hw later once found!
  radioDeviceRef <- constructNullDevice 0 >>= newIORef

  let ri = RadioInterface { withRadio            = withRadioInterface radioDeviceRef radioOnRef
                          , setSamplesPerSymbol  = modifyIORef samplesPerSymbolRef . const
                          , getSamplesPerSymbol  = readIORef samplesPerSymbolRef
                          , isUnderrun           = undefined -- do flowstate <- readIORef bufferFlowRef ; modifyIORef bufferFlowRef False ; return flowstate
                          , setRxGain            = radioInterfaceSetRxGain radioDeviceRef
                          , setVCTCXO            = radioInterfaceSetVCTCXO radioDeviceRef
                          , tuneTx               = radioInterfaceTuneTx radioDeviceRef
                          , tuneRx               = radioInterfaceTuneRx radioDeviceRef
                          , setPowerAttenuation  = radioInterfaceSetPowerAttenuation radioDeviceRef
                          , fullScaleInputValue  = readIORef radioDeviceRef >>= radioDeviceFullScaleInputValue
                          , fullScaleOutputValue = readIORef radioDeviceRef >>= radioDeviceFullScaleOutputValue
                          , attachRadio          = radioInterfaceAttachRadio radioDeviceRef radioOnRef
                          , getRadioDevice       = readIORef radioDeviceRef
                          }

-- void setSamplesPerSymbol(int wSamplesPerSymbol) {if (!mOn) samplesPerSymbol = wSamplesPerSymbol;}
-- int getSamplesPerSymbol() { return samplesPerSymbol;}
  return ri

-- | start the RadioInterface
radioInterfaceStart :: IORef RadioDevice -> IORef Bool -> IO ()
radioInterfaceStart r p = do
  radio <- readIORef r
  infoM radioDeviceLogger "Starting the radio interface...."
--  writeTimestamp = $ radioDeviceInitialWriteTimestamp radio
--  readTimestamp  = $ radioDeviceInitialReadTimestamp radio
  radioDeviceStart radio
  debugM radioDeviceLogger "Radio started"
--  radioDeviceUpdateAlignment radio (writeTimestamp - 10000)
  modifyIORef p (const True)

-- | stop  the RadioInterface
radioInterfaceStop :: IORef Bool -> IO ()
radioInterfaceStop p = do
  debugM radioDeviceLogger "Radio stopped"
  modifyIORef p (const False)

-- | Modify in-use backend RadioDevice
radioInterfaceAttachRadio :: IORef RadioDevice -> IORef Bool -> RadioDevice -> Int -> IO ()
radioInterfaceAttachRadio radiodevref p r s = do
  radioInterfaceStop p
  modifyIORef radiodevref (const r)
--  modifyIORef radioOversamplingRef s
  radioInterfaceStart radiodevref p

-- | ??
radioInterfaceSetPowerAttenuation :: IORef RadioDevice -> Double -> IO ()
radioInterfaceSetPowerAttenuation r a = do
  radio <- readIORef r
  hwdBAtten <- radioDeviceSetTxGain radio (-a)
  let dBAtten = a -- + hwdBAtten --   dBAtten -= (-HWdBAtten);
  let linearAtten = 10 ** (0.1 * dBAtten)
  let powerScaling | linearAtten < 1.0 = 1
                   | otherwise         = 1 / sqrt linearAtten
  infoM radioDeviceLogger $ "setting HW gain to " ++ show hwdBAtten ++ " and power scaling to " ++ show powerScaling

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

-- | Sink GSM bursts into the transmit pipe line
--
-- type Consumer a = Proxy () a () X
--
-- Upstream | Downstream
--     +---------+
--     |         |
-- () <==       <== ()
--     |         |
-- a  ==>       ==> X
--     |    |    |
--     +----|----+
--          v
--          r
-- radioInterfaceSink :: RadioDevice -> Consumer (BS.ByteString, TimeStamp) IO ()
radioInterfaceSink :: RadioDevice -> Consumer BS.ByteString IO ()
radioInterfaceSink radio = do
--  (bs, ts) <- await
  bs <- await
  let ts = 0
  x <- lift $ try $ do
    debugM loggerName $ "write timestamp: " ++ show ts
    radioDeviceWriteSamples radio bs 0 ts False
  case x of
    -- Gracefully terminate if we got a broken pipe error
    Left e@(G.IOError { G.ioe_type = t}) ->
       lift $ unless (t == G.ResourceVanished) $ throwIO e
    -- Otherwise loop
    Right () -> radioInterfaceSink radio

-- | Source GSM bursts from the receive pipe line
--
-- type Producer b = Proxy X () () b
--
-- Upstream | Downstream
--     +---------+
--     |         |
-- X  <==       <== ()
--     |         |
-- () ==>       ==> b
--     |    |    |
--     +----|----+
--          v
--          r
-- XXX                       FIXME vvv  forever is a bit too long, Rx buffer will timeout
radioInterfaceSource :: RadioDevice -> TimeStamp -> Producer BS.ByteString IO ()
radioInterfaceSource radio ts = forever $ do
  lift $ debugM loggerName $ "read timestamp: " ++ show ts
  (bs, _) <- lift $ radioDeviceReadSamples radio 1000 ts -- 1000 is the sample length
  yield bs

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
-- , receiveOffset    :: Int         -- ^ offset b/w transmit and receive GSM timestamps, in timeslots
-- , radioOn          :: Bool        -- ^ indicates radio is on
-- }
