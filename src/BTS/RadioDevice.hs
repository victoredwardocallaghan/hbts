{-|
  Module      : $Header$
  Copyright   : (c) 2014 Edward O'Callaghan
  License     : GPL-2
  Maintainer  : eocallaghan@alterapraxis.com
  Stability   : provisional
  Portability : portable

  ...
-}

{-# LANGUAGE ExistentialQuantification #-}
module BTS.RadioDevice where

import BTS.GSMCommon

import Control.Monad.State
import qualified Data.ByteString as BS

type TimeStamp = Integer -- ^ 64-bit virtual timestamp for radio data

type RadioState s a = StateT s IO a --note: could use newtype here. using type alias for brevity

-- | Existential type
-- "for all RadioDevice's 'd' we have the Radio type"
data Radio = forall d. RadioDevice d => Radio d

class RadioDevice d where
  radioDeviceStart       :: RadioState d ()
  radioDeviceStop        :: RadioState d ()
  radioDeviceSetVCTCXO   :: Int -> RadioState d ()              -- ^ Set the VCTCXO input voltage
  radioDeviceSetTxFreq   :: Double -> Double -> RadioState d () -- ^ Set the transmitter frequency
  radioDeviceSetRxFreq   :: Double -> Double -> RadioState d () -- ^ Set the receiver frequency
  -- | ..
  radioDeviceSetRxGain   :: Double -> RadioState d ()           -- ^ sets the receive chan gain, returns the gain setting
  radioDeviceSetTxGain   :: Double -> RadioState d ()           -- ^ sets the transmit chan gain, returns the gain setting
  radioDeviceGetRxGain   :: RadioState d Double                 -- ^ gets the current receive gain
  -- | ..
  radioDeviceGetMaxRxGain :: RadioState d Double                -- ^ return maximum Rx Gain
  radioDeviceGetMinRxGain :: RadioState d Double                -- ^ return minimum Rx Gain
  radioDeviceGetMaxTxGain :: RadioState d Double                -- ^ return maximum Tx Gain
  radioDeviceGetMinTxGain :: RadioState d Double                -- ^ return minimum Tx Gain
  -- | Return internal status values
  radioDeviceGetTxFreq     :: RadioState d Double
  radioDeviceGetRxFreq     :: RadioState d Double
  radioDeviceGetSampleRate :: RadioState d Double
  radioDeviceNumberRead    :: RadioState d Integer
  radioDeviceNumberWritten :: RadioState d Integer
  -- | ..
  radioDeviceUpdateAlignment :: TimeStamp -> RadioState d ()    -- ^ Update the alignment between the read and write timestamps
  -- | ..
  radioDeviceInitialWriteTimestamp :: RadioState d TimeStamp    -- ^ Returns the starting write Timestamp
  radioDeviceInitialReadTimestamp  :: RadioState d TimeStamp    -- ^ Returns the starting read Timestamp
  -- | ..
  radioDeviceFullScaleInputValue   :: RadioState d Double       -- ^ returns the full-scale transmit amplitude
  radioDeviceFullScaleOutputValue  :: RadioState d Double       -- ^ returns the full-scale receive amplitude
  -- | Read/Write samples from the radio, returns the number of samples actually read/written.
  radioDeviceReadSamples  :: Int                                -- ^ len number of samples desired to read.
                          -> TimeStamp                          -- ^ The timestamp of the first samples to be read.
                          -> RadioState d (BS.ByteString, Int)  -- ^ (buffer contains read result, RSSI The received signal strength of the read result, SamplesIO state)
--                          -> IO (BS.ByteString, Int, SamplesIO) -- ^ (buffer contains read result, RSSI The received signal strength of the read result, SamplesIO state)
  radioDeviceWriteSamples :: BS.ByteString                      -- ^ buf Contains the data to be written.
                          -> Int                                -- ^ len number of samples to write.
                          -> TimeStamp                          -- ^ timestamp The timestamp of the first sample of the data buffer.
                          -> Bool                               -- ^ isControl Set if data is a control packet, e.g. a ping command
                          -> RadioState d ()
--                          -> IO SamplesIO


-- | ..
data BufferFlowState = Underflow -- ^ Set if radio does not have data to transmit, e.g. data not being sent fast enough
                     | Overflow
                     | Normal

-- | ..
--data SamplesIO = SamplesIO { bufferFlowState :: BufferFlowState -- ^ overrun Set if read buffer has been overrun, e.g. data not being read fast enough
--                           , actualLength    :: Int             -- ^ The number of samples actually read/written
--                           }
