{-|
  Module      : $Header$
  Copyright   : (c) 2014 Edward O'Callaghan
  License     : GPL-2
  Maintainer  : eocallaghan@alterapraxis.com
  Stability   : provisional
  Portability : portable

  ...
-}

module BTS.RadioDevice where

import BTS.GSMCommon

import qualified Data.ByteString as BS

type TimeStamp = Integer -- ^ 64-bit virtual timestamp for radio data

data RadioDevice = RadioDevice { withRadioDevice        :: IO () -> IO ()            -- ^ bracket pattern wrapper
                               , radioDeviceSetVCTCXO   :: Int -> IO ()              -- ^ Set the VCTCXO input voltage
                               , radioDeviceSetTxFreq   :: Double -> Double -> IO () -- ^ Set the transmitter frequency
                               , radioDeviceSetRxFreq   :: Double -> Double -> IO () -- ^ Set the receiver frequency
                               -- | ..
                               , radioDeviceSetRxGain   :: Double -> IO ()           -- ^ sets the receive chan gain, returns the gain setting
                               , radioDeviceSetTxGain   :: Double -> IO ()           -- ^ sets the transmit chan gain, returns the gain setting
                               , radioDeviceGetRxGain   :: IO Double                 -- ^ gets the current receive gain
                               -- | ..
                               , radioDeviceGetMaxRxGain :: Double                   -- ^ return maximum Rx Gain
                               , radioDeviceGetMinRxGain :: Double                   -- ^ return minimum Rx Gain
                               , radioDeviceGetMaxTxGain :: Double                   -- ^ return maximum Tx Gain
                               , radioDeviceGetMinTxGain :: Double                   -- ^ return minimum Tx Gain
                               -- | Return internal status values
                               , radioDeviceGetTxFreq     :: Double
                               , radioDeviceGetRxFreq     :: Double
                               , radioDeviceGetSampleRate :: IO Double
                               , radioDeviceNumberRead    :: IO Integer
                               , radioDeviceNumberWritten :: IO Integer
                               -- | ..
                               , radioDeviceUpdateAlignment :: TimeStamp -> IO ()        -- ^ Update the alignment between the read and write timestamps
                               -- | ..
                               , radioDeviceInitialWriteTimestamp :: TimeStamp              -- ^ Returns the starting write Timestamp
                               , radioDeviceInitialReadTimestamp  :: TimeStamp              -- ^ Returns the starting read Timestamp
                               -- | ..
                               , radioDeviceFullScaleInputValue   :: IO Double              -- ^ returns the full-scale transmit amplitude
                               , radioDeviceFullScaleOutputValue  :: IO Double              -- ^ returns the full-scale receive amplitude
                               -- | Read/Write samples from the radio, returns the number of samples actually read/written.
                               , radioDeviceReadSamples  :: Int           -- ^ len number of samples desired to read.
                                                         -> TimeStamp     -- ^ The timestamp of the first samples to be read.
                                                         -> IO (BS.ByteString, Int, SamplesIO) -- ^ (buffer contains read result, RSSI The received signal strength of the read result, SamplesIO state)
                               , radioDeviceWriteSamples :: BS.ByteString -- ^ buf Contains the data to be written.
                                                         -> Int           -- ^ len number of samples to write.
                                                         -> TimeStamp     -- ^ timestamp The timestamp of the first sample of the data buffer.
                                                         -> Bool          -- ^ isControl Set if data is a control packet, e.g. a ping command
                                                         -> IO SamplesIO
                               }

-- | ..
data BufferFlowState = Underflow -- ^ Set if radio does not have data to transmit, e.g. data not being sent fast enough
                     | Overflow
                     | Normal

-- | ..
data SamplesIO = SamplesIO { bufferFlowState :: BufferFlowState -- ^ overrun Set if read buffer has been overrun, e.g. data not being read fast enough
                           , actualLength    :: Int             -- ^ The number of samples actually read/written
                           }
