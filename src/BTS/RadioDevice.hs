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

type TimeStamp = Integer -- ^ 64-bit virtual timestamp for radio data

data RadioDevice m = RadioDevice { withRadioDevice       :: Double -> m () -> IO ()  -- ^ bracket pattern wrapper
                                 , setVCTCXO             :: Int -> m ()              -- ^ Set the VCTCXO input voltage
                                 , setTxFreq             :: Double -> Double -> m () -- ^ Set the transmitter frequency
                                 , setRxFreq             :: Double -> Double -> m () -- ^ Set the receiver frequency
                                 --
                                 , setRxGain             :: Double -> Double -> m () -- ^ sets the receive chan gain, returns the gain setting
                                 , setTxGain             :: Double -> Double -> m () -- ^ sets the transmit chan gain, returns the gain setting
                                 , getRxGain             :: m Double                 -- ^ gets the current receive gain
                                 --
                                 , getMaxRxGain          :: Double                   -- ^ return maximum Rx Gain
                                 , getMinRxGain          :: Double                   -- ^ return minimum Rx Gain
                                 , getMaxTxGain          :: Double                   -- ^ return maximum Tx Gain
                                 , getMinTxGain          :: Double                   -- ^ return minimum Tx Gain
                                 -- ** Return internal status values
                                 , getTxFreq             :: Double
                                 , getRxFreq             :: Double
                                 , getSampleRate         :: m Double
                                 , numberRead            :: m Integer
                                 , numberWritten         :: m Integer
                                 -- **
                                 , updateAlignment       :: TimeStamp -> m ()        -- ^ Update the alignment between the read and write timestamps
                                 -- **
                                 , initialWriteTimestamp :: TimeStamp              -- ^ Returns the starting write Timestamp
                                 , initialReadTimestamp  :: TimeStamp              -- ^ Returns the starting read Timestamp
                                 -- **
                                 , fullScaleInputValue   :: Double                 -- ^ returns the full-scale transmit amplitude
                                 , fullScaleOutputValue  :: Double                 -- ^ returns the full-scale receive amplitude
                                 -- ** Read/Write samples from the radio, returns the number of samples actually read/written.
                                 , readSamples  :: TimeStamp -> m ()
                                 , writeSamples :: TimeStamp -> m ()
                                 }

-- ^ buf preallocated buf to contain read result
-- ^ len number of samples desired
-- ^ overrun Set if read buffer has been overrun, e.g. data not being read fast enough
-- ^ timestamp The timestamp of the first samples to be read
-- ^ underrun Set if radio does not have data to transmit, e.g. data not being sent fast enough
-- ^ RSSI The received signal strength of the read result
-- @return The number of samples actually read

--  virtual int readSamples( short *buf
--                         , int len
--                         , bool *overrun
--	  	           , TimeStamp timestamp
--	  	           , bool *underrun
--	  	           , unsigned *RSSI=NULL)=0;

-- ^ buf Contains the data to be written.
-- ^ len number of samples to write.
-- ^ underrun Set if radio does not have data to transmit, e.g. data not being sent fast enough
-- ^ timestamp The timestamp of the first sample of the data buffer.
-- ^ isControl Set if data is a control packet, e.g. a ping command
-- @return The number of samples actually written

--  virtual int writeSamples( short *buf
--                          , int len
--                          , bool *underrun
--		            , TimeStamp timestamp,
--		            , bool isControl=false)=0;
