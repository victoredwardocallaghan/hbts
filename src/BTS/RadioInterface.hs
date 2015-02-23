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

import BTS.GSMCommon
import BTS.RadioDevice

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
readTime rv = time rv

writeTime :: RadioVector -> Time -> RadioVector
writeTime (RadioVector _ arfcn) time = RadioVector time arfcn

-- | ARFCN read and write operators
readARFCN :: RadioVector -> Int
readARFCN rv = arfcn rv

writeARFCN :: RadioVector -> Int -> RadioVector
writeARFCN (RadioVector time _) arfcn = RadioVector time arfcn



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
getClock rc = rclock rc

-- | Set clock
-- void set(const GSM::Time& wTime) { mClock = wTime; signal.broadcast(); }
setClock :: RadioClock -> Time -> RadioClock
setClock (RadioClock _ sig) t = RadioClock t sig

-- | Increment clock
-- void incTN() { mClock.incTN(); signal.broadcast(); }
incClock :: RadioClock -> RadioClock
incClock (RadioClock clk sig) = RadioClock (incTN clk 1) sig


-- | Interface to abstract the software Transceiver with the SDR hardware backend driver
data RadioInterface = RadioInterface { receiveFIFO      :: VectorFIFO  -- ^ FIFO that holds receive bursts
--                                     , radio            :: RadioDevice -- ^ the SDR hw object XXX DO WE NEED THIS IN HASKELL????
                                     , clock            :: RadioClock  -- ^ the basestation clock

                                     , writeTimestamp   :: TimeStamp   -- ^ sample timestamp of next packet written to SDR hw
                                     , readTimestamp    :: TimeStamp   -- ^ sample timestamp of next packet read from SDR hw

                                     , samplesPerSymbol :: Int         -- ^ samples per GSM symbol
                                     , receiveOffset    :: Int         -- ^ offset b/w transmit and receive GSM timestamps, in timeslots
                                     , radioOn          :: Bool        -- ^ indicates radio is on
                                     }
