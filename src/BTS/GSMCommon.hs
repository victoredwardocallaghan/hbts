{-|
  Module      : $Header$
  Copyright   : (c) 2014 Edward O'Callaghan
  License     : GPL-2
  Maintainer  : eocallaghan@alterapraxis.com
  Stability   : provisional
  Portability : portable

  This module covers L1 FEC, L2 and L3 message translation.
-}

module BTS.GSMCommon where

import Data.Maybe
import Data.Tuple


-- | Call states based on GSM 04.08 5 and ITU-T Q.931
data CallState = NullState
               | Paging
               | AnsweredPaging
               | MOCInitiated
               | MOCProceeding
               | MTCConfirmed
               | CallReceived
               | CallPresent
               | ConnectIndication
               | Active
               | DisconnectIndication
               | ReleaseRequest
               | SMSDelivering
               | SMSSubmitting
               | HandoverInbound
               | HandoverProgress
               | HandoverOutbound
               | BusyReject
                deriving (Enum)

instance Show CallState where
  show NullState            = "null"
  show Paging               = "paging"
  show AnsweredPaging       = "answered-paging"
  show MOCInitiated         = "MOC-initiated"
  show MOCProceeding        = "MOC-proceeding"
  show MTCConfirmed         = "MTC-confirmed"
  show CallReceived         = "call-received"
  show CallPresent          = "call-present"
  show ConnectIndication    = "connect-indication"
  show Active               = "active"
  show DisconnectIndication = "disconnect-indication"
  show ReleaseRequest       = "release-request"
  show SMSDelivering        = "SMS-delivery"
  show SMSSubmitting        = "SMS-submission"
  show HandoverInbound      = "HANDOVER Inbound"
  show HandoverProgress     = "HANDOVER Progress"
  show HandoverOutbound     = "HANDOVER Outbound"
  show BusyReject           = "Busy Reject"

-- | GSM 04.08 Table 10.5.118 and GSM 03.40 9.1.2.5
data TypeOfNumber = UnknownTypeOfNumber
                  | InternationalNumber
                  | NationalNumber
                  | NetworkSpecificNumber
                  | ShortCodeNumber
                  | AlphanumericNumber
                  | AbbreviatedNumber
                   deriving (Eq)

instance Enum TypeOfNumber where
  fromEnum = fromJust . flip lookup tyofno
  toEnum   = fromJust . flip lookup (map swap tyofno)

tyofno = [ (UnknownTypeOfNumber,   0)
         , (InternationalNumber,   1)
         , (NationalNumber,        2)
         , (NetworkSpecificNumber, 3)
         , (ShortCodeNumber,       4)
         , (AlphanumericNumber,    5)
         , (AbbreviatedNumber,     6)
         ]

instance Show TypeOfNumber where
  show UnknownTypeOfNumber   = "unknown"
  show InternationalNumber   = "international"
  show NationalNumber        = "national"
  show NetworkSpecificNumber = "network-specific"
  show ShortCodeNumber       = "short code"

-- | GSM 04.08 Table 10.5.118 and GSM 03.40 9.1.2.5
data NumberingPlan = UnknownPlan
                   | E164Plan
                   | X121Plan
                   | F69Plan
                   | NationalPlan
                   | PrivatePlan
                   | ERMESPlan
                    deriving (Eq)

instance Enum NumberingPlan where
  fromEnum = fromJust . flip lookup noplans
  toEnum   = fromJust . flip lookup (map swap noplans)

noplans = [ (UnknownPlan,  0)
          , (E164Plan,     1)
          , (X121Plan,     3)
          , (F69Plan,      4)
          , (NationalPlan, 8)
          , (PrivatePlan,  9)
          , (ERMESPlan,   10)
          ]

instance Show NumberingPlan where
  show UnknownPlan  = "unknown"
  show E164Plan     = "E.164/ISDN"
  show X121Plan     = "X.121/data"
  show F69Plan      = "F.69/Telex"
  show NationalPlan = "national"
  show PrivatePlan  = "private"

-- | Codes for GSM band types, GSM 05.05 2.
data GSMBand = GSM850  -- ^ US cellular
	     | EGSM900 -- ^ extended GSM
	     | DCS1800 -- ^ worldwide DCS band
	     | PCS1900 -- ^ US PCS band
              deriving (Eq)

instance Enum GSMBand where
  fromEnum = fromJust . flip lookup gsmbands
  toEnum   = fromJust . flip lookup (map swap gsmbands)

gsmbands = [ (GSM850,   850)
           , (EGSM900,  900)
           , (DCS1800, 1800)
           , (PCS1900, 1900)
           ]

-- | Codes for logical channel types.
data ChannelType =
                 -- * Non-dedicated control channels.
                   SCHType   -- ^ sync
                 | FCCHType  -- ^ frequency correction
                 | BCCHType  -- ^ broadcast control
                 | CCCHType  -- ^ common control, a combination of several sub-types
                 | RACHType  -- ^ random access
                 | SACCHType -- ^ slow associated control (acutally dedicated, but...)
                 | CBCHType  -- ^ cell broadcast channel
        
                 -- * Dedicated control channels (DCCHs).
                 | SDCCHType -- ^ standalone dedicated control
                 | FACCHType -- ^ fast associated control

                 -- * Traffic channels
                 | TCHFType   -- ^ full-rate traffic
                 | TCHHType   -- ^ half-rate traffic
                 | AnyTCHType -- ^ any TCH type
                 
                 -- * Packet channels for GPRS.
                 | PDTCHCS1Type
                 | PDTCHCS2Type
                 | PDTCHCS3Type
                 | PDTCHCS4Type
                 
                 -- * Packet CHANNEL REQUEST responses
                 --   These are used only as return value from decodeChannelNeeded(), and do not correspond
                 --   to any logical channels.
                 | PSingleBlock1PhaseType
                 | PSingleBlock2PhaseType
                 
                 -- * Special internal channel types.
                 | LoopbackFullType -- ^ loopback testing
                 | LoopbackHalfType -- ^ loopback testing
                 | AnyDCCHType      -- ^ any dedicated control channel
                 | UndefinedCHType  -- ^ undefined

instance Show ChannelType where
  show UndefinedCHType        = "undefined"
  show SCHType                = "SCH"
  show FCCHType               = "FCCH"
  show BCCHType               = "BCCH"
  show RACHType               = "RACH"
  show SDCCHType              = "SDCCH"
  show FACCHType              = "FACCH"
  show CCCHType               = "CCCH"
  show SACCHType              = "SACCH"
  show TCHFType               = "TCH/F"
  show TCHHType               = "TCH/H"
  show AnyTCHType             = "any TCH"
  show LoopbackFullType       = "Loopback Full"
  show LoopbackHalfType       = "Loopback Half"
  show PDTCHCS1Type           = "PDTCHCS1"
  show PDTCHCS2Type           = "PDTCHCS2"
  show PDTCHCS3Type           = "PDTCHCS3"
  show PDTCHCS4Type           = "PDTCHCS4"
  show PSingleBlock1PhaseType = "GPRS_SingleBlock1Phase"
  show PSingleBlock2PhaseType = "GPRS_SingleBlock2Phase"
  show AnyDCCHType            = "any DCCH"

-- | Mobile identity types, GSM 04.08 10.5.1.4
data MobileIDType = NoIDType
                  | IMSIType
                  | IMEIType
                  | IMEISVType
                  | TMSIType
                   deriving (Eq)

instance Enum MobileIDType where
  fromEnum = fromJust . flip lookup mobidty
  toEnum   = fromJust . flip lookup (map swap mobidty)

mobidty = [ (NoIDType,   0)
          , (IMSIType,   1)
          , (IMEIType,   2)
          , (IMEISVType, 3)
          , (TMSIType,   4)
          ]

instance Show MobileIDType where
  show NoIDType   = "None"
  show IMSIType   = "IMSI"
  show IMEIType   = "IMEI"
  show TMSIType   = "TMSI"
  show IMEISVType = "IMEISV"

-- | Type and TDMA offset of a logical channel, from GSM 04.08 10.5.2.5
data TypeAndOffset = TDMA_MISC
                   | TCHF_0
                   -- * ..
                   | TCHH_0
                   | TCHH_1
                   -- * ..
                   | SDCCH_4_0
                   | SDCCH_4_1
                   | SDCCH_4_2
                   | SDCCH_4_3
                   -- * ..
                   | SDCCH_8_0
                   | SDCCH_8_1
                   | SDCCH_8_2
                   | SDCCH_8_3
                   -- * ..
                   | SDCCH_8_4
                   | SDCCH_8_5
                   | SDCCH_8_6
                   | SDCCH_8_7
                   -- Some extra ones for our internal use.
                   | TDMA_BEACON_BCCH
                   | TDMA_BEACON_CCCH
                   | TDMA_BEACON
                   | TDMA_PDTCHF -- ^ packet data traffic logical channel, full speed.
                   | TDMA_PDCH   -- ^ packet data channel, inclusive
                   | TDMA_PACCH  -- ^ packet control channel, shared with data but distinguished in MAC header.
                   | TDMA_PTCCH  -- ^ packet data timing advance logical channel
                   | TDMA_PDIDLE -- ^ Handles the packet channel idle frames.
                    deriving (Eq)

instance Enum TypeAndOffset where
  fromEnum = fromJust . flip lookup tyoffset
  toEnum   = fromJust . flip lookup (map swap tyoffset)

tyoffset = [ (TDMA_MISC, 0)
           , (TCHF_0, 1)
           , (TCHH_0, 2), (TCHH_1, 3)
           , (SDCCH_4_0, 4), (SDCCH_4_1, 5), (SDCCH_4_2, 6), (SDCCH_4_3, 7)
           , (SDCCH_8_0, 8), (SDCCH_8_1, 9), (SDCCH_8_2, 10), (SDCCH_8_3, 11)
           , (SDCCH_8_4, 12), (SDCCH_8_5, 13), (SDCCH_8_6, 14), (SDCCH_8_7, 15)
           , (TDMA_BEACON_BCCH, 253)
           , (TDMA_BEACON_CCCH, 252)
           , (TDMA_BEACON, 255)
           , (TDMA_PDTCHF, 256)
           , (TDMA_PDCH, 257)
           , (TDMA_PACCH, 258)
           , (TDMA_PTCCH, 259)
           , (TDMA_PDIDLE, 260)
           ]

instance Show TypeAndOffset where
  show TDMA_MISC        = "(misc)"
  show TCHF_0           = "TCH/F"
  show TCHH_0           = "TCH/H-0"
  show TCHH_1           = "TCH/H-1"
  show SDCCH_4_0        = "SDCCH/4-0"
  show SDCCH_4_1        = "SDCCH/4-1"
  show SDCCH_4_2        = "SDCCH/4-2"
  show SDCCH_4_3        = "SDCCH/4-3"
  show SDCCH_8_0        = "SDCCH/8-0"
  show SDCCH_8_1        = "SDCCH/8-1"
  show SDCCH_8_2        = "SDCCH/8-2"
  show SDCCH_8_3        = "SDCCH/8-3"
  show SDCCH_8_4        = "SDCCH/8-4"
  show SDCCH_8_5        = "SDCCH/8-5"
  show SDCCH_8_6        = "SDCCH/8-6"
  show SDCCH_8_7        = "SDCCH/8-7"
  show TDMA_BEACON      = "BCH"
  show TDMA_BEACON_BCCH = "BCCH"
  show TDMA_BEACON_CCCH = "CCCH"
  show TDMA_PDCH        = "PDCH"
  show TDMA_PACCH       = "PACCH"
  show TDMA_PTCCH       = "PTCCH"
  show TDMA_PDIDLE      = "PDIDLE"

-- | L3 Protocol Discriminator, GSM 04.08 10.2, GSM 04.07 11.2.3.1.1.
data L3PD = L3GroupCallControlPD
          | L3BroadcastCallControlPD
          | L3PDSS1PD
          | L3CallControlPD
          | L3PDSS2PD
          | L3MobilityManagementPD
          | L3RadioResourcePD
          | L3GPRSMobilityManagementPD
          | L3SMSPD
          | L3GPRSSessionManagementPD
          | L3NonCallSSPD
          | L3LocationPD
          | L3ExtendedPD
          | L3TestProcedurePD
          | L3UndefinedPD
           deriving (Eq)

instance Enum L3PD where
  fromEnum = fromJust . flip lookup l3pd
  toEnum   = fromJust . flip lookup (map swap l3pd)

l3pd = [ (L3GroupCallControlPD,       0x00)
       , (L3BroadcastCallControlPD,   0x01)
       , (L3PDSS1PD,                  0x02)
       , (L3CallControlPD,            0x03)
       , (L3PDSS2PD,                  0x04)
       , (L3MobilityManagementPD,     0x05)
       , (L3RadioResourcePD,          0x06)
       , (L3GPRSMobilityManagementPD, 0x08)
       , (L3SMSPD,                    0x09)
       , (L3GPRSSessionManagementPD,  0x0a)
       , (L3NonCallSSPD,              0x0b)
       , (L3LocationPD,               0x0c)
       , (L3ExtendedPD,               0x0e)
       , (L3TestProcedurePD,          0x0f)
       , (L3UndefinedPD,                -1)
       ]

instance Show L3PD where
  show L3CallControlPD        = "Call Control"
  show L3MobilityManagementPD = "Mobility Management"
  show L3RadioResourcePD      = "Radio Resource"

-- | ..
uplinkOffsetKHz :: GSMBand -> Int
uplinkOffsetKHz b = case b of
    GSM850  -> 45000
    EGSM900 -> 45000
    DCS1800 -> 95000
    PCS1900 -> 80000


-- | XXX ARFCN should not be a Int rather its own type
-- NOTES.. for each band the following inequalities should hold:
-- assert((ARFCN>=128)&&(ARFCN<=251));
-- assert((ARFCN>=975)&&(ARFCN<=1023));
-- assert((ARFCN>=512)&&(ARFCN<=885));
-- assert((ARFCN>=512)&&(ARFCN<=810));
uplinkFreqKHz :: GSMBand -> Int -> Int
uplinkFreqKHz b arfcn = case b of
    GSM850  -> 824200+200*(arfcn-128)
    EGSM900 -> if arfcn <= 124 then 890000+200*arfcn else 890000+200*(arfcn-1024)
    DCS1800 -> 1710200+200*(arfcn-512)
    PCS1900 -> 1850200+200*(arfcn-512)

-- | ??
downlinkFreqKHz :: GSMBand -- ^ GSM band
                -> Int     -- ^ ARFCN
                -> Int
downlinkFreqKHz band arfcn = uplinkFreqKHz band arfcn + uplinkOffsetKHz band

-- | Modulus operations for frame numbers.
--   The GSM hyperframe is largest time period in the GSM system, GSM 05.02 4.3.3.
--   It is 2715648
hyperframe = 2048 * 26 * 51

-- | Get a clock difference, within the modulus, v1-v2.
fnDelta :: Int -> Int -> Int
fnDelta u v =
    if delta >= halfModulus
    then delta - hyperframe
    else delta + hyperframe
  where
    delta = u - v
    halfModulus = hyperframe `div` 2

-- | Compare two frame clock values.
--   @return 1 if v1>v2, -1 if v1<v2, 0 if v1==v2
fnCompare :: Int -> Int -> Int
fnCompare u v | fnDelta u v > 0 = 1
              | fnDelta u v < 0 = -1
              | otherwise       = 0


-- | GSM frame clock value. GSM 05.02 4.3
-- No internal thread sync.
data Time = Time { fn :: Int -- ^ frame number in the hyperframe
                 , tn :: Int -- ^ timeslot number
                 } deriving (Eq)

instance Ord Time where
    t < t'  | fn t == fn t' = tn t < tn t'
            | otherwise     = fnCompare (fn t) (fn t') < 0
                             
    t > t'  | fn t == fn t' = tn t > tn t'
            | otherwise     = fnCompare (fn t) (fn t') > 0

    t <= t' | fn t == fn t' = tn t <= tn t'
            | otherwise     = fnCompare (fn t) (fn t') <= 0
                            
    t >= t' | fn t == fn t' = tn t >= tn t'
            | otherwise     = fnCompare (fn t) (fn t') >= 0

instance Show Time where
  show t = show (tn t) ++ ":" ++ show (fn t)

-- | Arithmetic.

inc :: Time -> Time
inc (Time fn tn) = Time ((fn+1) `mod` hyperframe) tn

-- ..
-- assert(step<=8);
decTN :: Time
      -> Int  -- ^ step
      -> Time
decTN (Time fn tn) s = Time fn' tn'
    where
    (fn', tn')
        | tnx >= 0  = (fn, tnx)
        | otherwise = (fnx + (if fnx < 0 then hyperframe else 0), tnx + 8)
    tnx = tn - s
    fnx = fn - 1

-- ..
-- assert(step<=8);
incTN :: Time
      -> Int  -- ^ step
      -> Time
incTN (Time fn tn) s = Time fn' tn'
    where
    (fn', tn')
        | tnx <= 7  = (fn, tnx)
        | otherwise = (fnx `mod` hyperframe, tnx - 8)
    tnx = tn + s
    fnx = fn + 1

-- | Standard derivations.
sfn, t1, t2, t3, t3p, tc, t1p, t1r :: Time -> Int

-- GSM 05.02 3.3.2.2.1
sfn t = fn t `div` (26*51)

-- GSM 05.02 3.3.2.2.1
t1 t = sfn t `mod` 2048

-- GSM 05.02 3.3.2.2.1
t2 t = fn t `mod` 26

-- GSM 05.02 3.3.2.2.1
t3 t = fn t `mod` 51

-- GSM 05.02 3.3.2.2.1.
t3p t = (t3 t - 1) `div` 10

-- GSM 05.02 6.3.1.3.
tc t = (fn t `div` 51) `mod` 8

-- GSM 04.08 10.5.2.30.
t1p t = sfn t `mod` 32

-- GSM 05.02 6.2.3
t1r t = t1 t `mod` 64
