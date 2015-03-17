module Transceiver (constructTransceiver, controlLoop, dataLoop) where

import BTS.GSMTransfer
import BTS.RadioInterface hiding (bracket)

import Network.Socket hiding (recv)
import qualified Data.ByteString.Char8 as C
import Network.Socket.ByteString (recv, sendAll)
import Control.Exception
import Data.List
import Control.Applicative ((<$), (<$>), (<*), (*>))
import Control.Monad (unless, when)

import Pipes
import qualified Pipes.ByteString as P
import qualified Data.ByteString as BS
import Data.ByteString.Char8 (pack)
import Data.Attoparsec.ByteString.Char8
import Data.Foldable

controlPort    = "3000"
datastreamPort = "3001"

constructTransceiver :: RadioInterface  -- ^ RadioInterface
                     -> Int             -- ^ no. of ARFCN's
                     -> Int             -- ^ Samples per symbol
                     -> IO ()
constructTransceiver ri arfcns sps = do
  putStrLn $ "INFO " ++ "Running " ++ show arfcns ++ " ARFCN's"

  -- generate pulse and setup up signal processing library
--  gsmPulse = generateGSMPulse(2,mSamplesPerSymbol);
--  putStrLn $ "DEBUG " ++ "gsmPulse: " ++ show gsmPulse
--  sigProcLibSetup(mSamplesPerSymbol);

--  txFullScale = mRadioInterface->fullScaleInputValue();
--  rxFullScale = mRadioInterface->fullScaleOutputValue();
  fsiv       <- fullScaleInputValue ri
  fsov       <- fullScaleOutputValue ri

  putStrLn "XXX to complete.."


-- radioVector *Transceiver::fixRadioVector(BitVector &burst, int RSSI, GSM::Time &wTime, int ARFCN)
--   -- modulate and stick into queue
--   signalVector* modBurst = modulateBurst(burst,*gsmPulse, 8 + (wTime.TN() % 4 == 0), mSamplesPerSymbol);
--   float headRoom = (mNumARFCNs > 1) ? 0.5 : 1.0;
--   scaleVector(*modBurst,txFullScale * headRoom * pow(10,-RSSI/10)/mNumARFCNs);
--   radioVector *newVec = new radioVector(*modBurst,wTime,ARFCN);
--   return newVec;
fixRadioVector :: BS.ByteString -- ^ burst (BitVector)
               -> Int           -- ^ RSSI
               -> Int           -- ^ XXX fix type Time
               -> Int           -- ^ ARFCN
               -> IO ()         -- ^ XXX fix type signalVector
fixRadioVector burst rssi t arfcn = do
  putStrLn "FIXME"

-- | Drive UDP Control sock (main loop)
controlLoop :: IO ()
controlLoop = withSocketsDo $ bracket (connectMe controlPort) sClose procCtrlMessages

-- | Drive UDP Data down to Tx Pipe (main loop)
dataLoop :: IO ()
dataLoop = withSocketsDo $ bracket (connectMe datastreamPort) sClose procDataStream

connectMe :: String -> IO Socket
connectMe port = do
  let hints = defaultHints
                { addrFamily = AF_INET
                , addrFlags  = [AI_PASSIVE]
                , addrSocketType = Datagram
                }
  ai:_ <- getAddrInfo (Just hints) Nothing (Just port)

  sock <- socket (addrFamily ai) (addrSocketType ai) (addrProtocol ai)
  bindSocket sock (addrAddress ai)
  return sock

-- | Loop forever processing incoming control data.
procCtrlMessages :: Socket -> IO ()
procCtrlMessages sock = do -- Receive one UDP packet, maximum length 1024 bytes,
                           -- and save its content into msg and its source
                           -- IP and port into addr
  (msg, _, addr) <- recvFrom sock 1024
  -- Handle it
  ctrlhandlerfunc (sock, addr) msg
  -- And process more messages
  procCtrlMessages sock

-- | Loop forever processing incoming Tx data.
procDataStream :: Socket -> IO ()
procDataStream sock = do
  (msg, _, addr) <- recvFrom sock 1024
  datahandlerfunc (sock, addr) msg
  procDataStream sock

ctrlhandlerfunc :: (Socket, SockAddr) -> String -> IO ()
ctrlhandlerfunc (sock, addr) msg = do
  putStrLn $ "<DEBUG> " ++ "[ " ++ show addr ++ " ]" ++ " Recieved: { " ++ msg ++ " }"
  let cmd = parseOnly parseARFCNCommand (pack msg)
  case cmd of
    Left e  -> do putStrLn $ "ERR " ++ " badly formed data on control UDP socket"
                  return ()
    Right c -> do putStrLn $ "<DEBUG> " ++ "Sending Response: { " ++ arfcnCmdResponseLookup c ++ " }"
                  sendstr (sock,addr) $ arfcnCmdResponseLookup c
                  arfcnCmdDispatch c
  where
    -- Send until everything is done
    sendstr :: (Socket, SockAddr) -> String -> IO ()
    sendstr (sock,addr) msg = unless (null msg) $ do
                                sent <- sendTo sock msg addr
                                sendstr (sock,addr) (genericDrop sent msg)

datahandlerfunc :: (Socket, SockAddr) -> String -> IO ()
datahandlerfunc (sock, addr) msg = do
  putStrLn $ "<DEBUG> " ++ "[ " ++ show addr ++ " ]" ++ " Recieved: { " ++ msg ++ " }"
  -- XXX finish data hot path into pipe sink
  when (length msg == fromIntegral (gSlotLen+1+4+1)) $ putStrLn $ "ERR " ++ "badly formatted packet on GSM->TRX interface"

  putStrLn $ "DEBUG " ++ "rcvd. burst at: " -- ++ GSM::Time(frameNum,timeSlot) <<LOGVAR(fillerFlag);
--  int RSSI = (int) buffer[5];	// (pat) It is not RSSI, it is transmit gain, always set to 0.
--  let rssi = 0
--  GSM::Time currTime = GSM::Time(frameNum,timeSlot);
--  radioVector *newVec = fixRadioVector(newBurst,RSSI,currTime,ARFCN);
--  OK now we write newVec down into the sink
-- ....
--  putStrLn $ "DEBUG" ++ "added burst - time: " ++ show currTime ++ ", RSSI: " ++ show rssi ++ ", data: " ++ show newBurst

-- | ..
data ARFCNCommand = ARFCNCommand { arfcnCmd  :: ARFCNCommandRequest
                                 , arfcnCmdV :: Double
                                 } deriving (Show)

parseARFCNCommand :: Parser ARFCNCommand
parseARFCNCommand = do
  c <- parseARFCNCommandRequest
  char ' '
  v <- double
  return $ ARFCNCommand c v

-- | ..
data ARFCNCommandRequest = PowerOn
                         | PowerOff
                         | SetRxGain
                         | SetTxAtten
                         | SetFreqOffset
                         | NoiseLevel
                         | SetPower
                         | AdjustPower
                         | RxTune
                         | TxTune
                         | SetTsc
                         | Handover
                         | NoHandover
                         | SetSlot
                         deriving (Eq, Show, Enum, Bounded)

parseARFCNCommandRequest :: Parser ARFCNCommandRequest
parseARFCNCommandRequest = asum $ map touch allCommands
  where touch x = x <$ (string . pack . show) x
        allCommands = [minBound .. maxBound :: ARFCNCommandRequest]

-- | ..
arfcnCmdResponseLookup :: ARFCNCommand -> String
arfcnCmdResponseLookup c = arfcnCmdLookup (arfcnCmd c) (arfcnCmdV c)
  where arfcnCmdLookup c v | c == PowerOn       = "RSP Power 0"
                           | c == PowerOff      = "RSP Power 1"
                           | c == SetRxGain     = "RSP SETRXGAIN 0"
                           | c == SetTxAtten    = "RSP SETTXATTEN 0"
                           | c == SetFreqOffset = "RSP SETFREQOFFSET " ++ show v
                           | c == NoiseLevel    = "RSP NOISELEV "      ++ show v
                           | c == SetPower      = "RSP SETPOWER "      ++ show v
                           | c == AdjustPower   = "RSP ADJUPOWER "     ++ show v
                           | c == RxTune        = "RSP RXTUNE "        ++ show v
                           | c == TxTune        = "RSP TXTUNE "        ++ show v
                           | c == SetTsc        = "RSP SETTSC "        ++ show v
                           | c == Handover      = "RSP HANDOVER "      ++ show v
                           | c == NoHandover    = "RSP NOHANDOVER "    ++ show v
                           | c == SetSlot       = "RSP SETSLOT "       ++ show v


-- | XXX figure out which actions to dispatch to?
arfcnCmdDispatch :: ARFCNCommand -> IO ()
arfcnCmdDispatch c = arfcnCmdDispatcher (arfcnCmd c) (arfcnCmdV c)
  where arfcnCmdDispatcher c v | c == PowerOn       = putStrLn ".."
                               | c == PowerOff      = putStrLn ".."
                               | c == SetRxGain     = putStrLn ".."
                               | c == SetTxAtten    = putStrLn ".."
                               | c == SetFreqOffset = putStrLn ".."
                               | c == NoiseLevel    = putStrLn ".."
                               | c == SetPower      = putStrLn ".."
                               | c == AdjustPower   = putStrLn ".."
                               | c == RxTune        = putStrLn ".."
                               | c == TxTune        = putStrLn ".."
                               | c == SetTsc        = putStrLn ".."
                               | c == Handover      = putStrLn ".."
                               | c == NoHandover    = putStrLn ".."
                               | c == SetSlot       = putStrLn ".."
