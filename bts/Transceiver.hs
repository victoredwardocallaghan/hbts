module Transceiver (controlLoop, dataLoop) where

import Control.Monad (unless)
import Network.Socket hiding (recv)
import qualified Data.ByteString.Char8 as C
import Network.Socket.ByteString (recv, sendAll)
import Control.Exception
import Data.List
import Control.Applicative ((<$>), (<*), (*>))
import Text.Parsec

controlPort    = "3000"
datastreamPort = "3001"

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
  putStrLn $ "<DEBUG> " ++ "Sending Response: { " ++ (arfcnCmdResponseLookup (read msg) 0) ++ " }"
  -- XXX we should dispath to the correct ARFCN command function here.
  -- XXX should they all become actions or functions?
  sendstr (sock,addr) $ arfcnCmdResponseLookup (read msg) 0
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



-- | ..
data ARFCNCommand = ARFCNCommandRequest | Int deriving (Show)

--parseS :: String -> Either ParseError [ARFCNCommand]
--parseS input = parse content "" input

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
                         deriving (Eq, Read)

-- | ..
arfcnCmdResponseLookup :: Show a => ARFCNCommandRequest -> a -> String
arfcnCmdResponseLookup c a | c == PowerOn       = "RSP Power 0"
                           | c == PowerOff      = "RSP Power 1"
                           | c == SetRxGain     = "RSP SETRXGAIN 0"
                           | c == SetTxAtten    = "RSP SETTXATTEN 0"
                           | c == SetFreqOffset = "RSP SETFREQOFFSET " ++ show a
                           | c == NoiseLevel    = "RSP NOISELEV "      ++ show a
                           | c == SetPower      = "RSP SETPOWER "      ++ show a
                           | c == AdjustPower   = "RSP ADJUPOWER "     ++ show a
                           | c == RxTune        = "RSP RXTUNE "        ++ show a
                           | c == TxTune        = "RSP TXTUNE "        ++ show a
                           | c == SetTsc        = "RSP SETTSC "        ++ show a
                           | c == Handover      = "RSP HANDOVER "      ++ show a
                           | c == NoHandover    = "RSP NOHANDOVER "    ++ show a
                           | c == SetSlot       = "RSP SETSLOT "       ++ show a
