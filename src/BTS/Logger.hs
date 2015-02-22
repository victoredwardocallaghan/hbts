{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module BTS.Logger ( debugBladeRF
                  , infoBladeRF
                  , noticeBladeRF
                  , warningBladeRF
                  , errorBladeRF
                  , criticalBladeRF
                  , alertBladeRF
                  , emergencyBladeRF
-- XXX FIXME make this interface work properly and remove this..
                  , initLogger
                  ) where

import Control.Applicative (Applicative(..), (<$>), liftA)
import Control.Monad (ap)
import Control.Monad.Trans
import Control.Monad.Trans.State
import Control.Monad.Trans.Except
import Control.Monad.IO.Class

import System.IO (stderr, Handle)

import System.Log.Logger
import System.Log.Handler        (setFormatter, close)
import System.Log.Handler.Simple (fileHandler, streamHandler, GenericHandler)
import System.Log.Formatter

import LibBladeRF.LibBladeRF


--
--errorBladeRF, criticalBladeRF, alertBladeRF, emergencyBladeRF,
--  warningBladeRF, noticeBladeRF, infoBladeRF, debugBladeRF :: MonadIO m => String -> String -> m ()

--debugBladeRF m = liftIO $ debugM (sel s2) m
debugBladeRF l m = liftIO $ debugM l m
infoBladeRF l m = liftIO $ infoM l m
noticeBladeRF l m = liftIO $ noticeM l m
warningBladeRF l m = liftIO $ warningM l m
errorBladeRF l m = liftIO $ errorM l m 
criticalBladeRF l m = liftIO $ criticalM l m
alertBladeRF l m = liftIO $ alertM l m
emergencyBladeRF l m = liftIO $ emergencyM l m

--
-- | Internal logger state.
data BTSLog = BTSLog { fileH :: GenericHandler Handle
                     , logN  :: String
                     }
--
-- | BTS Logger StateT Monad
newtype BTSLogger a = BTSLogger { unBTSLogger :: StateT BTSLog IO a }
                    deriving (Monad, MonadIO)

instance Functor BTSLogger where
  {-# INLINE fmap #-}
  fmap f m = BTSLogger (f <$> unBTSLogger m)

instance Applicative BTSLogger where
  {-# INLINE pure #-}
  pure  = return
  {-# INLINE (<*>) #-}
  (<*>) = ap

-- Type'ed state transistion structure
type Selector a = (BTSLogger a, a -> BTSLogger ())

s1 :: Selector (GenericHandler Handle)
-- s1 = (gets fileH, \x -> modify (\vs -> vs {fileH = x}))
s1 = (BTSLogger (gets fileH), \x -> BTSLogger (modify (\vs -> vs {fileH = x})))
 
s2 :: Selector String
s2 = (BTSLogger (gets logN), \x -> BTSLogger (modify (\vs -> vs {logN = x})))

sel :: Selector a -> BTSLogger a
sel = fst

--mods :: (t, a -> b) -> (t -> a) -> b
--mods (gf,uf) mfun = liftA uf mfun gf
mods :: Selector a -> (a -> a) -> BTSLogger ()
mods (gf,uf) mfun = do st <- gf
                       uf (mfun st)

-- | Call at invocation to setup diagnostic framework
-- withBTSLogger :: String -> BladeRF () -> BladeRF (BTSLog -> IO ())
withBTSLogger lname action = BladeRF $ liftIO $ return $ evalStateT $ bracket (openBTSLogger lname) closeBTSLogger action
--withBTSLogger :: String -> IO a -> BTSLog -> IO a
--withBTSLogger lname action = evalStateT $ bracket (openBTSLogger lname) closeBTSLogger action

--
-- | Wraps up the IO action inside the BTSLogger monadic structure
--bracket :: BTSLogger a -> BTSLogger a1 -> IO b -> StateT BTSLog IO b
bracket open close action = do
  unBTSLogger open
  liftIO $ return $ action
  unBTSLogger close

--
-- XXX FIXME make this interface work properly and remove this..
initLogger :: MonadIO m => String -> m ()
initLogger lname = do
  let logPath = "debug.log"

  -- let loggerName = RootLoggerName
  let loggerName = lname

  logFileHandler <- liftIO $ fileHandler logPath DEBUG
  -- logStreamHandler <- liftIO $ streamHandler stderr INFO
  let logFileHandler' = withFormatter logFileHandler
  -- let logStreamHandler' = withFormatter logStreamHandler

  liftIO $ updateGlobalLogger loggerName (setLevel INFO)
  -- liftIO $ updateGlobalLogger loggerName (setHandlers [logFileHandler', logStreamHandler'])
  liftIO $ updateGlobalLogger loggerName (setHandlers [logFileHandler'])
--
-- XXX


withFormatter :: GenericHandler Handle -> GenericHandler Handle
withFormatter handler = setFormatter handler formatter
  where formatter = simpleLogFormatter "[$time : $loggername : $prio] $msg"

--
-- | ..
openBTSLogger :: String -> BTSLogger ()
openBTSLogger lname = do
  let logPath = "debug.log"

  -- let loggerName = RootLoggerName
  let loggerName = lname

  logFileHandler <- liftIO $ fileHandler logPath DEBUG
  mods s1 (const logFileHandler)
  -- logStreamHandler <- liftIO $ streamHandler stderr INFO
  let logFileHandler' = withFormatter logFileHandler
  -- let logStreamHandler' = withFormatter logStreamHandler

  liftIO $ updateGlobalLogger loggerName (setLevel INFO)
  -- liftIO $ updateGlobalLogger loggerName (setHandlers [logFileHandler', logStreamHandler'])
  liftIO $ updateGlobalLogger loggerName (setHandlers [logFileHandler'])

--
-- | ..
closeBTSLogger :: BTSLogger ()
closeBTSLogger = sel s1 >>= liftIO . close
--closeBTSLogger = liftA close $ sel s1
