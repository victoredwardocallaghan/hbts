{-|
  Module      : $Header$
  Copyright   : (c) 2014 Edward O'Callaghan
  License     : GPL-2
  Maintainer  : eocallaghan@alterapraxis.com
  Stability   : provisional
  Portability : portable

  ...
-}

module BTS.Logger ( initLogger
-- XXX FIXME make this interface work properly and remove this..
                  , module System.Log.Logger
                  ) where

import System.IO (stderr, Handle)
import System.Log.Logger
import System.Log.Handler        (setFormatter, close)
import System.Log.Handler.Simple (fileHandler, streamHandler, GenericHandler)
import System.Log.Formatter

withFormatter :: GenericHandler Handle -> GenericHandler Handle
withFormatter handler = setFormatter handler formatter
  where formatter = simpleLogFormatter "[$time : $loggername : $prio] $msg"

-- | ..
initLogger :: String -> IO ()
initLogger lname = do
  let logPath = "debug.log"

  -- let loggerName = RootLoggerName
  let loggerName = lname

  logFileHandler <- fileHandler logPath DEBUG
  -- logStreamHandler <- liftIO $ streamHandler stderr INFO
  let logFileHandler' = withFormatter logFileHandler
  -- let logStreamHandler' = withFormatter logStreamHandler

  updateGlobalLogger loggerName (setLevel INFO)
  -- liftIO $ updateGlobalLogger loggerName (setHandlers [logFileHandler', logStreamHandler'])
  updateGlobalLogger loggerName (setHandlers [logFileHandler'])
