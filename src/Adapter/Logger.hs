{-# LANGUAGE TemplateHaskell #-}

module Adapter.Logger where

import           Control.Exception
import           Katip
import           System.IO         (stdout)

import           ClassyPrelude

logsFormatting :: Show a => [a] -> KatipContextT IO ()
logsFormatting = mapM_ $ $(logTM) ErrorS . showLS

log :: (MonadIO m, Show a) => [a] -> m ()
log elemsToLog = do
    handleScribe <- liftIO $ mkHandleScribe ColorIfTerminal stdout InfoS V2
    let mkLogEnv = registerScribe "stdout" handleScribe defaultScribeSettings =<< initLogEnv "MyApp" "production"
    liftIO $ Control.Exception.bracket mkLogEnv closeScribes $ \le -> runKatipContextT le () mempty $ logsFormatting elemsToLog
