{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE IncoherentInstances #-}

module Adapter.Logger where

import           Control.Exception
import           Katip

import           ClassyPrelude
import qualified Domain.Messages               as D
import qualified Domain.User                   as D

class Show a => Loggable a where
    type R a
    log' :: a -> R (IO ())
    show' :: a -> Text

instance Show a => Loggable a where
        type R a = KatipContextT IO ()
        log' mess = $(logTM) ErrorS (showLS mess)
        show' = tshow

instance Loggable D.Error where
        type R D.Error = KatipContextT IO ()
        log' err = $(logTM) ErrorS (showLS err)
        show' = tshow

instance Show a => Loggable ( D.Message a ) where
        type R (D.Message a) = KatipContextT IO ()
        log' (D.ErrorMsg   mess) = $(logTM) ErrorS (showLS $ show mess)
        log' (D.WarningMsg mess) = $(logTM) WarningS (showLS $ show mess)
        log' (D.InfoMsg    mess) = $(logTM) InfoS (showLS $ show mess)
        log' (D.DebugMsg   mess) = $(logTM) DebugS (showLS $ show mess)
        show' = tshow 


logsFormatting :: (Loggable a) => [a] -> KatipContextT IO ()
logsFormatting = mapM_ log'

log :: (MonadIO m, Loggable a) => [a] -> m ()
log elemsToLog = do
        handleScribe <- liftIO
                $ mkHandleScribe ColorIfTerminal stdout (permitItem DebugS) V2
        let mkLogEnv =
                    registerScribe "stdout" handleScribe defaultScribeSettings
                            =<< initLogEnv "MyApp" "production"
        liftIO
                $ Control.Exception.bracket mkLogEnv closeScribes
                $ \le -> runKatipContextT le () mempty
                          $ logsFormatting elemsToLog
