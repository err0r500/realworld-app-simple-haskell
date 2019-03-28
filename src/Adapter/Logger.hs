{-# LANGUAGE TemplateHaskell #-}

module Adapter.Logger where

import           Control.Exception
import           Katip
import           System.IO         (stdout)

import           ClassyPrelude

test :: KatipContextT IO ()
test = do
    $(logTM) InfoS "Hello from Katip!"
    $(logTM) InfoS "I can do any kind of `IO` action here with `liftIO`!"

internalLogger :: (MonadIO m, Show a) => () -> [a] -> m ()
internalLogger _ elems = do
    handleScribe <- liftIO $ mkHandleScribe ColorIfTerminal stdout InfoS V2
    let mkLogEnv = registerScribe "stdout" handleScribe defaultScribeSettings =<< initLogEnv "MyApp" "production"
    liftIO $
        Control.Exception.bracket mkLogEnv closeScribes $ \le -> runKatipContextT le (mempty :: LogContexts) mempty test

log :: (MonadIO m, Show a) => [a] -> m ()
log _ = do
    handleScribe <- liftIO $ mkHandleScribe ColorIfTerminal stdout InfoS V2
    let mkLogEnv = registerScribe "stdout" handleScribe defaultScribeSettings =<< initLogEnv "MyApp" "production"
    liftIO $
        Control.Exception.bracket mkLogEnv closeScribes $ \le -> runKatipContextT le (mempty :: LogContexts) mempty test
--    scribe <- liftIO $ Katip.mkHandleScribe Katip.ColorIfTerminal stdout Katip.InfoS Katip.V2
--    le <- liftIO $ Katip.initLogEnv "add_context_loop" "test"
--    Katip.runKatipContextT $ le scribe "loop" $ Katip.logFM Katip.ErrorS $ Katip.showLS $ concat $ map tshow elems
