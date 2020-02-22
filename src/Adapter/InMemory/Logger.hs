module Adapter.InMemory.Logger where

import           ClassyPrelude
import qualified Data.Has                      as DH
import qualified Adapter.Logger   as Adapter

newtype Logs = Logs
    { logs :: [Text]
    }

type Logger r m = (MonadReader r m, MonadIO m, DH.Has (TVar Logs) r)

log :: (Logger r m, MonadIO m, Adapter.Loggable a) => [a] -> m ()
log elemsToLog = do
        tvar <- asks DH.getter
        atomically $ do
                state <- readTVar tvar
                writeTVar
                        tvar
                        state { logs = logs state ++ map Adapter.show' elemsToLog}

getLogs :: Logger r m => m [Text]
getLogs = do
        tvar <- asks DH.getter
        atomically $ do
                state <- readTVar tvar
                pure $ logs state
