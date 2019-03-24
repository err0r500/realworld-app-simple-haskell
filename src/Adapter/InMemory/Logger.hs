module Adapter.InMemory.Logger where

import           ClassyPrelude
import qualified Data.Has      as DH

newtype Logs = Logs
    { logs :: [Text]
    }

type Logger r m = (MonadReader r m, MonadIO m, DH.Has (TVar Logs) r)

log' :: (Logger r m, Show a) => [a] -> m ()
log' txts = do
    tvar <- asks DH.getter
    atomically $ do
        state <- readTVar tvar
        writeTVar tvar state {logs = logs state ++ map tshow txts}

getLogs :: Logger r m => m [Text]
getLogs = do
    tvar <- asks DH.getter
    atomically $ do
        state <- readTVar tvar
        return $ logs state
