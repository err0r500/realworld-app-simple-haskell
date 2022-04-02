module Adapter.Fake.Logger where

import qualified Adapter.Logger as Adapter
import qualified Data.Has as DH
import RIO

type Logs = [Text]

type Logger r m = (MonadReader r m, MonadIO m, DH.Has (TVar Logs) r)

log :: (Logger r m, Adapter.Loggable a) => [a] -> m ()
log elemsToLog = do
  tvar <- asks DH.getter
  atomically $ do
    state <- readTVar tvar
    writeTVar tvar $ state ++ map Adapter.show' elemsToLog

getLogs :: Logger r m => m Logs
getLogs = do
  tvar <- asks DH.getter
  readTVarIO tvar
