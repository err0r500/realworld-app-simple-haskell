module Adapter.Fake.Logger where

import qualified Adapter.Logger                as Adapter
import qualified Data.Has                      as Has
import           RIO

newtype Logs = Logs
    { logs :: [Text]
    }

type Logger r m = (MonadReader r m, MonadIO m, Has.Has (TVar Logs) r)

log :: (Logger r m, MonadIO m, Adapter.Loggable a) => [a] -> m ()
log elemsToLog = do
  tvar <- asks Has.getter
  atomically $ do
    state <- readTVar tvar
    writeTVar tvar state { logs = logs state ++ map Adapter.show' elemsToLog }

getLogs :: Logger r m => m [Text]
getLogs = do
  tvar <- asks Has.getter
  atomically $ do
    state <- readTVar tvar
    pure $ logs state
