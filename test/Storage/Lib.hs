module Storage.Lib where

import           RIO

import           Control.Monad.Fail
import qualified Usecase.Interactor            as UC
import qualified Adapter.Fake.Logger           as Logger


type Logs = TVar Logger.Logs
newtype App a = App (RIO Logs a) deriving (Functor, Applicative, Monad, MonadReader Logs, MonadIO)

instance MonadFail App where
  fail = fail

instance UC.Logger App where
  log = Logger.log

emptyLogs :: IO Logs
emptyLogs = newTVarIO $ Logger.Logs []

run :: Logs -> App a -> IO a
run logs (App app) = runRIO logs app

type ResetFunc m = Monad m => () -> m ()
