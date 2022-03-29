module Storage.Lib where

--import Control.Monad.Fail

import qualified Adapter.Fake.Logger as Logger
import qualified Adapter.Storage.Hasql.User as Storage
import Control.Exception
import Data.Text (pack)
import Data.Text.Lazy (isInfixOf)
import qualified Domain.Messages as D
import qualified Domain.User as D
import qualified Hasql.Connection as HConn
import qualified Hasql.Decoders as HD
import qualified Hasql.Encoders as HE
import Hasql.Session
import qualified Hasql.Session as Session
import qualified Hasql.Statement as HS
import qualified Hasql.TH as TH
import RIO
import System.Directory as System
import System.IO as System
import Test.Hspec
import qualified TestContainers.Hspec as TC
import qualified Usecase.Interactor as UC
import Utils

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

data TestException
  = PgConnectionFailed HConn.ConnectionError
  | PgQueryFailed QueryError
  deriving (Show, Typeable)

instance Exception TestException

startAndConnectPostgres :: TC.MonadDocker m => m HConn.Connection
startAndConnectPostgres = do
  let pgPassword = "pg_password"

  currDir <- liftIO getCurrentDirectory

  pgContainer <-
    TC.run $
      TC.containerRequest (TC.fromTag "postgres:14")
        TC.& TC.setExpose [5432]
        TC.& TC.setEnv [("POSTGRES_PASSWORD", pgPassword)]
        TC.& TC.setVolumeMounts [(pack currDir <> "/scripts/pg.sql", "/docker-entrypoint-initdb.d/init.sql")]
        TC.& TC.setWaitingFor (TC.waitForLogLine TC.Stdout ("PostgreSQL init process complete; ready for start up." `isInfixOf`))

  conn <-
    liftIO $
      HConn.acquire $
        HConn.settings
          "localhost"
          (fromIntegral $ TC.containerPort pgContainer 5432)
          "postgres"
          (encodeUtf8 pgPassword)
          "postgres"

  case conn of
    Right c -> pure c
    Left e -> throwM $ PgConnectionFailed e

resetDbAndLogs :: HConn.Connection -> IO (UC.UserRepo App, HConn.Connection, Logs)
resetDbAndLogs conn = do
  logs <- emptyLogs
  truncateTable conn
  pure (userRepo conn, conn, logs)
  where
    userRepo :: HConn.Connection -> UC.UserRepo App
    userRepo c =
      UC.UserRepo
        (Storage.insertUserPswd c)
        (Storage.getUserByID c)
        (Storage.getUserByEmail c)
        (Storage.getUserByName c)
        undefined

truncateTable :: (MonadFail m, MonadIO m) => HConn.Connection -> m ()
truncateTable c = do
  Right res <- liftIO $ Session.run (Session.statement () truncateStmt) c
  pure ()

truncateStmt :: HS.Statement () ()
truncateStmt = HS.Statement "truncate table users" HE.noParams HD.noResult True
