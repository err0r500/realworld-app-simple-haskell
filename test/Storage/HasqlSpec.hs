module Storage.HasqlSpec where

import qualified Adapter.Fake.Logger as Logger
import qualified Adapter.Storage.Hasql.User as Storage
import Control.Exception
import Data.Text (pack)
import Data.Text.Lazy (isInfixOf)
import qualified Domain.User as D
import qualified Hasql.Connection as Connection
import RIO
import qualified Storage.Lib as Lib
import qualified Storage.Specs.User as User
import System.Directory (getCurrentDirectory)
import System.IO
import Test.Hspec
import qualified TestContainers.Hspec as TC
import qualified Usecase.Interactor as UC
import Utils

data TestException = PgConnectionFailed
  deriving (Show, Typeable)

instance Exception TestException

setupPostgres :: TC.MonadDocker m => m Connection.Connection
setupPostgres = do
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
      Connection.acquire $
        Connection.settings
          "localhost"
          (fromIntegral $ TC.containerPort pgContainer 5432)
          "postgres"
          (encodeUtf8 pgPassword)
          "postgres"

  case conn of
    Right c -> pure c
    Left e -> do
      liftIO $ print e
      throwM PgConnectionFailed

reset :: Connection.Connection -> IO (UC.UserRepo Lib.App, Connection.Connection, Lib.Logs)
reset conn = do
  logs <- Lib.emptyLogs
  pure (userRepo conn, conn, logs)
  where
    userRepo :: Connection.Connection -> UC.UserRepo Lib.App
    userRepo c =
      UC.UserRepo
        (Storage.insertUserPswd c)
        (Storage.getUserByID c)
        (Storage.getUserByEmail c)
        (Storage.getUserByName c)
        undefined

spec :: Spec
spec =
  let uid = fakeUUID1
      user = D.User uid (D.Name "matth") (D.Email "matth@example.com")
      emptyPassword = D.Password ""
      otherUser = D.User fakeUUID2 (D.Name "other") (D.Email "other@example.com")
      startPostgresContainer = TC.withContainers setupPostgres
   in aroundAll startPostgresContainer $
        beforeWith reset $
          it "succeeds" $ \(repo, conn, logs) -> do
            Right result <- Lib.run logs $ do
              Nothing <- UC._insertUserPswd repo user emptyPassword
              Nothing <- UC._insertUserPswd repo otherUser emptyPassword
              UC._getUserByID repo uid
            result `shouldBe` Just user
