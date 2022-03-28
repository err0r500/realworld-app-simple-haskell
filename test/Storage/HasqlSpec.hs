{-# LANGUAGE RecordWildCards #-}

module Storage.HasqlSpec where

import qualified Adapter.Fake.Logger as Logger
import qualified Adapter.Storage.Hasql.User as Storage
import Data.Text.Lazy (isInfixOf)
import qualified Domain.User as D
import qualified Hasql.Connection as Connection
import RIO
import qualified Storage.Lib as Lib
import qualified Storage.Specs.User as User
import System.IO
import Test.Hspec
import qualified TestContainers.Hspec as TC
import qualified Usecase.Interactor as UC
import Utils

type PostgresPort = Int

pgPassword :: Text
pgPassword = "pg_password"

setupPostgres :: TC.MonadDocker m => m PostgresPort
setupPostgres = do
  postgresql <-
    TC.run $
      TC.containerRequest (TC.fromTag "postgres:14")
        TC.& TC.setExpose [5432]
        TC.& TC.setEnv [("POSTGRES_PASSWORD", pgPassword)]
        TC.& TC.setVolumeMounts [("/home/matth/projects/perso/realworld-app-simple-haskell/scripts/pg.sql", "/docker-entrypoint-initdb.d/init.sql")]
        TC.& TC.setWaitingFor (TC.waitForLogLine TC.Stdout ("database system is ready to accept connections" `isInfixOf`))

  pure $ TC.containerPort postgresql 5432

connSettings :: PostgresPort -> Connection.Settings
connSettings port = Connection.settings "localhost" 49499 "postgres" "pg_password" "postgres"

resetFunc :: Connection.Connection -> Lib.ResetFunc Lib.App
resetFunc = Storage.truncateTable

userRW :: Connection.Connection -> UC.UserRepo Lib.App
userRW c =
  UC.UserRepo
    (Storage.insertUserPswd c)
    (Storage.getUserByID c)
    (Storage.getUserByEmail c)
    (Storage.getUserByName c)
    undefined

appToIO :: Lib.Logs -> Lib.App a -> IO a
appToIO logs x = liftIO $ Lib.run logs x

resetAndGetLogs :: IO Lib.Logs
resetAndGetLogs = do
  logs <- Lib.emptyLogs
  -- appToIO logs $ reset ()
  return logs

reset :: PostgresPort -> IO (UC.UserRepo Lib.App, Connection.Connection, Lib.Logs)
reset pgIp = do
  Right conn <- liftIO $ Connection.acquire $ connSettings pgIp
  logs <- resetAndGetLogs
  pure (userRW conn, conn, logs)

startPostgresContainer = TC.withContainers setupPostgres

spec :: Spec
spec =
  let uid = fakeUUID1
      user = D.User uid (D.Name "matth") (D.Email "matth@example.com")
      emptyPassword = D.Password ""
      otherUser = D.User fakeUUID2 (D.Name "other") (D.Email "other@example.com")
   in aroundAll startPostgresContainer $
        beforeWith reset $ do
          it "succeeds" $ \(repo, conn, logs) -> do
            Right result <- appToIO logs $ do
              Nothing <- UC._insertUserPswd repo user emptyPassword
              Nothing <- UC._insertUserPswd repo otherUser emptyPassword
              UC._getUserByID repo uid
            result `shouldBe` Just user

--spec :: Spec
--spec =
--  aroundAll ar $ do
--    let x = 3
--    beforeWith beforeWithFn $ do
--      it "first test" $ \(a, b) ->
--        do
--          putStrLn $ show $ a + x
--          putStrLn b
--          2 `shouldBe` 2
--

--do
--  Right result <- appToIO logs $ do
--    Nothing <- UC._insertUserPswd r user emptyPassword
--    Nothing <- UC._insertUserPswd r otherUser emptyPassword
--    UC._getUserByID r uid

--do
--  r <- getRepo pgIp
--  Nothing <- UC._insertUserPswd r user emptyPassword
--  () `shouldNotBe` ()

--spec :: Spec
--spec =
--  around (TC.withContainers setupPostgres) $
--    describe "User" $
--    describe "find user by ID" $
--      it "succeeds" $ \dbIP -> do
--        Right result <- appToIO logs $ do
--          Nothing <- UC._insertUserPswd r user emptyPassword
--          Nothing <- UC._insertUserPswd r otherUser emptyPassword
--          UC._getUserByID r uid
--        result `shouldBe` Just user
--c <- runIO $ do
--  Right conn <- Connection.acquire $ connSettings (TC.containerIp container)
--  pure conn
--      User.spec2 -- (userRW c) (resetFunc c)

--do
--  -- \pgIP ->
--  c <- runIO $ do
--    Right conn <- Connection.acquire $ connSettings "hello"
--    pure conn
--  User.spec (userRW c) (resetFunc c)
