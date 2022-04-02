module Storage.Specs.User (getUserSpec, insertUserSpec) where

import Adapter.Fake.Logger (getLogs)
import Data.Text.Array (empty)
import qualified Data.UUID as UUID
import qualified Domain.Messages as D
import qualified Domain.User as D
import qualified Hasql.Decoders as HD
import qualified Hasql.Encoders as HE
import qualified Hasql.Session as Session
import qualified Hasql.Statement as HS
import qualified Hasql.TH as TH
import RIO
import Storage.Lib (truncateTable)
import qualified Storage.Lib as Lib
import Test.Hspec
import Usecase.Interactor (Err (AnyErr))
import qualified Usecase.Interactor as UC
import Utils

-- todo : check the logs
getUserSpec =
  beforeWith Lib.resetDbAndFlushLogs $ do
    -- all the functions below are expected to share the same behavior :
    -- get the user, but from different fields
    describe "getUserByID" $
      runTests (UC._getUserByID, D._id user)
    describe "getUserByName" $
      runTests (UC._getUserByName, D._name user)
    describe "getUserByEmail" $
      runTests (UC._getUserByEmail, D._email user)
  where
    runTests (getUserBy, arg) = do
      it "returns the user if present" $ \(repo, conn, logs) -> do
        Right result <- Lib.run logs $ do
          Nothing <- UC._insertUserPswd repo user emptyPassword
          Nothing <- UC._insertUserPswd repo otherUser emptyPassword
          getUserBy repo arg
        result `shouldBe` Just user

      it "returns nothing if not found" $ \(repo, conn, logs) -> do
        Right result <- Lib.run logs $ do
          Nothing <- UC._insertUserPswd repo otherUser emptyPassword
          getUserBy repo arg
        result `shouldBe` Nothing

      it "returns an error if something goes wrong" $ \(repo, conn, logs) -> do
        Left result <- Lib.run logs $ do
          Nothing <- UC._insertUserPswd repo user emptyPassword
          Right _ <- liftIO $ Session.run (Session.statement () dropColumnStmt) conn
          getUserBy repo arg
        result `shouldBe` AnyErr
        truncateTable conn
        Right _ <- liftIO $ Session.run (Session.statement () restoreColumnStmt) conn
        pure ()

insertUserSpec =
  beforeWith Lib.resetDbAndFlushLogs $ do
    describe "insertUser" $ do
      it "succeeds for a valid user" $ \(repo, conn, logs) -> do
        result <- Lib.run logs $ do
          UC._insertUserPswd repo user (D.Password "password")
        result `shouldBe` Nothing

      it "succeeds for a user with a password" $ \(repo, conn, logs) -> do
        result <- Lib.run logs $ do
          UC._insertUserPswd repo user emptyPassword
        result `shouldBe` Nothing

      it "succeeds on password collision" $ \(repo, conn, logs) -> do
        result <- Lib.run logs $ do
          let password = D.Password "password"
          Nothing <- UC._insertUserPswd repo user password
          UC._insertUserPswd repo otherUser password
        result `shouldBe` Nothing

      it "fails if the user has no name" $ \(repo, conn, logs) -> do
        result <- Lib.run logs $ UC._insertUserPswd repo (user {D._name = D.Name ""}) emptyPassword
        result `shouldBe` Just UC.AnyErr
        ll <- readTVarIO logs
        length ll `shouldBe` 1

      it "fails if the user has no email" $ \(repo, conn, logs) -> do
        result <- Lib.run logs $ UC._insertUserPswd repo (user {D._email = D.Email ""}) emptyPassword
        result `shouldBe` Just UC.AnyErr
        ll <- readTVarIO logs
        length ll `shouldBe` 1

      it "fails with conflict on id collision" $
        \(repo, conn, logs) -> do
          result <- Lib.run logs $ do
            Nothing <- UC._insertUserPswd repo user emptyPassword
            UC._insertUserPswd repo (otherUser {D._id = userUUID}) emptyPassword
          result `shouldBe` Just (UC.SpecificErr UC.InsertUserConflict)
          ll <- readTVarIO logs
          length ll `shouldBe` 1

      it "fails with conflict on name collision" $ \(repo, conn, logs) -> do
        result <- Lib.run logs $ do
          Nothing <- UC._insertUserPswd repo user emptyPassword
          UC._insertUserPswd repo (otherUser {D._name = D._name user}) emptyPassword
        result `shouldBe` Just (UC.SpecificErr UC.InsertUserConflict)
        ll <- readTVarIO logs
        length ll `shouldBe` 1

      it "fails with conflict on email collision" $ \(repo, conn, logs) -> do
        result <- Lib.run logs $ do
          Nothing <- UC._insertUserPswd repo user emptyPassword
          UC._insertUserPswd repo (otherUser {D._email = D._email user}) emptyPassword
        result `shouldBe` Just (UC.SpecificErr UC.InsertUserConflict)
        ll <- readTVarIO logs
        length ll `shouldBe` 1

--shouldContainAnError :: [D.Message a] -> Bool
--shouldContainAnError = any isError
--  where
--    isError m = case m of
--      D.ErrorMsg _ -> True
--      _ -> False

-- helpers
dropColumnStmt :: HS.Statement () ()
dropColumnStmt =
  HS.Statement "ALTER TABLE users DROP COLUMN uid" HE.noParams HD.noResult True

restoreColumnStmt :: HS.Statement () ()
restoreColumnStmt =
  HS.Statement "ALTER TABLE users ADD COLUMN uid UUID NOT NULL UNIQUE" HE.noParams HD.noResult True

emptyPassword = D.Password ""

userUUID = fakeUUID1

user = D.User userUUID (D.Name "userName") (D.Email "user@email.com")

otherUser = D.User fakeUUID2 (D.Name "otherUserName") (D.Email "otherUser@email.com")
