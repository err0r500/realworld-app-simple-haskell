module Storage.Specs.User where

import Data.Text.Array (empty)
import qualified Data.UUID as UUID
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

getUserSpec =
  beforeWith Lib.resetDbAndLogs $ do
    -- all functions are expected to shared the same behavior
    -- todo : check the logs
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
  beforeWith Lib.resetDbAndLogs $ do
    describe "invalid user" $ do
      it "fails on empty name" $ \(repo, conn, logs) -> do
        result <- Lib.run logs $ UC._insertUserPswd repo (user {D._name = D.Name ""}) emptyPassword
        result `shouldBe` Just UC.AnyErr

      it "fails on empty email" $ \(repo, conn, logs) -> do
        result <- Lib.run logs $ UC._insertUserPswd repo (user {D._email = D.Email ""}) emptyPassword
        result `shouldBe` Just UC.AnyErr

-- helpers
dropColumnStmt :: HS.Statement () ()
dropColumnStmt =
  HS.Statement "ALTER TABLE users DROP COLUMN uid" HE.noParams HD.noResult True

restoreColumnStmt :: HS.Statement () ()
restoreColumnStmt =
  HS.Statement "ALTER TABLE users ADD COLUMN uid UUID NOT NULL UNIQUE" HE.noParams HD.noResult True

--spec :: UC.UserRepo Lib.App -> Lib.ResetFunc Lib.App -> Spec
--spec r reset = do
--  let uid = fakeUUID1
--      user = D.User uid (D.Name "matth") (D.Email "matth@example.com")
--      emptyPassword = D.Password ""
--      otherUser = D.User fakeUUID2 (D.Name "other") (D.Email "other@example.com")
--
--  before (resetAndGetLogs) $ do
--    -- Insert User
--    describe "invalid user (empty email)" $
--      it "fails" $ \logs -> do
--        result <- appToIO logs $ UC._insertUserPswd r (otherUser {D._email = D.Email ""}) emptyPassword
--        result `shouldBe` Just UC.AnyErr
--
--    describe "invalid user (empty name)" $
--      it "fails" $ \logs -> do
--        result <- appToIO logs $ UC._insertUserPswd r (otherUser {D._name = D.Name ""}) emptyPassword
--        result `shouldBe` Just UC.AnyErr
--
--    describe "2 different users" $
--      it "succeeds" $ \logs -> do
--        result <- appToIO logs $ do
--          Nothing <- UC._insertUserPswd r user emptyPassword
--          UC._insertUserPswd r otherUser emptyPassword
--        result `shouldBe` Nothing
--
--    describe "2 users with same id" $
--      it "fails" $ \logs -> do
--        result <- appToIO logs $ do
--          Nothing <- UC._insertUserPswd r user emptyPassword
--          UC._insertUserPswd r (otherUser {D._id = uid}) emptyPassword
--        result `shouldBe` Just (UC.SpecificErr UC.InsertUserConflict)
--
--    describe "2 users with same name" $
--      it "fails" $ \logs -> do
--        result <- appToIO logs $ do
--          Nothing <- UC._insertUserPswd r user emptyPassword
--          UC._insertUserPswd r (otherUser {D._name = D._name user}) emptyPassword
--        result `shouldBe` Just (UC.SpecificErr UC.InsertUserConflict)
--
--    describe "2 users with same email" $
--      it "fails" $ \logs -> do
--        result <- appToIO logs $ do
--          Nothing <- UC._insertUserPswd r user emptyPassword
--          UC._insertUserPswd r (otherUser {D._email = D._email user}) emptyPassword
--        result `shouldBe` Just (UC.SpecificErr UC.InsertUserConflict)
