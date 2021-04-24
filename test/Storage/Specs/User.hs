module Storage.Specs.User where

import qualified Data.UUID as UUID
import qualified Domain.User as D
import RIO
import qualified Storage.Lib as Lib
import Test.Hspec
import qualified Usecase.Interactor as UC
import Utils

appToIO :: Lib.Logs -> Lib.App a -> IO a
appToIO logs x = liftIO $ Lib.run logs x

resetAndGetLogs :: Lib.ResetFunc Lib.App -> IO Lib.Logs
resetAndGetLogs reset = do
  logs <- Lib.emptyLogs
  appToIO logs $ reset ()
  return logs

spec :: UC.UserRepo Lib.App -> Lib.ResetFunc Lib.App -> Spec
spec r reset = do
  let uid = fakeUUID1
      user = D.User uid "matth" "matth@example.com"
      otherUser = D.User fakeUUID2 "other" "other@example.com"

  before (resetAndGetLogs reset) $ do
    -- Get User
    describe "find user by ID" $
      it "succeeds" $ \logs -> do
        Right result <- appToIO logs $ do
          Nothing <- UC._insertUserPswd r user ""
          Nothing <- UC._insertUserPswd r otherUser ""
          UC._getUserByID r uid
        result `shouldBe` Just user

    describe "find user by email" $
      it "succeeds" $ \logs -> do
        Right result <- appToIO logs $ do
          Nothing <- UC._insertUserPswd r user ""
          Nothing <- UC._insertUserPswd r otherUser ""
          UC._getUserByEmail r (D._email user)
        result `shouldBe` Just user

    describe "find user by name" $
      it "succeeds" $ \logs -> do
        Right result <- appToIO logs $ do
          Nothing <- UC._insertUserPswd r user ""
          Nothing <- UC._insertUserPswd r otherUser ""
          UC._getUserByName r (D._name user)
        result `shouldBe` Just user

    -- Insert User
    -- flaky test ! it depends on a schema detail...
    describe "invalid user (empty email)" $
      it "fails" $ \logs -> do
        result <- appToIO logs $ UC._insertUserPswd r (otherUser {D._email = ""}) ""
        result `shouldBe` Just UC.AnyErr

    describe "invalid user (empty name)" $
      it "fails" $ \logs -> do
        result <- appToIO logs $ UC._insertUserPswd r (otherUser {D._name = ""}) ""
        result `shouldBe` Just UC.AnyErr

    describe "2 different users" $
      it "succeeds" $ \logs -> do
        result <- appToIO logs $ do
          Nothing <- UC._insertUserPswd r user ""
          UC._insertUserPswd r otherUser ""
        result `shouldBe` Nothing

    describe "2 users with same id" $
      it "fails" $ \logs -> do
        result <- appToIO logs $ do
          Nothing <- UC._insertUserPswd r user ""
          UC._insertUserPswd r (otherUser {D._id = uid}) ""
        result `shouldBe` Just (UC.SpecificErr UC.InsertUserConflict)

    describe "2 users with same name" $
      it "fails" $ \logs -> do
        result <- appToIO logs $ do
          Nothing <- UC._insertUserPswd r user ""
          UC._insertUserPswd r (otherUser {D._name = D._name user}) ""
        result `shouldBe` Just (UC.SpecificErr UC.InsertUserConflict)

    describe "2 users with same email" $
      it "fails" $ \logs -> do
        result <- appToIO logs $ do
          Nothing <- UC._insertUserPswd r user ""
          UC._insertUserPswd r (otherUser {D._email = D._email user}) ""
        result `shouldBe` Just (UC.SpecificErr UC.InsertUserConflict)
