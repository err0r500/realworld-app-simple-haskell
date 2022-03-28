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

resetAndGetLogs :: IO Lib.Logs
resetAndGetLogs = do
  logs <- Lib.emptyLogs
  --appToIO logs $ reset ()
  return logs

spec :: UC.UserRepo Lib.App -> Lib.ResetFunc Lib.App -> Spec
spec r reset = do
  let uid = fakeUUID1
      user = D.User uid (D.Name "matth") (D.Email "matth@example.com")
      emptyPassword = D.Password ""
      otherUser = D.User fakeUUID2 (D.Name "other") (D.Email "other@example.com")

  before (resetAndGetLogs) $ do
    -- Get User
    describe "find user by ID" $
      it "succeeds" $ \logs -> do
        Right result <- appToIO logs $ do
          Nothing <- UC._insertUserPswd r user emptyPassword
          Nothing <- UC._insertUserPswd r otherUser emptyPassword
          UC._getUserByID r uid
        result `shouldBe` Just user

    describe "find user by email" $
      it "succeeds" $ \logs -> do
        Right result <- appToIO logs $ do
          Nothing <- UC._insertUserPswd r user emptyPassword
          Nothing <- UC._insertUserPswd r otherUser emptyPassword
          UC._getUserByEmail r (D._email user)
        result `shouldBe` Just user

    describe "find user by name" $
      it "succeeds" $ \logs -> do
        Right result <- appToIO logs $ do
          Nothing <- UC._insertUserPswd r user emptyPassword
          Nothing <- UC._insertUserPswd r otherUser emptyPassword
          UC._getUserByName r (D._name user)
        result `shouldBe` Just user

    -- Insert User
    describe "invalid user (empty email)" $
      it "fails" $ \logs -> do
        result <- appToIO logs $ UC._insertUserPswd r (otherUser {D._email = D.Email ""}) emptyPassword
        result `shouldBe` Just UC.AnyErr

    describe "invalid user (empty name)" $
      it "fails" $ \logs -> do
        result <- appToIO logs $ UC._insertUserPswd r (otherUser {D._name = D.Name ""}) emptyPassword
        result `shouldBe` Just UC.AnyErr

    describe "2 different users" $
      it "succeeds" $ \logs -> do
        result <- appToIO logs $ do
          Nothing <- UC._insertUserPswd r user emptyPassword
          UC._insertUserPswd r otherUser emptyPassword
        result `shouldBe` Nothing

    describe "2 users with same id" $
      it "fails" $ \logs -> do
        result <- appToIO logs $ do
          Nothing <- UC._insertUserPswd r user emptyPassword
          UC._insertUserPswd r (otherUser {D._id = uid}) emptyPassword
        result `shouldBe` Just (UC.SpecificErr UC.InsertUserConflict)

    describe "2 users with same name" $
      it "fails" $ \logs -> do
        result <- appToIO logs $ do
          Nothing <- UC._insertUserPswd r user emptyPassword
          UC._insertUserPswd r (otherUser {D._name = D._name user}) emptyPassword
        result `shouldBe` Just (UC.SpecificErr UC.InsertUserConflict)

    describe "2 users with same email" $
      it "fails" $ \logs -> do
        result <- appToIO logs $ do
          Nothing <- UC._insertUserPswd r user emptyPassword
          UC._insertUserPswd r (otherUser {D._email = D._email user}) emptyPassword
        result `shouldBe` Just (UC.SpecificErr UC.InsertUserConflict)
