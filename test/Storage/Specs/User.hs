module Storage.Specs.User where

import           RIO

import           Test.Hspec

import qualified Storage.Lib                   as Lib

import qualified Domain.User                   as D
import qualified Usecase.Interactor            as UC


spec :: UC.UserRepo Lib.App -> Lib.ResetFunc Lib.App -> Spec
spec r reset = do
  let uid       = "a0eebc99-9c0b-4ef8-bb6d-6bb9bd380a11"
      user      = D.User uid "matth" "matth@example.com"
      otherUser = D.User "61b4ea9a-cfdb-44cc-b40b-affffeedc14e" "other" "other@example.com"

  -- Get User
  describe "find user by ID" $ it "succeeds" $ do
    logs         <- Lib.emptyLogs
    Right result <- liftIO $ Lib.run logs $ do
      reset ()
      Nothing <- UC._insertUserPswd r user ""
      Nothing <- UC._insertUserPswd r otherUser ""
      UC._getUserByID r uid
    result `shouldBe` Just user

  describe "find user by email" $ it "succeeds" $ do
    logs         <- Lib.emptyLogs
    Right result <- liftIO $ Lib.run logs $ do
      reset ()
      Nothing <- UC._insertUserPswd r user ""
      Nothing <- UC._insertUserPswd r otherUser ""
      UC._getUserByEmail r (D._email user)
    result `shouldBe` Just user

  describe "find user by name" $ it "succeeds" $ do
    logs         <- Lib.emptyLogs
    Right result <- liftIO $ Lib.run logs $ do
      reset ()
      Nothing <- UC._insertUserPswd r user ""
      Nothing <- UC._insertUserPswd r otherUser ""
      UC._getUserByName r (D._name user)
    result `shouldBe` Just user


  -- Insert User
  describe "2 different users" $ it "succeeds" $ do
    logs   <- Lib.emptyLogs
    result <- liftIO $ Lib.run logs $ do
      reset ()
      Nothing <- UC._insertUserPswd r user ""
      UC._insertUserPswd r otherUser ""
    result `shouldBe` Nothing

  describe "2 users with same id" $ it "fails" $ do
    logs   <- Lib.emptyLogs
    result <- liftIO $ Lib.run logs $ do
      reset ()
      Nothing <- UC._insertUserPswd r user ""
      UC._insertUserPswd r (otherUser { D._id = uid }) ""
    result `shouldBe` Just (UC.SpecificErr UC.InsertUserConflict)

  describe "2 users with same name" $ it "fails" $ do
    logs   <- Lib.emptyLogs
    result <- liftIO $ Lib.run logs $ do
      reset ()
      Nothing <- UC._insertUserPswd r user ""
      UC._insertUserPswd r (otherUser { D._name = D._name user }) ""
    result `shouldBe` Just (UC.SpecificErr UC.InsertUserConflict)

  describe "2 users with same email" $ it "fails" $ do
    logs   <- Lib.emptyLogs
    result <- liftIO $ Lib.run logs $ do
      reset ()
      Nothing <- UC._insertUserPswd r user ""
      UC._insertUserPswd r (otherUser { D._email = D._email user }) ""
    result `shouldBe` Just (UC.SpecificErr UC.InsertUserConflict)
