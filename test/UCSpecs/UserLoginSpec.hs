{-# LANGUAGE OverloadedStrings #-}
module UCSpecs.UserLoginSpec
  ( spec
  )
where

import           RIO

import           Test.Hspec

import           Lib
import           Utils

import qualified Adapter.Storage.InMem.User    as UserRepo
import qualified Domain.User                   as D
import qualified Usecase.UserLogin             as UC

uc :: UC.Login App
uc = UC.login (\t -> pure $ "hashed-" <> t) UserRepo.getUserByEmailAndHashedPassword

loginUser :: State -> UC.Login IO
loginUser state = run state . uc

insertUser_ :: D.User -> Text -> IO State
insertUser_ user password = do
  state   <- emptyState
  Right _ <- run state $ UserRepo.insertUser (D._id user) (D._name user) (D._email user) password
  pure state

spec :: Spec
spec = do
  let userEmail    = "userEmail"
      userPassword = "userPassword" :: Text
      myUser       = D.User "id" "userName" userEmail

  describe "happy case" $ it "returns an uuid if found" $ do
    state     <- insertUser_ myUser ("hashed-" <> userPassword)
    foundUser <- loginUser state $ D.LoginDetails userEmail userPassword
    foundUser `shouldBe` Right myUser

  describe "not found" $ it "returns a ErrUserNotFound" $ do
    state        <- insertUser_ myUser userPassword
    notFoundUser <- loginUser state $ D.LoginDetails (userEmail <> "oops") userPassword
    notFoundUser `shouldBe` Left D.ErrUserNotFound

  describe "not found 2" $ it "returns a ErrUserNotFound" $ do
    state        <- insertUser_ myUser userPassword
    notFoundUser <- loginUser state $ D.LoginDetails userEmail (userPassword <> "oops")
    notFoundUser `shouldBe` Left D.ErrUserNotFound
