{-# LANGUAGE OverloadedStrings #-}
module UCSpecs.UserLoginSpec
  ( spec
  )
where

import           RIO

import           Test.Hspec

import           Lib
import           Utils

import qualified Adapter.Fake.Hasher           as Hasher
import qualified Adapter.Fake.Logger           as Logger
import qualified Adapter.InMemory.UserRepo     as UserRepo
import qualified Domain.User                   as D
import qualified Usecase.UserLogin             as UC

uc :: UC.Login App
uc = UC.login (\t -> pure $ "hashed-" <> t) UserRepo.getUserByEmailAndHashedPassword

loginUser :: State -> UC.Login IO
loginUser state = run state . uc

insertUser_ :: D.User -> IO State
insertUser_ user = do
  state   <- emptyState
  Right _ <- run state $ UserRepo.insertUser user
  pure state

spec :: Spec
spec = do
  let userEmail    = "userEmail"
      userPassword = "userPassword" :: Text
      myUser       = D.User "userName" userEmail ("hashed-" <> userPassword)

  describe "happy case" $ it "returns an uuid if found" $ do
    state     <- insertUser_ myUser
    foundUser <- loginUser state $ D.LoginDetails userEmail userPassword
    foundUser `shouldBe` Right myUser

  describe "not found" $ it "returns a ErrUserNotFound" $ do
    state        <- insertUser_ myUser
    notFoundUser <- loginUser state $ D.LoginDetails (userEmail <> "oops") userPassword
    notFoundUser `shouldBe` Left D.ErrUserNotFound

  describe "not found 2" $ it "returns a ErrUserNotFound" $ do
    state        <- insertUser_ myUser
    notFoundUser <- loginUser state $ D.LoginDetails userEmail (userPassword <> "oops")
    notFoundUser `shouldBe` Left D.ErrUserNotFound
