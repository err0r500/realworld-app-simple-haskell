module Usecase.Specs.UserLoginSpec
  ( spec
  )
where

import           RIO

import           Test.Hspec
import qualified Data.UUID                     as UUID

import           Utils
import           Usecase.Lib
import           Usecase.Utils

import qualified Adapter.Storage.InMem.User    as UserRepo
import qualified Domain.User                   as D
import qualified Usecase.UserLogin             as UC

uc :: UC.Login App
uc = UC.login (\t -> pure $ "hashed-" <> t) UserRepo.getUserByEmailAndHashedPassword

loginUser :: State -> UC.Login IO
loginUser state = run state . uc

insertUserPswd_ :: D.User -> Text -> IO State
insertUserPswd_ user password = do
  state   <- emptyState
  Nothing <- run state $ UserRepo.insertUserPswd user password
  pure state

spec :: Spec
spec = do
  let userEmail    = "userEmail"
      userPassword = "userPassword" :: Text
      myUser       = D.User fakeUUID1 "userName" userEmail

  describe "happy case" $ it "returns an uuid if found" $ do
    state     <- insertUserPswd_ myUser ("hashed-" <> userPassword)
    foundUser <- loginUser state $ D.LoginDetails userEmail userPassword
    foundUser `shouldBe` Right myUser

  describe "not found" $ it "returns a ErrUserNotFound" $ do
    state        <- insertUserPswd_ myUser userPassword
    notFoundUser <- loginUser state $ D.LoginDetails (userEmail <> "oops") userPassword
    notFoundUser `shouldBe` Left UC.UserNotFound -- D.ErrUserNotFound

  describe "not found 2" $ it "returns a ErrUserNotFound" $ do
    state        <- insertUserPswd_ myUser userPassword
    notFoundUser <- loginUser state $ D.LoginDetails userEmail (userPassword <> "oops")
    notFoundUser `shouldBe` Left UC.UserNotFound --D.ErrUserNotFound
