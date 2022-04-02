module Usecase.Specs.UserLoginSpec
  ( spec,
  )
where

import qualified Adapter.Storage.InMem.User as UserRepo
import qualified Data.UUID as UUID
import qualified Domain.User as D
import RIO
import Test.Hspec
import Usecase.Lib
import qualified Usecase.UserLogin as UC
import Usecase.Utils
import Utils

uc :: UC.Login App
uc = UC.login (\t -> pure $ "hashed-" <> t) UserRepo.getUserByEmailAndHashedPassword

loginUser :: State -> UC.Login IO
loginUser state = run state . uc

insertUserPswd_ :: D.User -> D.Password -> IO State
insertUserPswd_ user password = do
  state <- emptyState
  Nothing <- run state $ UserRepo.insertUserPswd user password
  pure state

spec :: Spec
spec = do
  let userEmail = D.Email "userEmail"
      userPassword = D.Password "userPassword"
      userName = D.Name "userName"
      myUser = D.User fakeUUID1 userName userEmail

  describe "happy case" $
    it "returns an uuid if found" $ do
      state <- insertUserPswd_ myUser (D.Password "hashed-" <> userPassword)
      foundUser <- loginUser state $ D.LoginDetails userEmail userPassword
      foundUser `shouldBe` Right myUser

  describe "not found" $
    it "returns a ErrUserNotFound" $ do
      state <- insertUserPswd_ myUser userPassword
      notFoundUser <- loginUser state $ D.LoginDetails (userEmail <> D.Email "oops") userPassword
      notFoundUser `shouldBe` Left UC.UserNotFound
  describe "not found 2" $
    it "returns a ErrUserNotFound" $ do
      state <- insertUserPswd_ myUser userPassword
      notFoundUser <- loginUser state $ D.LoginDetails userEmail (userPassword <> D.Password "oops")
      notFoundUser `shouldBe` Left UC.UserNotFound
