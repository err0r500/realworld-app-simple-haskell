module Usecase.Specs.UserRegistrationSpec
  ( spec,
  )
where

import qualified Adapter.EmailChecker as MailChecker
import qualified Adapter.Fake.Logger as Logger
import qualified Adapter.Fake.UUID as Uuid
import qualified Adapter.Storage.InMem.User as InMem
import qualified Data.UUID as UUID
import qualified Domain.User as D
import RIO
import Test.Hspec
import qualified Usecase.Interactor as UC
import Usecase.Lib
import qualified Usecase.UserRegistration as UC
import Usecase.Utils
import Utils

uc :: UC.Register App
uc =
  UC.register
    (Uuid.genUUID fakeUUID1)
    MailChecker.checkEmailFormat
    InMem.getUserByEmail
    InMem.getUserByName
    InMem.insertUserPswd

registerUser :: State -> UC.Register IO
registerUser st name email pswd = run st $ uc name email pswd

spec :: Spec
spec = do
  let prevUsr = D.User fakeUUID1 (D.Name "collidingUserName") (D.Email "colliding@email.fr")
      currUsr = D.User fakeUUID2 (D.Name "currentUserName") (D.Email "current@email.fr")
      malformedEmail = D.Email "current.email.fr"
      pswd = D.Password "myPass"
      insertPrev st = run st $ InMem.insertUserPswd prevUsr pswd

  before emptyState $ do
    describe "happy case" $
      it "should return the uuid" $ \st -> do
        Right uuid <- registerUser st (D._name currUsr) (D._email currUsr) pswd
        uuid `shouldBe` UUID.toText fakeUUID1

    describe "collision with other user email" $
      it "raises an UserEmailAlreadyInUse error" $
        \st -> do
          insertPrev st
          Left (UC.ErrValidation resp) <- registerUser st (D._name currUsr) (D._email prevUsr) pswd
          resp `shouldBe` [UC.EmailConflict]
          checkLogs st [UC.EmailConflict]

    describe "collision with other user name" $ --
      it "raises an UserNameAlreadyInUse error" $
        \st -> do
          insertPrev st
          Left (UC.ErrValidation resp) <- registerUser st (D._name prevUsr) (D._email currUsr) pswd
          resp `shouldBe` [UC.NameConflict]
          checkLogs st [UC.NameConflict]

    describe "collision with other user name & other user email" $
      it "raises UserNameAlreadyInUse & UserEmailAlreadyInUse errors" $
        \st -> do
          insertPrev st
          Left (UC.ErrValidation resp) <- registerUser st (D._name prevUsr) (D._email prevUsr) pswd
          resp `shouldMatchList` [UC.NameConflict, UC.EmailConflict]
          checkLogs st [UC.NameConflict, UC.EmailConflict]

    describe "malformed email" $
      it "raises an MalformedEmail error" $ \st -> do
        Left (UC.ErrValidation resp) <- registerUser st (D._name currUsr) malformedEmail pswd
        resp `shouldMatchList` [UC.MalformedEmail]
        checkLogs st [UC.MalformedEmail]
