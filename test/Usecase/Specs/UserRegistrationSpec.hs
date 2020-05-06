module Usecase.Specs.UserRegistrationSpec
  ( spec
  )
where

import           RIO

import           Test.Hspec

import           Usecase.Lib
import           Utils
import           Usecase.Utils
import qualified Data.UUID                     as UUID

import qualified Adapter.EmailChecker          as MailChecker
import qualified Adapter.Fake.Logger           as Logger
import qualified Adapter.Fake.UUID             as Uuid

import qualified Adapter.Storage.InMem.User    as InMem
import qualified Domain.User                   as D
import qualified Usecase.Interactor            as UC
import qualified Usecase.UserRegistration      as UC

uc :: UC.Register App
uc = UC.register (Uuid.genUUID fakeUUID1)
                 MailChecker.checkEmailFormat
                 InMem.getUserByEmail
                 InMem.getUserByName
                 InMem.insertUserPswd

registerUser :: State -> UC.Register IO
registerUser state name email password = run state $ uc name email password

spec :: Spec
spec = do
  let prevUser       = D.User fakeUUID1 "collidingUserName" "colliding@email.fr"
      currUser       = D.User fakeUUID2 "currentUserName" "current@email.fr"
      malformedEmail = "current.email.fr" :: Text
      password       = "myPass" :: Text

  describe "happy case" $ it "should return the uuid" $ do
    state      <- emptyState
    Right uuid <- registerUser state (D._name currUser) (D._email currUser) password
    uuid `shouldBe` UUID.toText fakeUUID1


  describe "collision with other user email" $ it "raises an UserEmailAlreadyInUse error" $ do
    state <- emptyState
    run state $ InMem.insertUserPswd prevUser password
    Left (UC.ErrValidation resp) <- registerUser state
                                                 (D._name currUser)
                                                 (D._email prevUser)
                                                 password
    resp `shouldBe` [UC.EmailConflict]
    checkLogs state [UC.EmailConflict]


  describe "collision with other user name" $ it "raises an UserNameAlreadyInUse error" $ do
    state <- emptyState
    run state $ InMem.insertUserPswd prevUser password
    Left (UC.ErrValidation resp) <- registerUser state
                                                 (D._name prevUser)
                                                 (D._email currUser)
                                                 password
    resp `shouldBe` [UC.NameConflict]
    checkLogs state [UC.NameConflict]


  describe "collision with other user name & other user email"
    $ it "raises UserNameAlreadyInUse & UserEmailAlreadyInUse errors"
    $ do
        state <- emptyState
        run state $ InMem.insertUserPswd prevUser password
        Left (UC.ErrValidation resp) <- registerUser state
                                                     (D._name prevUser)
                                                     (D._email prevUser)
                                                     password
        resp `shouldMatchList` [UC.NameConflict, UC.EmailConflict]
        checkLogs state [UC.NameConflict, UC.EmailConflict]


  describe "malformed email" $ it "raises an MalformedEmail error" $ do
    state                        <- emptyState
    Left (UC.ErrValidation resp) <- registerUser state (D._name currUser) malformedEmail password
    resp `shouldMatchList` [UC.MalformedEmail]
    checkLogs state [UC.MalformedEmail]
