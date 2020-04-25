module UCSpecs.UserRegistrationSpec
  ( spec
  )
where

import           RIO

import           Test.Hspec

import           Lib
import           Utils
import qualified Adapter.EmailChecker          as MailChecker
import qualified Adapter.Fake.Logger           as Logger
import qualified Adapter.Fake.UUID             as Uuid

import qualified Adapter.InMemory.UserRepo     as InMem
import qualified Domain.User                   as D
import qualified Usecase.Interactor            as UC
import qualified Usecase.UserRegistration      as UC

uc :: UC.Register App
uc = UC.register (UC._genUUID i)
                 (UC._checkEmailFormat i)
                 (UC._getUserByEmail $ UC._userRepo i)
                 (UC._getUserByName $ UC._userRepo i)
 where
  i = UC.Interactor (UC.UserRepo undefined InMem.getUserByEmail InMem.getUserByName undefined)
                    MailChecker.checkEmailFormat
                    (Uuid.genUUID defaultFakeUUID)
                    undefined

registerUser :: State -> UC.Register IO
registerUser state name email = run state $ uc name email

spec :: Spec
spec = do
  let prevUser =
        D.User { D.name = "collidingUserName", D.email = "colliding@email.fr", D.password = "" }
      currUser =
        D.User { D.name = "currentUserName", D.email = "current@email.fr", D.password = "" }
      malformedEmail = "current.email.fr" :: Text

  describe "happy case" $ it "should return the uuid" $ do
    state      <- emptyState
    Right uuid <- registerUser state (D.name currUser) (D.email currUser)
    uuid `shouldBe` defaultFakeUUID


  describe "collision with other user email" $ it "raises an UserEmailAlreadyInUse error" $ do
    state <- emptyState
    run state $ InMem.insertUser prevUser
    Left resp <- registerUser state (D.name currUser) (D.email prevUser)
    resp `shouldBe` [D.ErrUserEmailAlreadyInUse]
    checkLogs state [D.ErrUserEmailAlreadyInUse]


  describe "collision with other user name" $ it "raises an UserNameAlreadyInUse error" $ do
    state <- emptyState
    run state $ InMem.insertUser prevUser
    Left resp <- registerUser state (D.name prevUser) (D.email currUser)
    resp `shouldBe` [D.ErrUserNameAlreadyInUse]
    checkLogs state [D.ErrUserNameAlreadyInUse]


  describe "collision with other user name & other user email"
    $ it "raises UserNameAlreadyInUse & UserEmailAlreadyInUse errors"
    $ do
        state <- emptyState
        run state $ InMem.insertUser prevUser
        Left resp <- registerUser state (D.name prevUser) (D.email prevUser)
        resp `shouldMatchList` [D.ErrUserNameAlreadyInUse, D.ErrUserEmailAlreadyInUse]
        checkLogs state [D.ErrUserNameAlreadyInUse, D.ErrUserEmailAlreadyInUse]


  describe "malformed email" $ it "raises an MalformedEmail error" $ do
    state     <- emptyState
    Left resp <- registerUser state (D.name currUser) malformedEmail
    resp `shouldMatchList` [D.ErrMalformedEmail]
    checkLogs state [D.ErrMalformedEmail]
