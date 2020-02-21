module UserRegistrationSpec
        ( spec
        )
where

import qualified Adapter.EmailChecker as MailChecker
import qualified Adapter.InMemory.Logger as Logger
import qualified Adapter.InMemory.UserRepo as InMem
import qualified Adapter.UUIDGen as UuidGen
import           App
import           ClassyPrelude
import qualified Domain.User as D
import           Test.Hspec
import qualified Usecase.Class as UC
import           Usecase.UserRegistration as UC

fakeUUID :: Text
fakeUUID = "uuid-1234"

getFreshState :: (MonadIO m) => m App.State
getFreshState = do
        state  <- newTVarIO $ InMem.UsersState mempty
        logger <- newTVarIO $ Logger.Logs []
        uuid   <- newTVarIO $ UuidGen.UUIDGen fakeUUID
        return (state, logger)

uc :: Register InMemoryApp
uc = UC.register (UC.genUUID_ i)
                 (UC.checkEmailFormat_ i)
                 (UC.getUserByEmail_ $ UC.userRepo_ i)
                 (UC.getUserByName_ $ UC.userRepo_ i)
    where
        userRepo = UC.UserRepo undefined
                               InMem.getUserByEmail
                               InMem.getUserByName
                               undefined
        mailChecker = MailChecker.checkEmailFormat
        i           = UC.Interactor userRepo
                                    mailChecker
                                    (UuidGen.genUUIDFake fakeUUID)
                                    undefined

registerUser :: App.State -> UC.Register IO
registerUser state name email = App.run state $ uc name email

getLogs :: (Show a) => App.State -> [a] -> IO ()
getLogs state expectedLogs = do
        logs <- App.run state Logger.getLogs
        length logs `shouldBe` length expectedLogs
        logs `shouldMatchList` map tshow expectedLogs

spec :: Spec
spec = describe "register user" $ do
        let     prevUser = D.User { D.name     = "collidingUserName"
                                  , D.email    = "colliding@email.fr"
                                  , D.password = ""
                                  }
                currUser = D.User { D.name     = "currentUserName"
                                  , D.email    = "current@email.fr"
                                  , D.password = ""
                                  }
                malformedEmail = "current.email.fr" :: Text
        describe "happy case" $ it "should return the uuid" $ do
                state      <- getFreshState
                Right uuid <- registerUser state
                                           (D.name currUser)
                                           (D.email currUser)
                uuid `shouldBe` fakeUUID
        describe "collision with other user email"
                $ it "raises an UserEmailAlreadyInUse error"
                $ do
                          state     <- getFreshState
                          _         <- App.run state $ InMem.insertUser prevUser
                          Left resp <- registerUser state
                                                    (D.name currUser)
                                                    (D.email prevUser)
                          resp `shouldBe` [D.ErrUserEmailAlreadyInUse]
                          getLogs state [D.ErrUserEmailAlreadyInUse]
        describe "collision with other user name"
                $ it "raises an UserNameAlreadyInUse error"
                $ do
                          state     <- getFreshState
                          _         <- App.run state $ InMem.insertUser prevUser
                          Left resp <- registerUser state
                                                    (D.name prevUser)
                                                    (D.email currUser)
                          resp `shouldBe` [D.ErrUserNameAlreadyInUse]
                          getLogs state [D.ErrUserNameAlreadyInUse]
        describe "collision with other user name & other user email"
                $ it
                          "raises UserNameAlreadyInUse & UserEmailAlreadyInUse errors"
                $ do
                          state     <- getFreshState
                          _         <- App.run state $ InMem.insertUser prevUser
                          Left resp <- registerUser state
                                                    (D.name prevUser)
                                                    (D.email prevUser)
                          resp
                                  `shouldMatchList` [ D.ErrUserNameAlreadyInUse
                                                    , D.ErrUserEmailAlreadyInUse
                                                    ]
                          getLogs
                                  state
                                  [ D.ErrUserNameAlreadyInUse
                                  , D.ErrUserEmailAlreadyInUse
                                  ]
        describe "malformed email" $ it "raises an MalformedEmail error" $ do
                state     <- getFreshState
                Left resp <- registerUser state (D.name currUser) malformedEmail
                resp `shouldMatchList` [D.ErrMalformedEmail]
                getLogs state [D.ErrMalformedEmail]
