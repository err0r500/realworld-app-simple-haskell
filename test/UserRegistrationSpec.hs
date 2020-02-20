module UserRegistrationSpec
        ( spec
        )
where

import           ClassyPrelude
import           Test.Hspec
import qualified Adapter.InMemory.Logger       as Logger
import qualified Adapter.InMemory.UserRepo     as InMemUserRepo
import qualified Adapter.InMemory.UuidGen      as UuidGen
import           App
import qualified Domain.User                   as D
import           Usecase.UserRegistration      as UC
import qualified Usecase.Class                 as UC
import qualified Adapter.EmailChecker          as MailChecker

fakeUUID :: Text
fakeUUID = "uuid-1234"

getFreshState :: (MonadIO m) => m App.Global
getFreshState = do
        state  <- newTVarIO $ InMemUserRepo.UsersState mempty
        logger <- newTVarIO $ Logger.Logs []
        uuid   <- newTVarIO $ UuidGen.UUIDGen fakeUUID
        return (state, logger, uuid)

uc :: Register InMemoryApp
uc = UC.register (UC.genUUID_ i)
                 (UC.checkEmailFormat_ i)
                 (UC.getUserByEmail_ $ UC.userRepo_ i)
                 (UC.getUserByName_ $ UC.userRepo_ i)
    where
        userRepo = UC.UserRepo undefined
                               InMemUserRepo.getUserByEmail
                               InMemUserRepo.getUserByName
                               undefined
        mailChecker = MailChecker.checkEmailFormat
        genU        = UuidGen.genUUIDv4
        i           = UC.Interactor userRepo mailChecker genU

registerUser
        :: (UsersState, LoggerState, UUIDGen_)
        -> Text
        -> Text
        -> IO (Either [D.Error] Text)
registerUser state name email = App.run state $ uc name email

checkPresentLogs
        :: (Show a) => (UsersState, LoggerState, UUIDGen_) -> [a] -> IO ()
checkPresentLogs state expectedLogs = do
        logs <- App.run state Logger.getLogs
        length logs `shouldBe` length expectedLogs
        logs `shouldMatchList` map tshow expectedLogs

spec :: Spec
spec = describe "register user" $ do
        let     previousUser = D.User { D.name     = "collidingUserName"
                                      , D.email    = "colliding@email.fr"
                                      , D.password = ""
                                      }
                currentUser = D.User { D.name     = "currentUserName"
                                     , D.email    = "current@email.fr"
                                     , D.password = ""
                                     }
                malformedEmail = "current.email.fr" :: Text
        describe "happy case" $ it "should return the uuid" $ do
                state      <- getFreshState
                Right uuid <- registerUser state
                                           (D.name currentUser)
                                           (D.email currentUser)
                uuid `shouldBe` fakeUUID
        describe "collision with other user email"
                $ it "should raise an UserEmailAlreadyInUse error"
                $ do
                          state <- getFreshState
                          _     <- App.run state
                                  $ InMemUserRepo.insertUser previousUser
                          Left resp <- registerUser
                                  state
                                  (D.name currentUser)
                                  (D.email previousUser)
                          resp `shouldBe` [D.ErrUserEmailAlreadyInUse]
                          checkPresentLogs state [D.ErrUserEmailAlreadyInUse]
        describe "collision with other user name"
                $ it "should raise an UserNameAlreadyInUse error"
                $ do
                          state <- getFreshState
                          _     <- App.run state
                                  $ InMemUserRepo.insertUser previousUser
                          Left resp <- registerUser
                                  state
                                  (D.name previousUser)
                                  (D.email currentUser)
                          resp `shouldBe` [D.ErrUserNameAlreadyInUse]
                          checkPresentLogs state [D.ErrUserNameAlreadyInUse]
        describe "collision with other user name & other user name"
                $ it
                          "should raise an UserNameAlreadyInUse & UserEmailAlreadyInUse errors"
                $ do
                          state <- getFreshState
                          _     <- App.run state
                                  $ InMemUserRepo.insertUser previousUser
                          Left resp <- registerUser
                                  state
                                  (D.name previousUser)
                                  (D.email previousUser)
                          resp
                                  `shouldMatchList` [ D.ErrUserNameAlreadyInUse
                                                    , D.ErrUserEmailAlreadyInUse
                                                    ]
                          checkPresentLogs
                                  state
                                  [ D.ErrUserNameAlreadyInUse
                                  , D.ErrUserEmailAlreadyInUse
                                  ]
        describe "malformed email"
                $ it "should raise an MalformedEmail error"
                $ do
                          state     <- getFreshState
                          Left resp <- registerUser state
                                                    (D.name currentUser)
                                                    malformedEmail
                          resp `shouldMatchList` [D.ErrMalformedEmail]
                          checkPresentLogs state [D.ErrMalformedEmail]
