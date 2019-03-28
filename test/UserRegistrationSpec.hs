module UserRegistrationSpec
    ( spec
    ) where

import qualified Adapter.InMemory.Logger   as Logger
import qualified Adapter.InMemory.UserRepo as UserRepo
import qualified Adapter.InMemory.UuidGen  as UuidGen
import           App
import           ClassyPrelude
import qualified Domain.User               as Domain
import           Test.Hspec
import           Usecase.Class
import           Usecase.UserRegistration

fakeUUID = "uuid-1234"

getFreshState :: (MonadIO m) => m (UsersState, LoggerState, UUIDGen_)
getFreshState = do
    state <- newTVarIO $ UserRepo.UsersState mempty
    logger <- newTVarIO $ Logger.Logs []
    uuid <- newTVarIO $ UuidGen.UUIDGen fakeUUID
    return (state, logger, uuid)

getCurrentLogs :: (UsersState, LoggerState, UUIDGen_) -> IO [Text]
getCurrentLogs state = App.run state Logger.getLogs

triggerUC :: (UsersState, LoggerState, UUIDGen_) -> Text -> Text -> IO (Either [Domain.Error] Text)
triggerUC state name email = App.run state $ Usecase.UserRegistration.register name email

checkPresentLogs :: (Show a) => (UsersState, LoggerState, UUIDGen_) -> [a] -> IO ()
checkPresentLogs state expectedLogs = do
    logs <- App.run state Logger.getLogs
    length logs `shouldBe` length expectedLogs
    logs `shouldMatchList` map tshow expectedLogs

spec :: Spec
spec =
    describe "register user" $ do
        let previousUser = Domain.User {Domain.name = "collidingUserName", Domain.email = "colliding@email.fr"}
            currentUser = Domain.User {Domain.name = "currentUserName", Domain.email = "current@email.fr"}
            malformedEmail = "current.email.fr" :: Text
        describe "happy case" $
            it "should return the uuid" $ do
                state <- getFreshState
                uuid <- triggerUC state (Domain.name currentUser) (Domain.email currentUser)
                uuid `shouldBe` Right fakeUUID
        describe "collision with other user email" $
            it "should raise an UserEmailAlreadyInUse error" $ do
                state <- getFreshState
                App.run state $ UserRepo.insertUser previousUser
                Left resp <- triggerUC state (Domain.name currentUser) (Domain.email previousUser)
                resp `shouldBe` [Domain.ErrUserEmailAlreadyInUse]
                checkPresentLogs state [Domain.ErrUserEmailAlreadyInUse]
        describe "collision with other user name" $
            it "should raise an UserNameAlreadyInUse error" $ do
                state <- getFreshState
                App.run state $ UserRepo.insertUser previousUser
                Left resp <- triggerUC state (Domain.name previousUser) (Domain.email currentUser)
                resp `shouldBe` [Domain.ErrUserNameAlreadyInUse]
                checkPresentLogs state [Domain.ErrUserNameAlreadyInUse]
        describe "collision with other user name & other user name" $
            it "should raise an UserNameAlreadyInUse & UserEmailAlreadyInUse errors" $ do
                state <- getFreshState
                App.run state $ UserRepo.insertUser previousUser
                Left resp <- triggerUC state (Domain.name previousUser) (Domain.email previousUser)
                resp `shouldMatchList` [Domain.ErrUserNameAlreadyInUse, Domain.ErrUserEmailAlreadyInUse]
                checkPresentLogs state [Domain.ErrUserNameAlreadyInUse, Domain.ErrUserEmailAlreadyInUse]
        describe "malformed email" $
            it "should raise an MalformedEmail error" $ do
                state <- getFreshState
                Left resp <- triggerUC state (Domain.name currentUser) malformedEmail
                resp `shouldMatchList` [Domain.ErrMalformedEmail]
                checkPresentLogs state [Domain.ErrMalformedEmail]
