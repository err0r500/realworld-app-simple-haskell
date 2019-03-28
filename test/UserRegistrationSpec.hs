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

getCurrentLogs :: UsersState -> LoggerState -> UUIDGen_ -> IO [Text]
getCurrentLogs state logger uuid = App.run state logger uuid Logger.getLogs

triggerUC :: (UsersState, LoggerState, UUIDGen_) -> Text -> Text -> IO (Either [Domain.Error] Text)
triggerUC (state, logger, uuid) name email = App.run state logger uuid $ Usecase.UserRegistration.register name email

checkPresentLogs :: (Show a) => (UsersState, LoggerState, UUIDGen_) -> [a] -> IO ()
checkPresentLogs (users, logger, uuid) expectedLogs = do
    logs <- App.run users logger uuid Logger.getLogs
    length logs `shouldBe` length expectedLogs
    logs `shouldMatchList` map tshow expectedLogs

spec :: Spec
spec =
    describe "register user" $ do
        let previousUser = Domain.User "colliding@email.fr" "collidingUserName"
            currentUser = Domain.User "current@email.fr" "currentUserName"
        describe "happy case" $
            it "should return the uuid" $ do
                emptyState <- getFreshState
                uuid <- triggerUC emptyState (Domain.name currentUser) (Domain.email currentUser)
                uuid `shouldBe` Right fakeUUID
        describe "collision with other user email" $
            it "should raise an UserEmailAlreadyInUse error" $ do
                (users, logger, uuid) <- getFreshState
                App.run users logger uuid $ UserRepo.insertUser previousUser
                Left resp <- triggerUC (users, logger, uuid) (Domain.name currentUser) (Domain.email previousUser)
                resp `shouldBe` [Domain.ErrUserEmailAlreadyInUse]
                checkPresentLogs (users, logger, uuid) [Domain.ErrUserEmailAlreadyInUse]
        describe "collision with other user name" $
            it "should raise an UserNameAlreadyInUse error" $ do
                (users, logger, uuid) <- getFreshState
                App.run users logger uuid $ UserRepo.insertUser previousUser
                Left resp <- triggerUC (users, logger, uuid) (Domain.name previousUser) (Domain.email currentUser)
                resp `shouldBe` [Domain.ErrUserNameAlreadyInUse]
                checkPresentLogs (users, logger, uuid) [Domain.ErrUserNameAlreadyInUse]
        describe "collision with other user name & other user name" $
            it "should raise an UserNameAlreadyInUse & UserEmailAlreadyInUse errors" $ do
                (users, logger, uuid) <- getFreshState
                App.run users logger uuid $ UserRepo.insertUser previousUser
                Left resp <- triggerUC (users, logger, uuid) (Domain.name previousUser) (Domain.email previousUser)
                resp `shouldMatchList` [Domain.ErrUserNameAlreadyInUse, Domain.ErrUserEmailAlreadyInUse]
                checkPresentLogs (users, logger, uuid) [Domain.ErrUserNameAlreadyInUse, Domain.ErrUserEmailAlreadyInUse]
