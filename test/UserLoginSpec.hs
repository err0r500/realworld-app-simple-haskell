module UserLoginSpec
        ( spec
        )
where

import           ClassyPrelude
import           Test.Hspec
import           App
import qualified Adapter.InMemory.Logger       as Logger
import qualified Adapter.InMemory.UserRepo     as UserRepo
import qualified Adapter.InMemory.Hasher       as Hasher
import qualified Adapter.InMemory.UuidGen      as UuidGen
import qualified Domain.User                   as D
import qualified Usecase.UserLogin             as UC

fakeUUID :: Text
fakeUUID = "uuid-1234"

getFreshState :: (MonadIO m) => m App.Global
getFreshState = do
        state  <- newTVarIO $ UserRepo.UsersState mempty
        logger <- newTVarIO $ Logger.Logs []
        uuid   <- newTVarIO $ UuidGen.UUIDGen fakeUUID
        return (state, logger, uuid)

uc :: UC.Login InMemoryApp
uc = UC.login Hasher.hashText UserRepo.getUserByEmailAndHashedPassword

loginUser
        :: (UsersState, LoggerState, UUIDGen_)
        -> D.LoginDetails
        -> IO (Either D.Error D.User)
loginUser state user = App.run state $ uc user

spec :: Spec
spec =
        describe "user login"
                $ describe "happy case"
                $ it "should return an uuid"
                $ do
                          state <- getFreshState
                          let     userEmail    = "userEmail"
                                  userPassword = "userPassword"
                                  myUser       = D.User "userName"
                                                        userEmail
                                                        "hasheduserPassword"
                          Right _ <- App.run state $ UserRepo.insertUser myUser
                          foundUser <- loginUser state $ D.LoginDetails
                                  userEmail
                                  userPassword
                          foundUser `shouldBe` Right myUser
