module UserLoginSpec
        ( spec
        )
where

import qualified Adapter.InMemory.Hasher as Hasher
import qualified Adapter.InMemory.Logger as Logger
import qualified Adapter.InMemory.UserRepo as UserRepo
import           App
import           ClassyPrelude
import qualified Domain.User as D
import           Test.Hspec
import qualified Usecase.UserLogin as UC

getFreshState :: (MonadIO m) => m App.State
getFreshState = do
        state  <- newTVarIO $ UserRepo.UsersState mempty
        logger <- newTVarIO $ Logger.Logs []
        return (state, logger)

uc :: UC.Login InMemoryApp
uc = UC.login Hasher.hashText UserRepo.getUserByEmailAndHashedPassword

loginUser :: App.State -> UC.Login IO
loginUser state = App.run state . uc

spec :: Spec
spec =
        describe "user login"
                $ describe "happy case"
                $ it "returns an uuid"
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
