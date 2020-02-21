module UserLoginSpec
        ( spec
        )
where

import qualified Adapter.InMemory.Hasher       as Hasher
import qualified Adapter.InMemory.Logger       as Logger
import qualified Adapter.InMemory.UserRepo     as UserRepo
import           App
import           ClassyPrelude
import qualified Domain.User                   as D
import           Test.Hspec
import qualified Usecase.UserLogin             as UC

getFreshState :: (MonadIO m) => m App.State
getFreshState = do
        state  <- newTVarIO $ UserRepo.UsersState mempty
        logger <- newTVarIO $ Logger.Logs []
        return (state, logger)

uc :: UC.Login InMemoryApp
uc = UC.login (\t -> pure $ "hashed-" ++ t)
              UserRepo.getUserByEmailAndHashedPassword

loginUser :: App.State -> UC.Login IO
loginUser state = App.run state . uc

insertUser_ :: D.User -> IO State
insertUser_ user = do
        state   <- getFreshState
        Right _ <- App.run state $ UserRepo.insertUser user
        pure state

spec :: Spec
spec = do
        let     userEmail    = "userEmail"
                userPassword = "userPassword"
                myUser =
                        D.User "userName" userEmail ("hashed-" ++ userPassword)

        describe "happy case" $ it "returns an uuid if found" $ do
                state     <- insertUser_ myUser
                foundUser <- loginUser state
                        $ D.LoginDetails userEmail userPassword
                foundUser `shouldBe` Right myUser

        describe "not found" $ it "returns a ErrUserNotFound" $ do
                state        <- insertUser_ myUser
                notFoundUser <- loginUser state
                        $ D.LoginDetails (userEmail ++ "oops") userPassword
                notFoundUser `shouldBe` Left D.ErrUserNotFound

        describe "not found 2" $ it "returns a ErrUserNotFound" $ do
                state        <- insertUser_ myUser
                notFoundUser <- loginUser state
                        $ D.LoginDetails userEmail (userPassword ++ "oops")
                notFoundUser `shouldBe` Left D.ErrUserNotFound
