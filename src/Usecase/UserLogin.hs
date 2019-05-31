module Usecase.UserLogin where
import ClassyPrelude
import  Domain.User as D
import qualified Usecase.Class as UC

login ::
     (UC.UserRepo m, UC.Hasher m) => D.LoginDetails -> m (Either D.Error D.User) 
login loginDetails = do
  hashedPass <- UC.hashText $ userPassword loginDetails
  foundUser <-
    UC.getUserByEmailAndHashedPassword (userEmail loginDetails) hashedPass
  case foundUser of
    Just user -> pure $ Right user
    Nothing -> pure $ Left D.ErrUserEmailAlreadyInUse
