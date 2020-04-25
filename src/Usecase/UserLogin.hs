module Usecase.UserLogin where

import           RIO
import           Domain.User                   as D
import qualified Usecase.Interactor            as UC

type Login m = Monad m => D.LoginDetails -> m (Either D.Error D.User)

login :: UC.Logger m => UC.HashText m -> UC.GetUserByEmailAndHashedPassword m -> Login m
login hashText getUserByEmailAndHashedPassword loginDetails = do
  hashedPass <- hashText $ userPassword loginDetails
  foundUser  <- getUserByEmailAndHashedPassword (userEmail loginDetails) hashedPass
  case foundUser of
    Just user -> pure $ Right user
    Nothing   -> pure $ Left D.ErrUserNotFound
