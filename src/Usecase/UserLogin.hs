module Usecase.UserLogin where

import           RIO
import qualified Domain.User                   as D
import qualified Usecase.Interactor            as UC


type Login m = Monad m => D.LoginDetails -> m (Either D.Error D.User)

login :: UC.Logger m => UC.HashText m -> UC.GetUserByEmailAndHashedPassword m -> Login m
login hash getUserByEmailAndHashedPassword loginDetails = do
  hashedPass <- hash $ D._loginPassword loginDetails
  foundUser  <- getUserByEmailAndHashedPassword (D._loginEmail loginDetails) hashedPass
  case foundUser of
    Left err -> do
      UC.log [err]
      pure $ Left D.ErrTechnical
    Right (Just user) -> pure $ Right user
    Right Nothing     -> pure $ Left D.ErrUserNotFound
