module Usecase.UserLogin where

import qualified Domain.User as D
import RIO
import qualified Usecase.Interactor as UC

data Err = ErrTech | UserNotFound deriving (Show, Eq)

type Login m = Monad m => D.LoginDetails -> m (Either Err D.User)

login :: UC.Logger m => UC.HashText m -> UC.GetUserByEmailAndHashedPassword m -> Login m
login hash getUserByEmailAndHashedPassword loginDetails = do
  hashedPass <- hash $ D.unPassword $ D._loginPassword loginDetails
  foundUser <- getUserByEmailAndHashedPassword (D._loginEmail loginDetails) (D.Password hashedPass)
  case foundUser of
    Left err -> do
      UC.log [err]
      pure $ Left ErrTech
    Right (Just user) -> pure $ Right user
    Right Nothing -> pure $ Left UserNotFound
