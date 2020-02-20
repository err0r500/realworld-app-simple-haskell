module Usecase.UserLogin where

import           ClassyPrelude
import           Domain.User                   as D
import qualified Usecase.Class                 as UC

login
        :: (Monad m, UC.Hasher m)
        => UC.GetUserByEmailAndHashedPassword m
        -> D.LoginDetails
        -> m (Either D.Error D.User)
login getUserByEmailAndHashedPassword loginDetails = do
        hashedPass <- UC.hashText $ userPassword loginDetails
        foundUser  <- getUserByEmailAndHashedPassword
                (userEmail loginDetails)
                hashedPass
        case foundUser of
                Just user -> pure $ Right user
                Nothing   -> pure $ Left D.ErrUserEmailAlreadyInUse
