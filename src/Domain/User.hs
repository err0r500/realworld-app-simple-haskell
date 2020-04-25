module Domain.User where

import           RIO

data Error
    = ErrUserEmailAlreadyInUse
      | ErrUserNameAlreadyInUse
      | ErrMalformedEmail
      | ErrUserNotFound
    deriving (Show, Eq)

data User =
  User
    { name :: !Text
    , email :: !Text
    , password :: !Text
    } deriving ( Show, Eq )

data LoginDetails =
  LoginDetails
    { userEmail :: !Text
    , userPassword :: !Text
    }
