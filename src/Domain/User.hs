module Domain.User where

import           ClassyPrelude

data Error
    = ErrUserEmailAlreadyInUse
    | ErrUserNameAlreadyInUse
    | ErrMalformedEmail
    deriving (Show, Eq)

data User =
  User
    { name :: Text
    , email :: Text
    , password :: Text
    } deriving ( Show, Eq )

data LoginDetails =
  LoginDetails
    { userEmail :: Text
    , userPassword :: Text
    }
