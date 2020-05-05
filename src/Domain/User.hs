module Domain.User where

import           RIO

data Error
    = ErrEmailConflict
      | ErrNameConflict
      | ErrIdConflict
      | ErrMalformedEmail
      | ErrUserNotFound
      | ErrUserConflict
      | ErrTechnical
      | ErrMalformed
    deriving (Show, Eq)

data User =
  User { _id :: !Text
    , _name :: !Text
    , _email :: !Text
    } deriving ( Show, Eq )

data LoginDetails =
  LoginDetails
    { _loginEmail :: !Text
    , _loginPassword :: !Text
    }


data ErrMalformed'
data ErrIdConflict'
