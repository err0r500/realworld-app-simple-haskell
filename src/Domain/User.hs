module Domain.User where

import           RIO
import qualified Data.UUID                     as UUID

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
