module Domain.User where

import qualified Data.UUID as UUID
import RIO

data User = User
  { _id :: !UUID.UUID,
    _name :: !Text,
    _email :: !Text
  }
  deriving (Show, Eq)

data LoginDetails = LoginDetails
  { _loginEmail :: !Text,
    _loginPassword :: !Text
  }
