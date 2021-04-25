module Domain.User where

import Data.Aeson
import qualified Data.UUID as UUID
import RIO

newtype Email = Email {unEmail :: Text}
  deriving (Show, Eq, Semigroup, FromJSON, ToJSON)

newtype Name = Name {unName :: Text}
  deriving (Show, Eq, FromJSON, ToJSON)

newtype Password = Password {unPassword :: Text}
  deriving (Show, Eq, Semigroup, FromJSON, ToJSON)

data User = User
  { _id :: !UUID.UUID,
    _name :: !Name,
    _email :: !Email
  }
  deriving (Show, Eq)

data LoginDetails = LoginDetails
  { _loginEmail :: !Email,
    _loginPassword :: !Password
  }
