module Domain.User where

import           ClassyPrelude

data Error
    = ErrUserEmailAlreadyInUse
    | ErrUserNameAlreadyInUse
    deriving (Show, Eq)

data User = User
    { name  :: Text
    , email :: Text
    }


