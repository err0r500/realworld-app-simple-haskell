module Usecase.Interactor where

import           RIO

import qualified Domain.User                   as D
import qualified Adapter.Logger                as Logger

data Interactor m = Interactor {
  _userRepo :: UserRepo m,
  _checkEmailFormat :: CheckEmailFormat m,
  _genUUID :: GenUUID m,
  _hash :: HashText m
}

-- UserRepo
data UserRepo m = UserRepo {
  _getUserByID :: GetUserByID m,
  _getUserByEmail :: GetUserByEmail m,
  _getUserByName :: GetUserByName m,
  _getUserByEmailAndHashedPassword :: GetUserByEmailAndHashedPassword m
}

type GetUserByID m = Monad m => Text -> m (Maybe D.User)
type GetUserByEmail m = Monad m => Text -> m (Maybe D.User)
type GetUserByName m = Monad m => Text -> m (Maybe D.User)
type GetUserByEmailAndHashedPassword m = Monad m => Text -> Text -> m (Maybe D.User)

-- Mail utilies
type CheckEmailFormat m = Monad m => Text -> m (Maybe [D.Error])

-- UUID generation
type GenUUID m = Monad m => m Text

-- Hasher
type HashText m = Monad m => Text -> m Text


-- Logger
class Monad m => Logger m where
  log :: Logger.Loggable a => [a] -> m ()



