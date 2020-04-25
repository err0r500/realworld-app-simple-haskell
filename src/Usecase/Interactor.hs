module Usecase.Interactor where

import           RIO

import qualified Domain.User                   as D
import qualified Adapter.Logger                as Logger

data Interactor m = Interactor {
  _userRepo :: UserRepo m,
  _checkEmailFormat :: Monad m => CheckEmailFormat m,
  _genUUID :: Monad m => GenUUID m,
  _hash :: Monad m => HashText m
}

-- UserRepo
data UserRepo m = UserRepo {
  _getUserByID :: Monad m => GetUserByID m,
  _getUserByEmail :: Monad m => GetUserByEmail m,
  _getUserByName :: Monad m => GetUserByName m,
  _getUserByEmailAndHashedPassword :: Monad m => GetUserByEmailAndHashedPassword m
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



