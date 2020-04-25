module Usecase.Interactor where

import           RIO

import qualified Domain.User                   as D
import qualified Adapter.Logger                as Logger

data Interactor m = Interactor {
  userRepo_ :: UserRepo m,
  checkEmailFormat_ :: Monad m => CheckEmailFormat m,
  genUUID_ :: Monad m => GenUUID m,
  hashText_ :: Monad m => HashText m
}

-- UserRepo
data UserRepo m = UserRepo {
  getUserByID_ :: Monad m => GetUserByID m,
  getUserByEmail_ :: Monad m => GetUserByEmail m,
  getUserByName_ :: Monad m => GetUserByName m,
  getUserByEmailAndHashedPassword_ :: Monad m => GetUserByEmailAndHashedPassword m
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



