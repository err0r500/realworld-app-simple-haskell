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
  _insertUserPswd :: InsertUserPswd m,
  _getUserByID :: GetUserByID m,
  _getUserByEmail :: GetUserByEmail m,
  _getUserByName :: GetUserByName m,
  _getUserByEmailAndHashedPassword :: GetUserByEmailAndHashedPassword m
}

data Err a = AnyErr -- if it's a tech error we don't want more details at this level, it has to be handled "below", won't be logged here neither
  | SpecificErr a  -- usecase want to know about theses errors when they happen
  deriving (Show, Eq)

data ErrInsertUser = InsertUserConflict deriving (Show, Eq)
type InsertUserPswd m = Monad m => D.User -> Text -> m (Maybe (Err ErrInsertUser))
type GetUserByID m = Monad m => Text -> m (Either (Err Void) (Maybe D.User))
type GetUserByEmail m = Monad m => Text -> m (Either (Err Void) (Maybe D.User))
type GetUserByName m = Monad m => Text -> m (Either (Err Void) (Maybe D.User))
type GetUserByEmailAndHashedPassword m
  = Monad m => Text -> Text -> m (Either (Err Void) (Maybe D.User))

-- Mail utilies
type CheckEmailFormat m = Monad m => Text -> m (Maybe ())

-- UUID generation
type GenUUID m = Monad m => m Text

-- Hasher
type HashText m = Monad m => Text -> m Text


-- Logger
class Monad m => Logger m where
  log :: Logger.Loggable a => [a] -> m ()



