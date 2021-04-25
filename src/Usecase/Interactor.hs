module Usecase.Interactor where

import qualified Adapter.Logger as Logger
import qualified Data.UUID as UUID
import qualified Domain.User as D
import RIO

-- UserRepo
data UserRepo m = UserRepo
  { _insertUserPswd :: InsertUserPswd m,
    _getUserByID :: GetUserByID m,
    _getUserByEmail :: GetUserByEmail m,
    _getUserByName :: GetUserByName m,
    _getUserByEmailAndHashedPassword :: GetUserByEmailAndHashedPassword m
  }

data Err a
  = AnyErr -- if it's a tech error we don't want more details at this level, it has to be handled "below", won't be logged here neither
  | SpecificErr a -- usecase want to know about theses errors when they happen
  deriving (Show, Eq)

data ErrInsertUser = InsertUserConflict
  deriving (Show, Eq)

type InsertUserPswd m = Monad m => D.User -> D.Password -> m (Maybe (Err ErrInsertUser))

-- Err Void is obviously impossible to construct, the single constructor is then AnyErr
-- the idea is to make explicit the fact we don't care specially about some specific things
-- that may go wrong (contrary to the `Err ErrInsertUser` above, for example).
type GetUserByID m = Monad m => UUID.UUID -> m (Either (Err Void) (Maybe D.User))

type GetUserByEmail m = Monad m => D.Email -> m (Either (Err Void) (Maybe D.User))

type GetUserByName m = Monad m => D.Name -> m (Either (Err Void) (Maybe D.User))

type GetUserByEmailAndHashedPassword m =
  Monad m => D.Email -> D.Password -> m (Either (Err Void) (Maybe D.User))

-- Mail utilies
type CheckEmailFormat m = Monad m => D.Email -> m (Maybe ())

-- UUID generation
type GenUUID m = Monad m => m UUID.UUID

-- Hasher
type HashText m = Monad m => Text -> m Text

-- Logger
class Monad m => Logger m where
  log :: Logger.Loggable a => [a] -> m ()
