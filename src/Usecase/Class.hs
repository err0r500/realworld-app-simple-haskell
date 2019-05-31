module Usecase.Class where

import           ClassyPrelude
import qualified Data.Validation as Validation
import qualified Domain.User     as Domain

class Monad m =>
      UserRepo m
  where
  getUserByID :: Text -> m (Maybe Domain.User)
  getUserByEmail :: Text -> m (Maybe Domain.User)
  getUserByName :: Text -> m (Maybe Domain.User)
  getUserByEmailAndHashedPassword :: Text -> Text -> m (Maybe Domain.User)

class Monad m =>
      Logger m
    where
    log :: Show a => [a] -> m ()

class Monad m =>
      UUIDGen m
    where
    genUUID :: m Text

class Monad m =>
      EmailChecker m
    where
    checkEmailFormat :: Text -> m (Validation.Validation [Domain.Error] ())

class Monad m =>
      Hasher m
  where
  hashText :: Text -> m Text
