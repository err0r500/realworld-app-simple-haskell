module Usecase.Class where

import           ClassyPrelude
import qualified Domain.User   as Domain

class Monad m =>
      UserRepo m
    where
    getUserByID :: String -> m (Maybe Domain.User)
    getUserByEmail :: Text -> m (Maybe Domain.User)
    getUserByName :: Text -> m (Maybe Domain.User)

class Monad m =>
      Logger m
    where
    log' :: Show a => [a] -> m ()

class Monad m =>
      UUIDGen m
    where
    genUUID :: m Text
