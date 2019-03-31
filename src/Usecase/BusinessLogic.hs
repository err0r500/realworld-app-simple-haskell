module Usecase.BusinessLogic where

import           ClassyPrelude
import           Domain.User   as Domain

class (Monad m) =>
      UserLogic m
    where
    register :: Text -> Text -> m (Either [Domain.Error] Text)
