module Adapter.EmailChecker where

import qualified Domain.User as D
import RIO
import qualified Text.Email.Validate as Validate

checkEmailFormat :: Monad m => D.Email -> m (Maybe ())
checkEmailFormat (D.Email email) =
  if Validate.isValid $ encodeUtf8 email then pure Nothing else pure $ Just ()
