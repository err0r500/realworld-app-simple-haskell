module Adapter.EmailChecker where

import           RIO

import qualified Domain.User                   as D
import qualified Text.Email.Validate           as EmailValidator

checkEmailFormat :: Monad m => Text -> m (Maybe [D.Error])
checkEmailFormat email = if EmailValidator.isValid $ encodeUtf8 email
  then pure Nothing
  else pure $ Just [D.ErrMalformedEmail]
