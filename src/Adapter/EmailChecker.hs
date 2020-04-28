module Adapter.EmailChecker where

import           RIO

import qualified Domain.User                   as D
import qualified Text.Email.Validate           as Validate

checkEmailFormat :: Monad m => Text -> m (Maybe [D.Error])
checkEmailFormat email =
  if Validate.isValid $ encodeUtf8 email then pure Nothing else pure $ Just [D.ErrMalformedEmail]
