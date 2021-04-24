module Adapter.EmailChecker where

import RIO
import qualified Text.Email.Validate as Validate

checkEmailFormat :: Monad m => Text -> m (Maybe ())
checkEmailFormat email =
  if Validate.isValid $ encodeUtf8 email then pure Nothing else pure $ Just ()
