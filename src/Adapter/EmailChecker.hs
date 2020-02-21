module Adapter.EmailChecker where

import           ClassyPrelude
import           Control.Lens
import qualified Data.Validation               as Validation
import qualified Domain.User                   as D
import qualified Text.Email.Validate           as EmailValidator


checkEmailFormat :: Monad m => Text -> m (Validation.Validation [D.Error] ())
checkEmailFormat email = if EmailValidator.isValid $ encodeUtf8 email
        then pure $ Validation._Success # ()
        else pure $ Validation._Failure # [D.ErrMalformedEmail]
