module Adapter.EmailChecker where

import           ClassyPrelude
import           Control.Lens
import qualified Data.Validation     as Validation
import           Domain.User         as Domain
import qualified Text.Email.Validate as EmailValidator

checkEmailFormat :: (Monad m, MonadIO m) => Text -> m (Validation.Validation [Domain.Error] ())
checkEmailFormat userEmail =
    if EmailValidator.isValid $ encodeUtf8 userEmail
        then pure $ Validation._Success # ()
        else pure $ Validation._Failure # [Domain.ErrMalformedEmail]
