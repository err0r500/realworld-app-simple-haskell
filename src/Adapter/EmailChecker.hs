module Adapter.EmailChecker where

import           ClassyPrelude
import           Control.Lens
import qualified Data.Validation     as Validation
import           Domain.User         as Domain
import qualified Text.Email.Validate as EmailValidator

checkEmailFormat :: (Monad m, MonadIO m) => Text -> m (Validation.Validation [Domain.Error] ())
checkEmailFormat userEmail = do
    if EmailValidator.isValid $ encodeUtf8 userEmail
        then return $ Validation._Success # ()
        else return $ Validation._Failure # [Domain.ErrMalformedEmail]
