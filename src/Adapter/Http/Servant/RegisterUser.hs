module Adapter.Http.Servant.RegisterUser where

import           RIO
import           Servant                        ( err422
                                                , err500
                                                )

import qualified Adapter.Http.Lib              as Lib
import qualified Usecase.Interactor            as UC
import qualified Usecase.UserRegistration      as UC

registerUser
  :: (MonadThrow m, MonadIO m, UC.Logger m) => UC.Register m -> Lib.RegisterDetails -> m Text
registerUser uc (Lib.RegisterDetails name email password) = do
  resp <- uc name email password
  case resp of
    Left  (UC.ErrValidation _) -> throwM err422
    Left  (UC.ErrTechnical   ) -> throwM err500
    Right uuid                 -> pure uuid
