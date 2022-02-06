module Adapter.Http.Servant.RegisterUser where

import qualified Adapter.Http.Lib as Lib
import qualified Debug.Trace as Debug
import RIO
import Servant (err422, err500)
import qualified Usecase.Interactor as UC
import qualified Usecase.UserRegistration as UC

registerUser :: (MonadThrow m, MonadIO m, UC.Logger m) => UC.Register m -> Lib.RegisterDetails -> m Text
registerUser uc (Lib.RegisterDetails email name password) = do
  resp <- Debug.trace "http:" uc name email password
  case resp of
    Left (UC.ErrValidation ee) ->
      let _ = Debug.trace "x" ee
       in throwM err422
    Left UC.ErrTechnical -> throwM err500
    Right uuid -> pure uuid
