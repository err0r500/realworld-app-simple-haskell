module Adapter.Http.Servant.RegisterUser where

import           RIO
import           Servant                        ( err400 )

import qualified Usecase.Interactor            as UC
import qualified Usecase.UserRegistration      as UC
                                                ( Register )

registerUser :: (MonadThrow m, MonadIO m, UC.Logger m) => UC.Register m -> Text -> Text -> m Text
registerUser register name email = do
  resp <- register name email
  case resp of
    Left  _    -> throwM err400
    Right uuid -> pure uuid

