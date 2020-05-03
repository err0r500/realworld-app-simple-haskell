module Adapter.Http.Servant.RegisterUser where

import           RIO
import           Servant                        ( err400 )
import           Data.Aeson

import qualified Adapter.Http.Lib              as Lib
import qualified Usecase.Interactor            as UC
import qualified Usecase.UserRegistration      as UC
                                                ( Register )

registerUser
  :: (MonadThrow m, MonadIO m, UC.Logger m) => UC.Register m -> Lib.RegisterDetails -> m Text
registerUser register (Lib.RegisterDetails name email password) = do
  resp <- register name email password
  case resp of
    Left  _    -> throwM err400
    Right uuid -> pure uuid

