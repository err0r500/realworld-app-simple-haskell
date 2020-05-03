module Adapter.Http.Scotty.RegisterUser where

import           RIO
import           RIO.Text.Lazy
import           Network.HTTP.Types             ( status400 )

import qualified Web.Scotty.Trans              as ScottyT
import           Adapter.Http.Lib              as Lib
import           Usecase.UserRegistration      as UC
                                                ( Register )

registerUser :: MonadIO m => UC.Register m -> ScottyT.ActionT LText m ()
registerUser register = do
  (Lib.User body) <- ScottyT.jsonData
  resp <- lift $ register (Lib.email body) (Lib.username body) (Lib.password body)
  case resp of
    Left  _    -> ScottyT.status status400
    Right uuid -> ScottyT.html $ fromStrict uuid
