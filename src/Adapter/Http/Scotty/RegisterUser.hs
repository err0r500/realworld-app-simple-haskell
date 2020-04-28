module Adapter.Http.Scotty.RegisterUser where

import           RIO
import           RIO.Text.Lazy
import           Network.HTTP.Types             ( status400 )

import qualified Web.Scotty.Trans              as ScottyT
import           Usecase.UserRegistration      as UC
                                                ( Register )

registerUser :: Monad m => UC.Register m -> ScottyT.ActionT LText m ()
registerUser register = do
  name     <- ScottyT.param "name"
  email    <- ScottyT.param "email"
  password <- ScottyT.param "password"
  resp     <- lift $ register name email password
  case resp of
    Left  _    -> ScottyT.status status400
    Right uuid -> ScottyT.html $ fromStrict uuid
