module Adapter.Http.Scotty.RegisterUser where

import Adapter.Http.Lib as Lib
import Network.HTTP.Types
  ( status422,
    status500,
  )
import RIO
import RIO.Text.Lazy
import Usecase.UserRegistration as UC
import qualified Web.Scotty.Trans as ScottyT

registerUser :: MonadIO m => UC.Register m -> ScottyT.ActionT LText m ()
registerUser uc = do
  (Lib.User b) <- ScottyT.jsonData
  resp <-
    lift $
      uc (Lib.register_username b) (Lib.register_email b) (Lib.register_password b)
  case resp of
    Left (UC.ErrValidation _) -> ScottyT.status status422
    Left UC.ErrTechnical -> ScottyT.status status500
    Right uuid -> ScottyT.html $ fromStrict uuid
