module Adapter.Http.Scotty.LoginUser where

import Adapter.Http.Lib as Lib
import qualified Domain.User as D
import Network.HTTP.Types
  ( status404,
    status500,
  )
import RIO
import Usecase.UserLogin as UC
import qualified Web.Scotty.Trans as ScottyT

loginUser :: MonadIO m => UC.Login m -> ScottyT.ActionT LText m ()
loginUser uc = do
  (Lib.User body) <- ScottyT.jsonData
  resp <- lift $ uc $ D.LoginDetails (Lib.login_email body) (Lib.login_password body)
  case resp of
    Left UC.UserNotFound -> ScottyT.status status404
    Left UC.ErrTech -> ScottyT.status status500
    Right user -> ScottyT.json $ Lib.User $ Lib.fromDomain user
