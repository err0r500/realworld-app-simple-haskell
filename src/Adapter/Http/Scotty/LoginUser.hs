module Adapter.Http.Scotty.LoginUser where

import           RIO
import           Network.HTTP.Types             ( status404 )
import qualified Web.Scotty.Trans              as ScottyT

import           Adapter.Http.Lib              as Lib
import qualified Domain.User                   as D
import           Usecase.UserLogin             as UC
                                                ( Login )

loginUser :: MonadIO m => UC.Login m -> ScottyT.ActionT LText m ()
loginUser uc = do
  (Lib.User body) <- ScottyT.jsonData
  resp            <- lift $ uc $ D.LoginDetails (Lib.login_email body) (Lib.login_password body)
  case resp of
    Left  _    -> ScottyT.status status404
    Right user -> ScottyT.json $ Lib.User $ Lib.fromDomain user
