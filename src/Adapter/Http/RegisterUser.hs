module Adapter.Http.RegisterUser where

import           Adapter.Http.Shared
import           ClassyPrelude
import qualified Usecase.UserRegistration
import qualified Web.Scotty.Trans         as ScottyT

registerUser :: AppMonadicStack m => ScottyT.ActionT LText m ()
registerUser = do
    name <- ScottyT.param "name"
    email <- ScottyT.param "email"
    resp <- lift $ Usecase.UserRegistration.register name email
    case resp of
        Left err   -> ScottyT.json $ fromStrict $ tshow err
        Right uuid -> ScottyT.html $ fromStrict uuid
