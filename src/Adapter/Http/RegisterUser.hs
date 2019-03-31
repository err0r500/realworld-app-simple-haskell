module Adapter.Http.RegisterUser where

import           ClassyPrelude
import           Network.HTTP.Types    (status400)

import qualified Usecase.BusinessLogic as UC
import qualified Web.Scotty.Trans      as ScottyT

registerUser :: (UC.UserLogic m) => ScottyT.ActionT LText m ()
registerUser = do
    name <- ScottyT.param "name"
    email <- ScottyT.param "email"
    resp <- lift $ UC.register name email
    case resp of
        Left _     -> ScottyT.status status400
        Right uuid -> ScottyT.html $ fromStrict uuid
