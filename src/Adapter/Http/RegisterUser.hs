module Adapter.Http.RegisterUser where

import           ClassyPrelude
import           Network.HTTP.Types             ( status400 )

import qualified Web.Scotty.Trans              as ScottyT
import           Usecase.UserRegistration      as UC
                                                ( Register )

registerUser :: Monad m => UC.Register m -> ScottyT.ActionT LText m ()
registerUser register = do
        name  <- ScottyT.param "name"
        email <- ScottyT.param "email"
        resp  <- lift $ register name email
        case resp of
                Left  _    -> ScottyT.status status400
                Right uuid -> ScottyT.html $ fromStrict uuid
