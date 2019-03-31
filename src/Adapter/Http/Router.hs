module Adapter.Http.Router where

import           Adapter.Http.RegisterUser
import           ClassyPrelude
import           Network.HTTP.Types        (status200, status404)
import           Network.Wai               (Application, Response)
import           Usecase.BusinessLogic     as UC
import qualified Web.Scotty.Trans          as ScottyT

start :: (MonadIO m, UC.UserLogic m) => (m Response -> IO Response) -> IO Application
start runner = ScottyT.scottyAppT runner routes

routes :: (MonadIO m, UC.UserLogic m) => ScottyT.ScottyT LText m ()
routes = do
    ScottyT.get "/" $ ScottyT.status status200 -- health check
    ScottyT.post "/:name/:email" registerUser
    ScottyT.notFound $ ScottyT.status status404
