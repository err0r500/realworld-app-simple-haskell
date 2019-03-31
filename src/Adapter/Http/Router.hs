module Adapter.Http.Router where

import           Adapter.Http.RegisterUser
import           Adapter.Http.Shared
import           ClassyPrelude
import           Network.Wai               (Response)
import qualified Web.Scotty.Trans          as ScottyT

start :: AppMonadicStack m => (m Response -> IO Response) -> IO ()
start runner =
    ScottyT.scottyT 3000 runner $ do
        ScottyT.post "/api/:name/:email" registerUser
        ScottyT.post "/:name/:email" registerUser
