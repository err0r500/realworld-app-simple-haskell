module Adapter.Http.Router where

import           Adapter.Http.RegisterUser
import           ClassyPrelude
import           Network.HTTP.Types             ( status200
                                                , status404
                                                )
import           Network.Wai                    ( Application
                                                , Response
                                                )
import qualified Web.Scotty.Trans              as ScottyT
import           Usecase.Class                 as UC
import qualified Usecase.LogicHandler          as UC


start
        :: (MonadIO m, UC.Logger m)
        => UC.LogicHandler m
        -> (m Response -> IO Response)
        -> IO Application
start logicHandler runner = ScottyT.scottyAppT
        runner
        (do
                ScottyT.get "/" $ ScottyT.status status200 -- health check
                ScottyT.post "/:name/:email" $ registerUser $ UC.userRegister_
                        logicHandler
                ScottyT.notFound $ ScottyT.status status404
        )
