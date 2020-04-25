module Adapter.Http.Router where

import           Adapter.Http.RegisterUser
import           RIO

import qualified Network.HTTP.Types            as HttpTypes
import qualified Network.Wai                   as Wai
import qualified Web.Scotty.Trans              as ScottyT

import           Usecase.Interactor            as UC
import qualified Usecase.LogicHandler          as UC


start
  :: (MonadIO m, UC.Logger m)
  => UC.LogicHandler m
  -> (m Wai.Response -> IO Wai.Response)
  -> IO Wai.Application
start logicHandler runner = ScottyT.scottyAppT
  runner
  (do
    ScottyT.get "/" $ ScottyT.status HttpTypes.status200 -- health check
    ScottyT.post "/:name/:email" $ registerUser $ UC._userRegister logicHandler
    ScottyT.notFound $ ScottyT.status HttpTypes.status404
  )
