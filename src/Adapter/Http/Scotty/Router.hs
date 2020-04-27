module Adapter.Http.Scotty.Router where

import           RIO


import qualified Adapter.Http.Lib              as Lib

import qualified Network.HTTP.Types            as HttpTypes
import qualified Web.Scotty.Trans              as ScottyT

import           Usecase.Interactor            as UC
import qualified Usecase.LogicHandler          as UC

import           Adapter.Http.Scotty.RegisterUser


start :: (MonadThrow m, MonadIO m, UC.Logger m) => Lib.Router m
start logicHandler runner = ScottyT.scottyAppT
  runner
  (do
    ScottyT.get "/" $ ScottyT.status HttpTypes.status200 -- health check
    ScottyT.post "/:name/:email" $ registerUser $ UC._userRegister logicHandler
    ScottyT.notFound $ ScottyT.status HttpTypes.status404
  )
