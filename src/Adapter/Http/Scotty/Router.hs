module Adapter.Http.Scotty.Router where

import           RIO

import qualified Network.HTTP.Types            as HttpTypes
import qualified Web.Scotty.Trans              as ScottyT

import qualified Adapter.Http.Lib              as Lib
import           Adapter.Http.Scotty.LoginUser
import           Adapter.Http.Scotty.RegisterUser
import qualified Usecase.Interactor            as UC
import qualified Usecase.LogicHandler          as UC


start :: (MonadThrow m, MonadIO m, UC.Logger m) => Lib.Router m
start logicHandler runner = ScottyT.scottyAppT
  runner
  (do
    ScottyT.get "/" $ ScottyT.status HttpTypes.status200 -- health check
    ScottyT.post "/api/users" $ registerUser $ UC._userRegister logicHandler
    ScottyT.post "/api/users/login" $ loginUser $ UC._userLogin logicHandler
    ScottyT.notFound $ ScottyT.status HttpTypes.status404
  )
