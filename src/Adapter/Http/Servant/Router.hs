{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE TypeOperators     #-}

module Adapter.Http.Servant.Router where

import           RIO hiding (Handler)

import           Servant

import qualified Control.Monad.Except          as Except

import qualified Adapter.Http.Lib              as Lib
import qualified Usecase.Interactor            as UC
import qualified Usecase.LogicHandler          as UC

import qualified Adapter.Http.Servant.RegisterUser as Handler



start :: (MonadIO m, UC.Logger m, MonadThrow m ) => Lib.Router m
start logicHandler runner =
  pure $
  serve (Proxy :: Proxy API) $
  hoistServer (Proxy :: Proxy API) ( ioToHandler . runner ) (server logicHandler)


type API =
  Get '[JSON] NoContent
  :<|> "api" :> "users" :> ReqBody '[JSON] (Lib.User Lib.RegisterDetails) :> Post '[PlainText] Text


server :: (MonadThrow m, MonadUnliftIO m, MonadIO m, UC.Logger m) => UC.LogicHandler m -> ServerT API m
server  logicHandler = healthH :<|> regUser
  where
    healthH = pure NoContent
    regUser (Lib.User body) = Handler.registerUser (UC._userRegister logicHandler) body


-- liftIO is not enough to catch the "throwM"s in handlers so we rebuild the handler manually
ioToHandler :: IO a -> Handler a
ioToHandler = Handler . Except.ExceptT . try

