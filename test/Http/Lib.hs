module Http.Lib where

import           RIO

import qualified Network.Wai                   as Wai

import qualified Usecase.LogicHandler          as UC
import qualified Usecase.Interactor            as UC
import qualified Usecase.UserRegistration      as UC
import qualified Adapter.Logger                as Logger

-- unit replaces State since we don't need any
newtype App a = App (RIO () a) deriving (Applicative, Functor, Monad, MonadUnliftIO, MonadThrow, MonadIO)

run :: App a -> IO a
run (App app) = runRIO () app

type StartRouter = UC.LogicHandler App -> IO Wai.Application

emptyLogicH :: UC.LogicHandler App
emptyLogicH = UC.LogicHandler { UC._userRegister = undefined, UC._userLogin = undefined }

instance UC.Logger App where
  log = Logger.log

