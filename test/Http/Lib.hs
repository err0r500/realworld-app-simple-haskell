module Http.Lib where

import           RIO

import qualified Network.Wai                   as Wai

import qualified Usecase.LogicHandler          as UC
import qualified Usecase.Interactor            as UC
import qualified Usecase.UserRegistration      as UC
import qualified Adapter.Http.Router           as Router
import qualified Adapter.Logger                as Logger

-- unit replaces State since we don't need any
newtype App a = App (RIO () a) deriving (Applicative, Functor, Monad, MonadIO)

run :: App a -> IO a
run (App app) = runRIO () app

start :: UC.LogicHandler App -> IO Wai.Application
start app = Router.start app $ run

emptyLogicH :: UC.LogicHandler App
emptyLogicH = UC.LogicHandler { UC.userRegister_ = undefined, UC.userLogin_ = undefined }

instance UC.Logger App where
  log = Logger.log

