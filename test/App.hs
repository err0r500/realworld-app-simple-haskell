module App where

import qualified Adapter.InMemory.Logger       as InMemLogger
import qualified Adapter.InMemory.UserRepo     as InMemUserRepo
import           ClassyPrelude
import           Usecase.Class

type UsersState = TVar InMemUserRepo.UsersState

type LoggerState = TVar InMemLogger.Logs

type Global = (UsersState, LoggerState)

newtype InMemoryApp a = InMemoryApp
    { unApp :: ReaderT Global IO a
    } deriving (Applicative, Functor, Monad, MonadReader Global, MonadIO)

run :: Global -> InMemoryApp a -> IO a
run globalState app = runReaderT (unApp app) globalState

instance Usecase.Class.Logger InMemoryApp where
        log = InMemLogger.log
