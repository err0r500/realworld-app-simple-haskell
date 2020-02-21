module App where

import qualified Adapter.EmailChecker          as RealEmailChecker
import qualified Adapter.InMemory.Logger       as InMemLogger
import qualified Adapter.InMemory.UserRepo     as InMemUserRepo
import qualified Adapter.InMemory.UuidGen      as InMemUuidGen
import qualified Adapter.InMemory.Hasher       as FakeHasher
import           ClassyPrelude
import           Usecase.Class

type UsersState = TVar InMemUserRepo.UsersState

type LoggerState = TVar InMemLogger.Logs

type UUIDGen_ = TVar InMemUuidGen.UUIDGen

type Global = (UsersState, LoggerState, UUIDGen_)

newtype InMemoryApp a = InMemoryApp
    { unApp :: ReaderT Global IO a
    } deriving (Applicative, Functor, Monad, MonadReader Global, MonadIO)

run :: Global -> InMemoryApp a -> IO a
run globalState app = runReaderT (unApp app) globalState

instance Usecase.Class.Logger InMemoryApp where
        log = InMemLogger.log
