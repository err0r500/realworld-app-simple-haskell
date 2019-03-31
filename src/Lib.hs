module Lib
    ( start
    ) where

import qualified Adapter.EmailChecker      as RealEmailChecker
import qualified Adapter.Http.Router       as HttpRouter
import qualified Adapter.InMemory.UserRepo as InMemUserRepo
import qualified Adapter.Logger            as Katip
import qualified Adapter.UUIDGen           as UUIDGen
import           ClassyPrelude
import qualified Usecase.Class             as UC

type UsersState = TVar InMemUserRepo.UsersState

newtype InMemoryApp a = InMemoryApp
    { unApp :: ReaderT UsersState IO a
    } deriving (Applicative, Functor, Monad, MonadReader UsersState, MonadIO)

run :: UsersState -> InMemoryApp a -> IO a
run state app = runReaderT (unApp app) state

instance UC.UserRepo InMemoryApp where
    getUserByID = InMemUserRepo.getUserByID
    getUserByName = InMemUserRepo.getUserByName
    getUserByEmail = InMemUserRepo.getUserByEmail

instance UC.Logger InMemoryApp where
    log = Katip.log

instance UC.UUIDGen InMemoryApp where
    genUUID = UUIDGen.genUUIDv4

instance UC.EmailChecker InMemoryApp where
    checkEmailFormat = RealEmailChecker.checkEmailFormat

getFreshState :: (MonadIO m) => m UsersState
getFreshState = newTVarIO $ InMemUserRepo.UsersState mempty

start :: IO ()
start = do
    state <- getFreshState
    HttpRouter.start $ run state
