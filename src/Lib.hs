module Lib
    ( start
    ) where

import qualified Adapter.EmailChecker      as RealEmailChecker
import qualified Adapter.Http.Router       as HttpRouter
import qualified Adapter.InMemory.UserRepo as InMemUserRepo
import qualified Adapter.Logger            as Katip
import qualified Adapter.UUIDGen           as UUIDGen
import           ClassyPrelude
import qualified Network.Wai.Handler.Warp  as Warp
import qualified Usecase.BusinessLogic     as UCLogic
import qualified Usecase.Class             as UCClasses
import qualified Usecase.UserRegistration  as UC

type UsersState = TVar InMemUserRepo.UsersState

newtype InMemoryApp a = InMemoryApp
    { unApp :: ReaderT UsersState IO a
    } deriving (Applicative, Functor, Monad, MonadReader UsersState, MonadIO)

run :: UsersState -> InMemoryApp a -> IO a
run state app = runReaderT (unApp app) state

getFreshState :: (MonadIO m) => m UsersState
getFreshState = newTVarIO $ InMemUserRepo.UsersState mempty

start :: IO ()
start = do
    state <- getFreshState
    router <- HttpRouter.start (run state)
    Warp.run 3000 router

instance UCClasses.UserRepo InMemoryApp where
    getUserByID = InMemUserRepo.getUserByID
    getUserByName = InMemUserRepo.getUserByName
    getUserByEmail = InMemUserRepo.getUserByEmail
    getUserByEmailAndHashedPassword = InMemUserRepo.getUserByEmailAndHashedPassword
    
instance UCClasses.Logger InMemoryApp where
    log = Katip.log

instance UCClasses.UUIDGen InMemoryApp where
    genUUID = UUIDGen.genUUIDv4

instance UCClasses.EmailChecker InMemoryApp where
    checkEmailFormat = RealEmailChecker.checkEmailFormat

instance UCLogic.UserLogic InMemoryApp where
    register = UC.register
