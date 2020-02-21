module Lib
        ( start
        )
where

import           ClassyPrelude
import qualified Adapter.EmailChecker          as RealEmailChecker
import qualified Adapter.Http.Router           as HttpRouter
import qualified Adapter.InMemory.UserRepo     as InMemUserRepo
import qualified Adapter.InMemory.Hasher       as InMem
import qualified Adapter.Logger                as Katip
import qualified Adapter.UUIDGen               as UUIDGen
import qualified Network.Wai.Handler.Warp      as Warp
import qualified Usecase.Class                 as UC
import qualified Usecase.LogicHandler          as UC
import qualified Usecase.UserRegistration      as UC
import qualified Usecase.UserLogin             as UC

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
        state  <- getFreshState
        router <- HttpRouter.start (logicHandler interactor) (run state)
        Warp.run 3000 router

interactor :: UC.Interactor InMemoryApp
interactor = UC.Interactor { UC.userRepo_         = userRepo
                           , UC.checkEmailFormat_ = mailChecker
                           , UC.genUUID_          = genU
                           , UC.hashText_         = InMem.hashText
                           }
    where
        userRepo = UC.UserRepo
                InMemUserRepo.getUserByID
                InMemUserRepo.getUserByEmail
                InMemUserRepo.getUserByName
                InMemUserRepo.getUserByEmailAndHashedPassword
        mailChecker = RealEmailChecker.checkEmailFormat
        genU        = UUIDGen.genUUIDv4

logicHandler :: UC.Interactor InMemoryApp -> UC.LogicHandler InMemoryApp
logicHandler i = UC.LogicHandler
        (UC.register (UC.genUUID_ i)
                     (UC.checkEmailFormat_ i)
                     (UC.getUserByEmail_ $ UC.userRepo_ i)
                     (UC.getUserByName_ $ UC.userRepo_ i)
        )
        (UC.login (UC.hashText_ i)
                  (UC.getUserByEmailAndHashedPassword_ $ UC.userRepo_ i)
        )


instance UC.Logger InMemoryApp where
        log = Katip.log
