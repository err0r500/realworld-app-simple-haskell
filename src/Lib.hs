module Lib
        ( start
        )
where

import           ClassyPrelude
import           System.Environment
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
        putStrLn "== Haskel Clean Architecture =="
        state  <- getFreshState
        router <- HttpRouter.start (logicHandler interactor) (run state)
        port   <- getPort
        putStrLn $ "starting server on port: " ++ tshow port
        Warp.run port router


getPort :: IO Int
getPort = do
        result <- tryIOError $ getEnv "PORT"
        case result of
                Left  _           -> pure defaultPort
                Right portFromEnv -> case readMay portFromEnv :: Maybe Int of
                        Just x  -> pure x
                        Nothing -> pure defaultPort
        where defaultPort = 3000

interactor :: UC.Interactor InMemoryApp
interactor = UC.Interactor
        { UC.userRepo_         = userRepo
        , UC.checkEmailFormat_ = RealEmailChecker.checkEmailFormat
        , UC.genUUID_          = UUIDGen.genUUIDv4
        , UC.hashText_         = InMem.hashText
        }
    where
        userRepo = UC.UserRepo
                InMemUserRepo.getUserByID
                InMemUserRepo.getUserByEmail
                InMemUserRepo.getUserByName
                InMemUserRepo.getUserByEmailAndHashedPassword

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
