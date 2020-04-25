module Lib
  ( start
  )
where

import           RIO
import           System.IO

import qualified Network.Wai.Handler.Warp      as Warp

import qualified Config.Config                 as Config
import qualified Adapter.EmailChecker          as EmailChecker
import qualified Adapter.Http.Router           as Router
import qualified Adapter.InMemory.UserRepo     as UserRepo
import qualified Adapter.Fake.Hasher           as Hasher
import qualified Adapter.Logger                as Logger
import qualified Adapter.UUIDGen               as UUIDGen

import qualified Usecase.Interactor            as UC
import qualified Usecase.LogicHandler          as UC
import qualified Usecase.UserRegistration      as UC
import qualified Usecase.UserLogin             as UC

type State = TVar UserRepo.Store

newtype App a = App (RIO State a) deriving (Applicative, Functor, Monad, MonadReader State, MonadIO)

run :: State -> App a -> IO a
run state (App app) = runRIO state app

start :: IO ()
start = do
  putStrLn "== Haskel Clean Architecture =="
  state  <- freshState
  router <- Router.start (logicHandler interactor) $ run state
  port   <- Config.getIntFromEnv "PORT" 3000
  putStrLn $ "starting server on port: " ++ show port
  Warp.run port router

interactor :: UC.Interactor App
interactor = UC.Interactor { UC.userRepo_         = userRepo
                           , UC.checkEmailFormat_ = EmailChecker.checkEmailFormat
                           , UC.genUUID_          = UUIDGen.genUUIDv4
                           , UC.hashText_         = Hasher.hashText
                           }
 where
  userRepo = UC.UserRepo UserRepo.getUserByID
                         UserRepo.getUserByEmail
                         UserRepo.getUserByName
                         UserRepo.getUserByEmailAndHashedPassword

logicHandler :: UC.Interactor App -> UC.LogicHandler App
logicHandler i = UC.LogicHandler
  (UC.register (UC.genUUID_ i)
               (UC.checkEmailFormat_ i)
               (UC.getUserByEmail_ $ UC.userRepo_ i)
               (UC.getUserByName_ $ UC.userRepo_ i)
  )
  (UC.login (UC.hashText_ i) (UC.getUserByEmailAndHashedPassword_ $ UC.userRepo_ i))


freshState :: (MonadIO m) => m State
freshState = newTVarIO $ UserRepo.Store mempty

instance UC.Logger App where
  log = Logger.log
