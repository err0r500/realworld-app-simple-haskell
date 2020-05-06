{-# LANGUAGE RankNTypes    #-}

module Main where

import           RIO
import           System.IO

import qualified Network.Wai.Handler.Warp      as Warp

import qualified Config.Config                 as Config
import qualified Adapter.EmailChecker          as EmailChecker


import qualified Adapter.Http.Servant.Router   as ServantRouter
import qualified Adapter.Http.Scotty.Router    as ScottyRouter
import qualified Adapter.Http.Lib              as SharedHttp
                                                ( Router )

import qualified Adapter.Storage.InMem.User    as InMemUserRepo

import qualified Adapter.Fake.Hasher           as FakeHasher
import qualified Adapter.Logger                as KatipLogger
import qualified Adapter.UUIDGen               as UUIDGen

import qualified Usecase.Interactor            as UC
import qualified Usecase.LogicHandler          as UC
import qualified Usecase.UserLogin             as UC
import qualified Usecase.UserRegistration      as UC



main :: IO ()
main = do
  putStrLn "== Haskell Clean Architecture =="
  state      <- freshState -- we use the inMemory Storage

  -- we pick the HTTP server implementation (scotty or servant) and the port it'll listen on
  serverName <- Config.getStringFromEnv "SERVER" "servant"
  port       <- Config.getIntFromEnv "PORT" 3000
  putStrLn $ "starting " ++ serverName ++ " server on port " ++ show port


  -- we "plug" everything, and start the router
  router <- liftIO $ pickServer serverName logicHandler $ runApp state
  Warp.run port router


type State = TVar InMemUserRepo.Store
newtype App a = App (RIO State a) deriving (Applicative, Functor, Monad, MonadThrow, MonadUnliftIO, MonadReader State, MonadIO)

runApp :: State -> App a -> IO a
runApp state (App app) = runRIO state app

-- we partially apply the "adapters" functions to get the pure usecases
logicHandler :: UC.LogicHandler App
logicHandler = UC.LogicHandler
  (UC.register UUIDGen.genUUIDv4
               EmailChecker.checkEmailFormat
               InMemUserRepo.getUserByEmail
               InMemUserRepo.getUserByName
               InMemUserRepo.insertUserPswd
  )
  (UC.login FakeHasher.hash InMemUserRepo.getUserByEmailAndHashedPassword)

freshState :: MonadIO m => m State
freshState = newTVarIO $ InMemUserRepo.Store mempty

pickServer
  :: (MonadUnliftIO m, MonadIO m, UC.Logger m, MonadThrow m) => String -> SharedHttp.Router m
pickServer str = if str == "scotty" then ScottyRouter.start else ServantRouter.start

instance UC.Logger App where
  log = KatipLogger.log
