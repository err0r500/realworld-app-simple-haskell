{-# LANGUAGE FlexibleInstances #-}

module Main where

import qualified Adapter.EmailChecker as EmailChecker
import qualified Adapter.Fake.Hasher as FakeHasher
import qualified Adapter.Http.Lib as SharedHttp (Router)
import qualified Adapter.Http.Scotty.Router as ScottyRouter
import qualified Adapter.Http.Servant.Router as ServantRouter
import qualified Adapter.Logger as KatipLogger
import qualified Adapter.Storage.Hasql.User as HasqlUserRepo
import qualified Adapter.Storage.InMem.User as InMemUserRepo
import qualified Adapter.UUIDGen as UUIDGen
import qualified Config.Config as Config
import qualified Hasql.Connection as Connection
import qualified Network.Wai
import qualified Network.Wai.Handler.Warp as Warp
import RIO
import qualified RIO.Text as T
import System.IO
import qualified Usecase.Interactor as UC
import qualified Usecase.LogicHandler as UC
import qualified Usecase.UserLogin as UC
import qualified Usecase.UserRegistration as UC

main :: IO ()
main = do
  putStrLn "== Haskell Clean Architecture =="

  -- we pick the HTTP server implementation (scotty or servant) and the port it'll listen on
  serverInstance <- Config.getStringFromEnv "SERVER" "servant"
  port <- Config.getIntFromEnv "PORT" 3000
  putStrLn $ "starting " ++ serverInstance ++ " server on port " ++ show port

  -- we pick the storage backend (hasql or inMem)
  storageBackend <- Config.getStringFromEnv "STORAGE" "hasql"
  putStrLn $ "using  " ++ storageBackend ++ " as storage backend"

  router <-
    case storageBackend of
      "hasql" -> do
        connSettings <- hasqlConnectionSettings
        Right conn <- Connection.acquire connSettings
        buildRouter serverInstance (hasqlUserRepo conn) ()
      _ -> do
        inMemStore <- newTVarIO $ InMemUserRepo.Store mempty
        buildRouter serverInstance inMemUserRepo inMemStore

  -- start the router
  Warp.run port router

buildRouter :: UC.Logger (App a) => String -> UC.UserRepo (App a) -> a -> IO Network.Wai.Application
buildRouter serverInstance a b =
  pickServer serverInstance (logicHandler a) $ runApp b
  where
    pickServer :: MonadThrow m => String -> SharedHttp.Router m
    pickServer serverInstance =
      case serverInstance of
        "scotty" -> ScottyRouter.start
        _ -> ServantRouter.start

-- we partially apply the "adapters" functions to get the pure usecases (shared by both storage backends)
logicHandler :: (Monad m, UC.Logger m, MonadUnliftIO m) => UC.UserRepo m -> UC.LogicHandler m
logicHandler uRepo =
  UC.LogicHandler
    ( UC.register
        UUIDGen.genUUIDv4
        EmailChecker.checkEmailFormat
        (UC._getUserByEmail uRepo)
        (UC._getUserByName uRepo)
        (UC._insertUserPswd uRepo)
    )
    ( UC.login
        FakeHasher.hash
        (UC._getUserByEmailAndHashedPassword uRepo)
    )

newtype App a b = InMemApp (RIO a b)
  deriving (Applicative, Functor, Monad, MonadThrow, MonadUnliftIO, MonadReader a, MonadIO)

runApp :: a -> App a b -> IO b
runApp inMemStore (InMemApp app) = runRIO inMemStore app

-- inMem app
type InMemStore = TVar InMemUserRepo.Store

inMemUserRepo :: InMemUserRepo.InMemory x m => UC.UserRepo m
inMemUserRepo =
  UC.UserRepo
    InMemUserRepo.insertUserPswd
    InMemUserRepo.getUserByID
    InMemUserRepo.getUserByEmail
    InMemUserRepo.getUserByName
    InMemUserRepo.getUserByEmailAndHashedPassword

-- hasql App
hasqlUserRepo :: (UC.Logger m, MonadThrow m, MonadUnliftIO m) => Connection.Connection -> UC.UserRepo m
hasqlUserRepo c =
  UC.UserRepo
    (HasqlUserRepo.insertUserPswd c)
    (HasqlUserRepo.getUserByID c)
    (HasqlUserRepo.getUserByEmail c)
    (HasqlUserRepo.getUserByName c)
    undefined

hasqlConnectionSettings :: IO Connection.Settings
hasqlConnectionSettings = do
  dbHost <- Config.getStringFromEnv "DB_HOST" "localhost"
  dbPort <- Config.getIntFromEnv "DB_PORT" 5432
  dbUser <- Config.getStringFromEnv "DB_USER" "postgres"
  dbPassword <- Config.getStringFromEnv "DB_PASSWORD" "password"
  dbName <- Config.getStringFromEnv "DB_NAME" "postgres"
  pure $ Connection.settings (bString dbHost) (fromIntegral dbPort) (bString dbUser) (bString dbPassword) (bString dbName)
  where
    bString = T.encodeUtf8 . T.pack

-- logger instances
instance UC.Logger (App a) where
  log = KatipLogger.log
