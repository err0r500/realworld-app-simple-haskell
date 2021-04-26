{-# LANGUAGE RankNTypes #-}

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
  serverName <- Config.getStringFromEnv "SERVER" "servant"
  port <- Config.getIntFromEnv "PORT" 3000

  router <- do
    -- we "plug" everything, and start the router
    storageBackend <- Config.getStringFromEnv "STORAGE" "hasql"
    putStrLn $ "using  " ++ storageBackend ++ " as storage backend"
    if storageBackend == "hasql"
      then do
        connSettings <- hasqlConnectionSettings
        Right conn <- Connection.acquire connSettings
        pickServer serverName (logicHandler $ hasqlUserRepo conn) runApp
      else do
        inMemStore <- freshState -- we use the inMemory Storage
        pickServer serverName (logicHandler inMemUserRepo) $ runAppInMem inMemStore

  putStrLn $ "starting " ++ serverName ++ " server on port " ++ show port
  Warp.run port router

type InMemStore = TVar InMemUserRepo.Store

newtype InMemApp a = InMemApp (RIO InMemStore a)
  deriving (Applicative, Functor, Monad, MonadThrow, MonadUnliftIO, MonadReader InMemStore, MonadIO)

runAppInMem :: InMemStore -> InMemApp a -> IO a
runAppInMem inMemStore (InMemApp app) = runRIO inMemStore app

-- we partially apply the "adapters" functions to get the pure usecases
logicHandler :: (Monad m, UC.Logger m, MonadThrow m, MonadUnliftIO m) => UC.UserRepo m -> UC.LogicHandler m
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

freshState :: MonadIO m => m InMemStore
freshState = newTVarIO $ InMemUserRepo.Store mempty

pickServer ::
  (MonadUnliftIO m, MonadIO m, UC.Logger m, MonadThrow m) => String -> SharedHttp.Router m
pickServer str = if str == "scotty" then ScottyRouter.start else ServantRouter.start

instance UC.Logger InMemApp where
  log = KatipLogger.log

instance UC.Logger App where
  log = KatipLogger.log

inMemUserRepo :: InMemUserRepo.InMemory x m => UC.UserRepo m
inMemUserRepo =
  UC.UserRepo
    InMemUserRepo.insertUserPswd
    InMemUserRepo.getUserByID
    InMemUserRepo.getUserByEmail
    InMemUserRepo.getUserByName
    InMemUserRepo.getUserByEmailAndHashedPassword

newtype App a = App (RIO () a) deriving (Functor, Applicative, Monad, MonadIO, MonadUnliftIO, MonadThrow)

runApp :: App a -> IO a
runApp (App app) = runRIO () app

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
