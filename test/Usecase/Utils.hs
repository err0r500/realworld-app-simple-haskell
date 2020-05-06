module Usecase.Utils where

import           RIO
import           Test.Hspec
import           Utils

import           Usecase.Lib
import qualified Adapter.Fake.Logger           as Logger
import qualified Adapter.Storage.InMem.User    as UserRepo
import qualified Adapter.Fake.UUID             as GenUUID

-- creates an empty state
emptyState :: (MonadIO m) => m State
emptyState = do
  state  <- newTVarIO $ UserRepo.Store mempty
  logger <- newTVarIO $ Logger.Logs []
  uuid   <- newTVarIO $ GenUUID.UUIDGen fakeUUID1
  return (state, logger)


checkLogs :: (Show a) => State -> [a] -> IO ()
checkLogs state expectedLogs = do
  logs <- run state Logger.getLogs
  length logs `shouldBe` length expectedLogs
  logs `shouldMatchList` map tshow expectedLogs

