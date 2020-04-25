module Utils where

import           RIO
import           Test.Hspec

import           Lib
import qualified Adapter.Fake.Logger           as Logger
import qualified Adapter.InMemory.UserRepo     as UserRepo
import qualified Adapter.Fake.UUID             as Uuid

-- used for tests
defaultFakeUUID :: Text
defaultFakeUUID = "uuid-1234"

-- creates an empty state
emptyState :: (MonadIO m) => m State
emptyState = do
  state  <- newTVarIO $ UserRepo.Store mempty
  logger <- newTVarIO $ Logger.Logs []
  uuid   <- newTVarIO $ Uuid.UUIDGen defaultFakeUUID
  return (state, logger)


checkLogs :: (Show a) => State -> [a] -> IO ()
checkLogs state expectedLogs = do
  logs <- run state Logger.getLogs
  length logs `shouldBe` length expectedLogs
  logs `shouldMatchList` map tshow expectedLogs

