module Storage.HasqlUserSpec where

import           RIO

import           System.IO
import           Test.Hspec
import qualified Hasql.Connection              as Connection

import qualified Domain.User                   as D
import qualified Usecase.Interactor            as UC
import qualified Adapter.Storage.Hasql.User    as Storage
import qualified Adapter.Fake.Logger           as Logger

type Logs = TVar Logger.Logs
newtype App a = App (RIO Logs a) deriving (Functor, Applicative, Monad, MonadReader Logs, MonadIO)

instance MonadFail App where
  fail = fail

instance UC.Logger App where
  log = Logger.log

run :: Logs -> App a -> IO a
run state (App app) = runRIO state app

emptyState :: MonadIO m => m Logs
emptyState = newTVarIO $ Logger.Logs []

connSettings :: Connection.Settings
connSettings = Connection.settings "postgres" 5432 "postgres" "example" "postgres"

truncateAndInsert :: D.User -> IO Connection.Connection
truncateAndInsert user = do
  state <- emptyState
  run state $ do
    Right conn <- liftIO $ Connection.acquire connSettings
    Right ()   <- Storage.truncateTable conn
    Nothing    <- Storage.insertUserPswd conn user ""
    pure conn


spec :: Spec
spec = do
  let uid       = "a0eebc99-9c0b-4ef8-bb6d-6bb9bd380a11"
      user      = D.User uid "matth" "matth@example.com"
      otherUser = D.User "61b4ea9a-cfdb-44cc-b40b-affffeedc14e" "other" "other@example.com"

  describe "find user by ID" $ it "succeeds" $ do
    state  <- emptyState
    conn   <- truncateAndInsert user
    result <- run state $ do
      Nothing <- Storage.insertUserPswd conn otherUser ""
      Storage.getUserByID conn uid
    result `shouldBe` Just user

  describe "find user by email" $ it "succeeds" $ do
    state  <- emptyState
    conn   <- truncateAndInsert user
    result <- run state $ do
      Nothing <- Storage.insertUserPswd conn otherUser ""
      Storage.getUserByEmail conn (D._email user)
    result `shouldBe` Just user

  describe "find user by name" $ it "succeeds" $ do
    state  <- emptyState
    conn   <- truncateAndInsert user
    result <- run state $ do
      Nothing <- Storage.insertUserPswd conn otherUser ""
      Storage.getUserByName conn (D._name user)
    result `shouldBe` Just user


  describe "2 different users" $ it "succeeds" $ do
    state  <- emptyState
    conn   <- truncateAndInsert user
    result <- run state $ Storage.insertUserPswd conn otherUser ""
    result `shouldBe` Nothing

  describe "2 users with same id" $ it "fails" $ do
    state  <- emptyState
    conn   <- truncateAndInsert user
    result <- run state $ Storage.insertUserPswd conn (otherUser { D._id = uid }) ""
    result `shouldBe` Just D.ErrUserConflict

  describe "2 users with same name" $ it "fails" $ do
    state  <- emptyState
    conn   <- truncateAndInsert user
    result <- run state $ Storage.insertUserPswd conn (otherUser { D._name = D._name user }) ""
    result `shouldBe` Just D.ErrUserConflict

  describe "2 users with same email" $ it "fails" $ do
    state  <- emptyState
    conn   <- truncateAndInsert user
    result <- run state $ Storage.insertUserPswd conn (otherUser { D._email = D._email user }) ""
    result `shouldBe` Just D.ErrUserConflict








isALeft :: Either a b -> Bool
isALeft e = case e of
  Left  _ -> True
  Right _ -> False



