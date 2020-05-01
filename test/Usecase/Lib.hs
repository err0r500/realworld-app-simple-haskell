module Usecase.Lib where

import           RIO

import qualified Adapter.Fake.Logger           as Fake
import qualified Adapter.Storage.InMem.User    as UserRepo
import qualified Adapter.Logger                as Katip
import           Usecase.Interactor

type State = (TVar UserRepo.Store, TVar Fake.Logs)

newtype App a = App (RIO State a) deriving
  (Functor, Applicative, Monad, MonadUnliftIO, MonadThrow, MonadReader State, MonadIO)

run :: State -> App a -> IO a
run globalState (App app) = runRIO globalState app

instance Logger App where
  log = Fake.log
