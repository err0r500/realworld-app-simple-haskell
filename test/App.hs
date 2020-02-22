module App where

import qualified Adapter.InMemory.Logger       as InMem
import qualified Adapter.InMemory.UserRepo     as InMem
import           ClassyPrelude
import qualified Adapter.Logger                as Katip
import           Usecase.Class

type State = (TVar InMem.UsersState, TVar InMem.Logs)

newtype InMemoryApp a = InMemoryApp
    { unApp :: ReaderT State IO a
    } deriving (Applicative, Functor, Monad, MonadReader State, MonadIO)

run :: State -> InMemoryApp a -> IO a
run globalState app = runReaderT (unApp app) globalState

instance Logger InMemoryApp where
        log = InMem.log
