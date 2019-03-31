module Http.Fixture where

import           Adapter.Http.Router
import           ClassyPrelude
import           Domain.User           as Domain
import           Katip
import           Network.Wai           (Application)
import           Usecase.BusinessLogic as UC

newtype Fixture m = Fixture
    { _register :: Text -> Text -> m (Either [Domain.Error] Text)
    }

emptyFixture :: Fixture IO
emptyFixture = Fixture {_register = const unimplemented}

instance UC.UserLogic App where
    register = dispatch2 _register

newtype App a = App
    { unApp :: ReaderT (Fixture IO) (KatipContextT IO) a
    } deriving (Applicative, Functor, Monad, MonadReader (Fixture IO), MonadIO, KatipContext, Katip)

app :: Fixture IO -> IO Application
app fixture = do
    le <- initLogEnv "HAuth" "test"
    let runner = runKatipContextT le () mempty . flip runReaderT fixture . unApp
    start runner

unimplemented :: a
unimplemented = error "unimplemented"

dispatch :: (MonadIO m, MonadReader r m) => (r -> a -> IO b) -> (a -> m b)
dispatch getter param = do
    func <- asks getter
    liftIO $ func param

dispatch2 :: (MonadIO m, MonadReader r m) => (r -> a -> b -> IO c) -> (a -> b -> m c)
dispatch2 getter param1 param2 = do
    func <- asks getter
    liftIO $ func param1 param2
