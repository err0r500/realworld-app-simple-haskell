module Http.Fixture where

import           ClassyPrelude
import qualified Adapter.Http.Router           as HttpRouter
import qualified Domain.User                   as Domain
import qualified Network.Wai                   as Wai
import qualified Usecase.LogicHandler          as UC
import qualified Adapter.InMemory.UserRepo     as InMemUserRepo
import qualified Usecase.Class                 as UC
import qualified Usecase.UserRegistration      as UC
import qualified Adapter.Logger                as Katip

instance UC.Logger App where
        log = Katip.log

newtype App a = App
    { unApp :: ReaderT (UC.LogicHandler App) IO a
    } deriving (Applicative, Functor, Monad, MonadReader (UC.LogicHandler App), MonadIO)

app :: UC.LogicHandler App -> IO Wai.Application
app logicHandler = do
        let runner = flip runReaderT logicHandler . unApp
        HttpRouter.start logicHandler runner

emptyFixture :: UC.LogicHandler App
emptyFixture = UC.LogicHandler { UC.userRegister_ = const unimplemented
                               , UC.userLogin_    = const unimplemented
                               }

unimplemented :: a
unimplemented = error "unimplemented"


