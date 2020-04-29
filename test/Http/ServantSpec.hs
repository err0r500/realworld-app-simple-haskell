module Http.ServantSpec where

import           RIO
import           Test.Hspec

import qualified Adapter.Http.Servant.Router   as Servant

import qualified Http.Lib                      as Lib
import qualified Http.Specs.Health             as Health
import qualified Http.Specs.PostRegisterUser   as PostRegisterUser

start :: Lib.StartRouter
start app = Servant.start app Lib.run

spec :: Spec
spec = do
  describe "health" $ Health.spec start
  describe "register user" $ PostRegisterUser.spec start


