module Http.ServantSpec where

import qualified Adapter.Http.Servant.Router as Servant
import qualified Http.Lib as Lib
import qualified Http.Specs.Health as Health
import qualified Http.Specs.LoginUser as LoginUser
import qualified Http.Specs.RegisterUser as RegisterUser
import RIO
import Test.Hspec

start :: Lib.StartRouter
start app = Servant.start app Lib.run

spec :: Spec
spec = do
  describe "health" $ Health.spec start
  describe "register user" $ RegisterUser.spec start
  describe "login user" $ LoginUser.spec start
