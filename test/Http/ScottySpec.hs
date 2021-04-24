module Http.ScottySpec where

import qualified Adapter.Http.Scotty.Router as Scotty
import qualified Http.Lib as Lib
import qualified Http.Specs.Health as Health
import qualified Http.Specs.LoginUser as LoginUser
import qualified Http.Specs.RegisterUser as RegisterUser
import RIO
import Test.Hspec

start :: Lib.StartRouter
start app = Scotty.start app Lib.run

spec :: Spec
spec = do
  describe "health" $ Health.spec start
  describe "register user" $ RegisterUser.spec start
  describe "login user" $ LoginUser.spec start
