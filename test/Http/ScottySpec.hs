module Http.ScottySpec where

import           RIO
import           Test.Hspec

import qualified Adapter.Http.Scotty.Router    as Scotty

import qualified Http.Lib                      as Lib
import qualified Http.Specs.Health             as Health
import qualified Http.Specs.PostRegisterUser   as PostRegisterUser

start :: Lib.StartRouter
start app = Scotty.start app Lib.run

spec :: Spec
spec = do
  describe "health" $ Health.spec start
  describe "register user" $ PostRegisterUser.spec start


