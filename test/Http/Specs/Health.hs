module Http.Specs.Health where

import           RIO
import           Test.Hspec
import           Test.Hspec.Wai

import qualified Http.Lib as Lib

spec :: Lib.StartRouter -> Spec
spec start =
  describe "health check"
    $                   with (start undefined)
    $                   it "responds with 200 without body"
    $                   get "/"
    `shouldRespondWith` "" { matchStatus = 200 }
