module Http.HealthSpec where

import           RIO
import           Test.Hspec
import           Test.Hspec.Wai

import           Http.Lib

spec :: Spec
spec =
  describe "health check"
    $                   with (start undefined)
    $                   it "responds with 200 without body"
    $                   get "/"
    `shouldRespondWith` "" { matchStatus = 200 }
