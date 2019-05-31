module Http.HealthSpec where

import           ClassyPrelude
import           Http.Fixture
import           Test.Hspec
import           Test.Hspec.Wai

spec :: Spec
spec =
    describe "health check" $
    with (app emptyFixture) $ it "responds with 200 without body" $ get "/" `shouldRespondWith` "" {matchStatus = 200}
