-- brittany --exactprint-only

module Http.PostRegisterUserSpec where

import           ClassyPrelude
import           Domain.User                   as Domain
import           Http.Fixture
import           Test.Hspec
import           Test.Hspec.Wai
import qualified Usecase.LogicHandler          as UC

spec :: Spec
spec = do
        describe "happycase" $ do
                let
                        allGood = emptyFixture
                                { UC.userRegister_ = \_ _ -> pure $ Right "blabla"
                                }
                with (app allGood)
                        $ it "responds with 200 with the uuid"
                        $ post "/username/example@email.com" ""
                        `shouldRespondWith` "blabla" { matchStatus = 200 }
        describe "email collision" $ do
                let
                        coll = emptyFixture
                                { UC.userRegister_ = \_ _ -> pure $ Left [ Domain.ErrUserEmailAlreadyInUse ]
                                }
                with (app coll)
                        $ it "responds with 400"
                        $ post "/username/example@email.com" ""
                        `shouldRespondWith` "" { matchStatus = 400 }
