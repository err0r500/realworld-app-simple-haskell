module Http.Specs.PostRegisterUser where

import           RIO

import           Test.Hspec
import           Test.Hspec.Wai

import qualified Http.Lib                      as Lib
import           Domain.User                   as D
import qualified Usecase.LogicHandler          as UC

spec :: Lib.StartRouter -> Spec
spec start = do
  let requestPath = "/username/example@email.com/pass"

  describe "happycase" $ do
    let allGood = Lib.emptyLogicH { UC._userRegister = \_ _ _ -> pure $ Right "myResp" }
    with (start allGood)
      $                   it "responds with 200 with the uuid"
      $                   post requestPath ""
      `shouldRespondWith` "myResp" { matchStatus = 200 }

  describe "email collision" $ do
    let coll =
          Lib.emptyLogicH { UC._userRegister = \_ _ _ -> pure $ Left [D.ErrEmailConflict] }
    with (start coll)
      $                   it "responds with 400"
      $                   post requestPath "" -- no indent
      `shouldRespondWith` "" { matchStatus = 400 }
