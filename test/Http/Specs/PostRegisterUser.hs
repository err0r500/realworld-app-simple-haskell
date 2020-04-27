module Http.Specs.PostRegisterUser where

import           RIO

import           Test.Hspec
import           Test.Hspec.Wai

import qualified Http.Lib                      as Lib
import           Domain.User                   as D
import qualified Usecase.LogicHandler          as UC

spec :: Lib.StartRouter -> Spec
spec start = do
  describe "happycase" $ do
    let allGood = Lib.emptyLogicH { UC._userRegister = \_ _ -> pure $ Right "myResp" }
    with (start allGood)
      $                   it "responds with 200 with the uuid"
      $                   post "/username/example@email.com" ""
      `shouldRespondWith` "myResp" { matchStatus = 200 }

  describe "email collision" $ do
    let coll = Lib.emptyLogicH { UC._userRegister = \_ _ -> pure $ Left [D.ErrUserEmailAlreadyInUse] }
    with (start coll)
      $                   it "responds with 400"
      $                   post "/username/example@email.com" ""
      `shouldRespondWith` "" { matchStatus = 400 }
