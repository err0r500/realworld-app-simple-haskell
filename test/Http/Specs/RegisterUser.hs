{-# LANGUAGE QuasiQuotes #-}

module Http.Specs.RegisterUser where

import qualified Domain.User as D
import qualified Http.Lib as Lib
import qualified Http.Utils as Utils
import RIO
import Test.Hspec
import Test.Hspec.Wai
import Test.Hspec.Wai.JSON
import qualified Usecase.LogicHandler as UC
import qualified Usecase.UserRegistration as UC

spec :: Lib.StartRouter -> Spec
spec start = do
  let reqPath = "/api/users"
      reqBody =
        [json|
          {
            "user": {
              "username": "u",
              "email": "e",
              "password": "p"
            }
          }
        |]

  describe "parameter passing check" $ do
    let allGood =
          Lib.emptyLogicH
            { UC._userRegister = \u e p ->
                if u == "u" && e == "e" && p == "p"
                  then pure $ Right "myResp"
                  else pure $ Left UC.ErrTechnical
            }
    with (start allGood) $
      it "responds with 200 with the uuid" $
        Utils.postSimpleJSON reqPath reqBody
          `shouldRespondWith` "myResp" {matchStatus = 200}

  describe "happycase" $ do
    let allGood = Lib.emptyLogicH {UC._userRegister = \_ _ _ -> pure $ Right "myResp"}
    with (start allGood) $
      it "responds with 200 with the uuid" $
        Utils.postSimpleJSON reqPath reqBody
          `shouldRespondWith` "myResp" {matchStatus = 200}

  describe "email collision" $ do
    let coll = Lib.emptyLogicH {UC._userRegister = \_ _ _ -> pure $ Left (UC.ErrValidation [])}
    with (start coll) $
      it "responds with 422" $
        Utils.postSimpleJSON reqPath reqBody
          `shouldRespondWith` "" {matchStatus = 422}
