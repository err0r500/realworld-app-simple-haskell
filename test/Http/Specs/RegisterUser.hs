{-# LANGUAGE QuasiQuotes #-}

module Http.Specs.RegisterUser where

import           RIO
import           Test.Hspec
import           Test.Hspec.Wai
import           Test.Hspec.Wai.JSON

import qualified Http.Lib                      as Lib
import qualified Http.Utils                    as Utils
import qualified Usecase.LogicHandler          as UC
import qualified Domain.User                   as D

spec :: Lib.StartRouter -> Spec
spec start = do
  let reqPath = "/api/users"
      reqBody = [json|
                      {
                        "user": {
                          "username": "matth",
                          "email": "matth@example.com",
                          "password": "mypassword"
                        }
                      }
                     |]


  describe "happycase" $ do
    let allGood = Lib.emptyLogicH { UC._userRegister = \_ _ _ -> pure $ Right "myResp" }
    with (start allGood)
      $                   it "responds with 200 with the uuid"
      $                   Utils.postSimpleJSON reqPath reqBody
      `shouldRespondWith` "myResp" { matchStatus = 200 }

  describe "email collision" $ do
    let coll = Lib.emptyLogicH { UC._userRegister = \_ _ _ -> pure $ Left [D.ErrEmailConflict] }
    with (start coll)
      $                   it "responds with 400"
      $                   Utils.postSimpleJSON reqPath reqBody
      `shouldRespondWith` "" { matchStatus = 400 }


