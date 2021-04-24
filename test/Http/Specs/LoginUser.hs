{-# LANGUAGE QuasiQuotes #-}

module Http.Specs.LoginUser where

import qualified Data.UUID as UUID
import qualified Domain.User as D
import qualified Http.Lib as Lib
import qualified Http.Utils as Utils
import RIO
import Test.Hspec
import Test.Hspec.Wai
import Test.Hspec.Wai.JSON
import qualified Usecase.LogicHandler as UC
import qualified Usecase.UserLogin as UC
  ( Err (..),
  )
import Utils

spec :: Lib.StartRouter -> Spec
spec start = do
  let reqPath = "/api/users/login"
      user = D.User fakeUUID1 "username" "email@example.com"
      reqBody =
        [json|
                    {
                      "user": {
                        "email": "matth@example.com",
                        "password": "mypassword"
                      }
                    }
                     |]
      respBody =
        [json|
                   {
                     "user": {
                       "email": "email@example.com",
                       "username": "username"
                     }
                   }
                   |]

  describe "happy case" $ do
    let allGood = Lib.emptyLogicH {UC._userLogin = \_ -> pure $ Right user}
    with (start allGood) $
      it "responds 200 with the user" $
        Utils.postSimpleJSON reqPath reqBody
          `shouldRespondWith` respBody {matchStatus = 200}

  describe "email collision" $ do
    let coll = Lib.emptyLogicH {UC._userLogin = \_ -> pure $ Left UC.UserNotFound}
    with (start coll) $
      it "responds with 404" $
        Utils.postSimpleJSON reqPath reqBody
          `shouldRespondWith` "" {matchStatus = 404}
