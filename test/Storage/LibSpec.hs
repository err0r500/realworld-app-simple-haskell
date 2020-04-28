module Storage.LibSpec where

import           RIO

import           System.IO
import           Test.Hspec
import qualified Hasql.Connection              as Connection

import qualified Domain.User                   as D
import qualified Usecase.Interactor            as UC
import qualified Adapter.Storage.Hasql.User    as Storage
import qualified Adapter.Logger                as Fake

spec :: Spec
spec = do
  let uid = "a0eebc99-9c0b-4ef8-bb6d-6bb9bd380a11"

  describe "happy case" $ it "returns an uuid if found" $ do
    Right connection <- Connection.acquire connectionSettings
    result           <- Storage.getUserByID connection uid
    result `shouldBe` Just (D.User uid "matth" "matth@example.com")
  where connectionSettings = Connection.settings "postgres" 5432 "postgres" "example" "postgres"

instance UC.Logger IO where
  log = Fake.log

