module Storage.HasqlSpec where

import           RIO

import           System.IO
import           Test.Hspec
import qualified Hasql.Connection              as Connection

import qualified Domain.User                   as D
import qualified Usecase.Interactor            as UC
import qualified Adapter.Storage.Hasql.User    as Storage
import qualified Adapter.Fake.Logger           as Logger
import qualified Storage.Specs.User            as User

import qualified Storage.Lib                   as Lib


connSettings :: Connection.Settings
connSettings = Connection.settings "postgres" 5432 "postgres" "example" "postgres"

resetFunc :: Connection.Connection -> Lib.ResetFunc Lib.App
resetFunc = Storage.truncateTable

userRW :: Connection.Connection -> UC.UserRepo Lib.App
userRW c = UC.UserRepo (Storage.insertUserPswd c)
                       (Storage.getUserByID c)
                       (Storage.getUserByEmail c)
                       (Storage.getUserByName c)
                       undefined

spec :: Spec
spec = describe "User" $ do
  c <- runIO $ do
    Right conn <- Connection.acquire connSettings
    pure conn
  User.spec (userRW c) (resetFunc c)

