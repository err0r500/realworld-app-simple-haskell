module Storage.HasqlSpec where

import qualified Domain.User as D
import RIO
import qualified Storage.Lib as Lib
import qualified Storage.Specs.User as UserRepo
import Test.Hspec
import qualified TestContainers.Hspec as TC
import qualified Usecase.Interactor as UC
import Utils

spec :: Spec
spec =
  aroundAll (TC.withContainers Lib.startAndConnectPostgres) $ do
    UserRepo.getUserSpec
    UserRepo.insertUserSpec
