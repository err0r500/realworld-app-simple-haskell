module Adapter.Http.Servant.LoginUser where

import           RIO
import           Servant                        ( err404 )

import qualified Adapter.Http.Lib              as Lib
import qualified Domain.User                   as D
import qualified Usecase.Interactor            as UC
import qualified Usecase.UserLogin             as UC
                                                ( Login )

loginUser
  :: (MonadThrow m, MonadIO m, UC.Logger m)
  => UC.Login m
  -> Lib.LoginDetails
  -> m (Lib.User Lib.UserDetails)
loginUser uc (Lib.LoginDetails email password) = do
  resp <- uc (D.LoginDetails email password)
  case resp of
    Left  _    -> throwM err404
    Right user -> pure $ Lib.User $ Lib.fromDomain user

