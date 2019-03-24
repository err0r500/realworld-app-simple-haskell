module Usecase.UserRegistration where

import           ClassyPrelude
import           Control.Lens
import           Data.Validation
import qualified Domain.User     as Domain
import           Usecase.Class

register :: (UserRepo m, Logger m, UUIDGen m) => Text -> Text -> m (Either [Domain.Error] Text)
register name email = do
    emailCheck <- checkNoCollisionUserEmail email
    nameCheck <- checkNoCollisionUserName name
    case emailCheck <* nameCheck of
        Failure errs -> return $ Left errs
        Success _    -> Right <$> genUUID

checkNoCollisionUserEmail :: UserRepo m => Text -> m (Validation [Domain.Error] ())
checkNoCollisionUserEmail email = do
    mayUser <- getUserByEmail email
    case mayUser of
        Nothing -> return $ _Success # ()
        Just _  -> return $ _Failure # [Domain.ErrUserEmailAlreadyInUse]

checkNoCollisionUserName :: UserRepo m => Text -> m (Validation [Domain.Error] ())
checkNoCollisionUserName name = do
    mayUser <- getUserByName name
    case mayUser of
        Nothing -> return $ _Success # ()
        Just _  -> return $ _Failure # [Domain.ErrUserNameAlreadyInUse]
