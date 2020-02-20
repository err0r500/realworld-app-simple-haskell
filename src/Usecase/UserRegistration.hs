module Usecase.UserRegistration
        ( register
        , Register
        )
where

import           ClassyPrelude           hiding ( log )
import           Control.Lens
import           Data.Validation
import qualified Domain.User                   as D
import qualified Usecase.Class                 as UC

type Register m = Monad m => Text -> Text -> m (Either [D.Error] Text)

register
        :: (UC.Logger m, Monad m)
        => UC.GenUUID m
        -> UC.CheckEmailFormat m
        -> UC.GetUserByEmail m
        -> UC.GetUserByName m
        -> Register m
register genUUID checkEmail getUserByEmail getUserByName name email = do
        malformedEmailCheckResult <- checkEmail email
        collidingEmailCheckResult <- checkNoCollisionUserEmail
                getUserByEmail
                email
        collidingNameCheckResult <- checkNoCollisionUserName getUserByName name
        case
                        collidingEmailCheckResult
                        <* collidingNameCheckResult
                        <* malformedEmailCheckResult
                of
                        Failure errs -> do
                                UC.log errs
                                pure $ Left errs
                        Success _ -> Right <$> genUUID

checkNoCollisionUserEmail
        :: Monad m => UC.GetUserByEmail m -> Text -> m (Validation [D.Error] ())
checkNoCollisionUserEmail getUserByEmail email = do
        mayUser <- getUserByEmail email
        case mayUser of
                Nothing -> pure $ _Success # ()
                Just _  -> pure $ _Failure # [D.ErrUserEmailAlreadyInUse]

checkNoCollisionUserName
        :: Monad m => UC.GetUserByName m -> Text -> m (Validation [D.Error] ())
checkNoCollisionUserName getUserByName name = do
        mayUser <- getUserByName name
        case mayUser of
                Nothing -> pure $ _Success # ()
                Just _  -> pure $ _Failure # [D.ErrUserNameAlreadyInUse]
