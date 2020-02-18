{-# LANGUAGE RankNTypes #-}

module Usecase.UserRegistration
        ( register
        )
where

import           ClassyPrelude           hiding ( log )
import           Control.Lens
import           Data.Validation
import qualified Domain.User                   as Domain
import qualified Usecase.Class                 as UC

register
        :: ( UC.UserRepo m
           , UC.Logger m
           , UC.UUIDGen m
           , UC.EmailChecker m
           , Monad m
           )
        => UC.CheckEmailFormatFn m
        -> Text
        -> Text
        -> m (Either [Domain.Error] Text)
register checkEmail name email = do
        malformedEmailCheckResult <- checkEmail email
        collidingEmailCheckResult <- checkNoCollisionUserEmail email
        collidingNameCheckResult  <- checkNoCollisionUserName name
        case
                        collidingEmailCheckResult
                        <* collidingNameCheckResult
                        <* malformedEmailCheckResult
                of
                        Failure errs -> do
                                UC.log errs
                                pure $ Left errs
                        Success _ -> Right <$> UC.genUUID

checkNoCollisionUserEmail
        :: UC.UserRepo m => Text -> m (Validation [Domain.Error] ())
checkNoCollisionUserEmail email = do
        mayUser <- UC.getUserByEmail email
        case mayUser of
                Nothing -> pure $ _Success # ()
                Just _  -> pure $ _Failure # [Domain.ErrUserEmailAlreadyInUse]

checkNoCollisionUserName
        :: UC.UserRepo m => Text -> m (Validation [Domain.Error] ())
checkNoCollisionUserName name = do
        mayUser <- UC.getUserByName name
        case mayUser of
                Nothing -> pure $ _Success # ()
                Just _  -> pure $ _Failure # [Domain.ErrUserNameAlreadyInUse]
