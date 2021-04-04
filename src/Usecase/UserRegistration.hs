module Usecase.UserRegistration
  ( register
  , Register
  , Err(..)
  , ValidationErr(..)
  ) where

import qualified Data.UUID                     as UUID
import qualified Domain.User                   as D
import           RIO
import qualified Usecase.Interactor            as UC

-- log business errors (example : get user return nothing is not an error at storage level but must be handled)

-- PUBLIC
-- the pure logic usecase signature
type Register m = Monad m => Text -> Text -> Text -> m (Either Err Text)

-- the errors that may be returned by the usecase
data Err = ErrTechnical
  | ErrValidation [ValidationErr]
    deriving (Show, Eq)
instance Exception Err

-- specific validation errors
data ValidationErr = EmailConflict
  | NameConflict
  | IdConflict
  | MalformedEmail
     deriving (Show, Eq)

-- the usecase
register
  :: (UC.Logger m, MonadThrow m, MonadUnliftIO m, Exception Err)
  => UC.GenUUID m
  -> UC.CheckEmailFormat m
  -> UC.GetUserByEmail m
  -> UC.GetUserByName m
  -> UC.InsertUserPswd m
  -> Register m
register genUUID checkEmailFn getUserByEmail getUserByName insertUserPswd name email pswd = catch

  (do
    malformedEmailRes <- checkEmail <$> checkEmailFn email
    collidingEmailRes <- checkNoCollidingEmail getUserByEmail email
    collidingNameRes  <- checkNoCollidingName getUserByName name
  -- we want to accumulate the potential errors of the previous steps
    checkValidation [malformedEmailRes, collidingEmailRes, collidingNameRes]
    uuid <- genUUID
    insertUser insertUserPswd (D.User uuid name email) pswd
    pure $ Right (UUID.toText uuid)
  )

  (\errs -> do
    err <- handleExceptions errs
    pure $ Left err
  )

-- PRIVATE
handleExceptions :: (UC.Logger m, MonadUnliftIO m, Exception Err) => Err -> m Err
handleExceptions errs = case errs of
  ErrTechnical -> do
    UC.log [errs]
    pure ErrTechnical
  ErrValidation mayVE -> do
    UC.log mayVE
    pure $ ErrValidation mayVE


checkEmail :: Maybe () -> Maybe ValidationErr
checkEmail Nothing  = Nothing
checkEmail (Just _) = Just MalformedEmail

checkNoCollidingEmail
  :: (MonadThrow m, Exception Err) => UC.GetUserByEmail m -> Text -> m (Maybe ValidationErr)
checkNoCollidingEmail getUserByEmail email = do
  mayUser <- getUserByEmail email
  case mayUser of
    Left  _        -> throwM ErrTechnical
    Right Nothing  -> pure Nothing
    Right (Just _) -> pure $ Just EmailConflict

checkNoCollidingName
  :: (MonadThrow m, Exception Err) => UC.GetUserByName m -> Text -> m (Maybe ValidationErr)
checkNoCollidingName getUserByName name = do
  mayUser <- getUserByName name
  case mayUser of
    Left  _        -> throwM ErrTechnical
    Right Nothing  -> pure Nothing
    Right (Just _) -> pure $ Just NameConflict

checkValidation :: (MonadThrow m, Exception Err) => [Maybe ValidationErr] -> m ()
checkValidation mayVE = case catMaybes mayVE of
  []   -> pure ()
  errs -> throwM (ErrValidation errs)

insertUser :: (MonadThrow m, Exception Err) => UC.InsertUserPswd m -> D.User -> Text -> m ()
insertUser insert user pswd = do
  res <- insert user pswd
  case res of
    Nothing -> pure ()
    Just _  -> throwM ErrTechnical
