module Usecase.UserRegistration
  ( register
  , Register
  )
where

import           RIO
import qualified Domain.User                   as D
import qualified Usecase.Interactor            as UC


-- PUBLIC
-- the pure logic usecase signature
type Register m = Monad m => Text -> Text -> Text -> m (Either [D.Error] Text)

-- the usecase
register
  :: (UC.Logger m, MonadThrow m, MonadUnliftIO m, Exception Err)
  => UC.GenUUID m
  -> UC.CheckEmailFormat m
  -> UC.GetUserByEmail m
  -> UC.GetUserByName m
  -> UC.InsertUserPswd m
  -> Register m
register genUUID checkEmail getUserByEmail getUserByName insertUserPswd name email pswd = catch

  (do
    malformedEmailRes <- checkEmail email
    collidingEmailRes <- checkNoCollidingEmail getUserByEmail email
    collidingNameRes  <- checkNoCollidingName getUserByName name
  -- we want to accumulate the potential errors of the previous steps
    checkValidation [malformedEmailRes, collidingEmailRes, collidingNameRes]
    uuid <- genUUID
    insertUser insertUserPswd (D.User uuid name email) pswd
    pure $ Right uuid
  )

  (\e -> do
    err <- handleExceptions e
    pure $ Left err
  )

-- PRIVATE
data Err = ErrValidation [D.Error]
  | ErrTechnical
    deriving (Show, Eq)
instance Exception Err

handleExceptions :: (UC.Logger m, MonadUnliftIO m, Exception Err) => Err -> m [D.Error]
handleExceptions e = case e of
  ErrTechnical -> do
    UC.log [e]
    pure [D.ErrTechnical]
  ErrValidation errs -> do
    UC.log errs
    pure errs

checkNoCollidingEmail
  :: (MonadThrow m, Exception Err) => UC.GetUserByEmail m -> Text -> m (Maybe [D.Error])
checkNoCollidingEmail getUserByEmail email = do
  mayUser <- getUserByEmail email
  case mayUser of
    Left  _        -> throwM ErrTechnical
    Right Nothing  -> pure Nothing
    Right (Just _) -> pure $ Just [D.ErrEmailConflict]

checkNoCollidingName
  :: (MonadThrow m, Exception Err) => UC.GetUserByName m -> Text -> m (Maybe [D.Error])
checkNoCollidingName getUserByName name = do
  mayUser <- getUserByName name
  case mayUser of
    Left  _        -> throwM ErrTechnical
    Right Nothing  -> pure Nothing
    Right (Just _) -> pure $ Just [D.ErrNameConflict]

checkValidation :: (MonadThrow m, Exception Err) => [Maybe [D.Error]] -> m ()
checkValidation errs = case combine errs of
  [] -> pure ()
  e  -> throwM (ErrValidation e)

combine :: [Maybe [a]] -> [a]
combine = concatMap concat

insertUser :: (MonadThrow m, Exception Err) => UC.InsertUserPswd m -> D.User -> Text -> m ()
insertUser insert user pswd = do
  res <- insert user pswd
  case res of
    Nothing -> pure ()
    Just _  -> throwM ErrTechnical
