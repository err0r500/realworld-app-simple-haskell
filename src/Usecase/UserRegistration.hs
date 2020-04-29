module Usecase.UserRegistration
  ( register
  , Register
  )
where

import           RIO
import qualified Domain.User                   as D
import qualified Usecase.Interactor            as UC

type Register m = Monad m => Text -> Text -> Text -> m (Either [D.Error] Text)

register
  :: UC.Logger m
  => UC.GenUUID m
  -> UC.CheckEmailFormat m
  -> UC.GetUserByEmail m
  -> UC.GetUserByName m
  -> Register m
register genUUID checkEmail getUserByEmail getUserByName name email _ = do
  malformedEmailCheckResult <- checkEmail email
  collidingEmailCheckResult <- checkNoCollisionUserEmail getUserByEmail email
  collidingNameCheckResult  <- checkNoCollisionUserName getUserByName name

  case combine [malformedEmailCheckResult, collidingEmailCheckResult, collidingNameCheckResult] of
    []   -> Right <$> genUUID
    errs -> do
      UC.log errs
      pure $ Left errs




combine :: [Maybe [a]] -> [a]
combine = concatMap concat

checkNoCollisionUserEmail :: UC.Logger m => UC.GetUserByEmail m -> Text -> m (Maybe [D.Error])
checkNoCollisionUserEmail getUserByEmail email = do
  mayUser <- getUserByEmail email
  case mayUser of
    Left err -> do
      UC.log [err]
      pure $ Just [D.ErrTechnical]
    Right Nothing  -> pure Nothing
    Right (Just _) -> pure $ Just [D.ErrEmailConflict]

checkNoCollisionUserName :: UC.Logger m => UC.GetUserByName m -> Text -> m (Maybe [D.Error])
checkNoCollisionUserName getUserByName name = do
  mayUser <- getUserByName name
  case mayUser of
    Left err -> do
      UC.log [err]
      pure $ Just [D.ErrTechnical]
    Right Nothing  -> pure Nothing
    Right (Just _) -> pure $ Just [D.ErrNameConflict]
