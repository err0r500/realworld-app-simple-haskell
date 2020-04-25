module Usecase.UserRegistration
  ( register
  , Register
  )
where

import           RIO
import qualified Domain.User                   as D
import qualified Usecase.Interactor            as UC

type Register m = Monad m => Text -> Text -> m (Either [D.Error] Text)

register
  :: UC.Logger m
  => UC.GenUUID m
  -> UC.CheckEmailFormat m
  -> UC.GetUserByEmail m
  -> UC.GetUserByName m
  -> Register m
register genUUID checkEmail getUserByEmail getUserByName name email = do
  malformedEmailCheckResult <- checkEmail email
  collidingEmailCheckResult <- checkNoCollisionUserEmail getUserByEmail email
  collidingNameCheckResult  <- checkNoCollisionUserName getUserByName name

  case combine [malformedEmailCheckResult, collidingEmailCheckResult, collidingNameCheckResult] of
    []   -> Right <$> genUUID
    errs -> do
      UC.log errs
      pure $ Left errs

combine :: [Maybe [a]] -> [a]
combine xs = concat $ map
  (\x -> case x of
    Nothing   -> []
    Just errs -> errs
  )
  xs

checkNoCollisionUserEmail :: Monad m => UC.GetUserByEmail m -> Text -> m (Maybe [D.Error])
checkNoCollisionUserEmail getUserByEmail email = do
  mayUser <- getUserByEmail email
  case mayUser of
    Nothing -> pure $ Nothing
    Just _  -> pure $ Just [D.ErrUserEmailAlreadyInUse]

checkNoCollisionUserName :: Monad m => UC.GetUserByName m -> Text -> m (Maybe [D.Error])
checkNoCollisionUserName getUserByName name = do
  mayUser <- getUserByName name
  case mayUser of
    Nothing -> pure $ Nothing
    Just _  -> pure $ Just [D.ErrUserNameAlreadyInUse]
