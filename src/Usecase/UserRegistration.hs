module Usecase.UserRegistration
  ( register,
    Register,
    Err (..),
    ValidationErr (..),
  )
where

import Control.Monad.Except
import qualified Data.UUID as UUID
import qualified Domain.User as D
import RIO hiding (handle)
import qualified Usecase.Interactor as UC

-- PUBLIC
-- the pure logic usecase signature
type Register m = Monad m => D.Name -> D.Email -> D.Password -> m (Either Err Text)

-- the errors that may be rocList diagnostics
data Err
  = ErrTechnical
  | ErrValidation [ValidationErr]
  deriving (Show, Eq)

-- specific validation errors
data ValidationErr
  = EmailConflict
  | NameConflict
  | IdConflict
  | MalformedEmail
  deriving (Show, Eq)

instance Semigroup Err where
  ErrTechnical <> _ = ErrTechnical
  _ <> ErrTechnical = ErrTechnical
  ErrValidation aa <> ErrValidation bb = ErrValidation (aa <> bb)

instance Monoid Err where
  mempty = ErrValidation []

-- the usecase
register ::
  Monad m =>
  UC.GenUUID m ->
  UC.CheckEmailFormat m ->
  UC.GetUserByEmail m ->
  UC.GetUserByName m ->
  UC.InsertUserPswd m ->
  Register m
register genUUID checkEmailFormat getUserByEmail getUserByName insertUserPswd name email pswd =
  runExceptT $
    do
      _ <- ExceptT validateInputs
      uuid <- ExceptT $ fmap Right genUUID
      _ <- ExceptT $ storeUser uuid
      pure $ UUID.toText uuid
  where
    validateInputs =
      checkValidationErrs
        <$> filterJust
          [ handleMaybe MalformedEmail <$> checkEmailFormat email,
            handleEither EmailConflict <$> getUserByEmail email,
            handleEither NameConflict <$> getUserByName name
          ]

    storeUser uuid = handleInsertUser <$> insertUserPswd (D.User uuid name email) pswd
      where
        handleInsertUser Nothing = Right ()
        handleInsertUser (Just _) = Left ErrTechnical

-- PRIVATE
handleMaybe :: ValidationErr -> Maybe a -> Maybe Err
handleMaybe v may = case may of
  Nothing -> Nothing
  (Just _) -> Just $ ErrValidation [v]

handleEither :: ValidationErr -> Either a1 (Maybe a2) -> Maybe Err
handleEither v e = case e of
  Right Nothing -> Nothing
  Right (Just _) -> Just (ErrValidation [v])
  Left _ -> Just ErrTechnical

filterJust :: Applicative f => [f (Maybe a)] -> f [a]
filterJust mMay = catMaybes <$> sequenceA mMay

checkValidationErrs :: Monoid a => [a] -> Either a ()
checkValidationErrs ee =
  if not (null ee)
    then Left $ mconcat ee
    else Right ()
