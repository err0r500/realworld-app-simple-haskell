{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Adapter.Storage.Hasql.User where

import qualified Data.UUID as UUID
import qualified Domain.Messages as D
import qualified Domain.User as D
import qualified Hasql.Connection as HConn
import qualified Hasql.Session as Session
import Hasql.Statement (Statement)
import qualified Hasql.TH as TH
import qualified PostgreSQL.ErrorCodes as PgErr
import RIO hiding (trace)
import qualified Usecase.Interactor as UC

execQuery :: MonadIO m => HConn.Connection -> a -> Statement a b -> m (Either Session.QueryError b)
execQuery conn param stmt = liftIO $ Session.run (Session.statement param stmt) conn

insertUserPswd :: (MonadIO m, UC.Logger m) => HConn.Connection -> UC.InsertUserPswd m
insertUserPswd conn user password = do
  result <- execQuery conn (getQueryFields user password) insertUserStmt
  case result of
    Right _ -> pure Nothing
    Left (Session.QueryError _ _ (Session.ResultError (Session.ServerError code e _ _))) ->
      if code == PgErr.unique_violation
        then do
          UC.log [D.ErrorMsg e]
          pure $ Just (UC.SpecificErr UC.InsertUserConflict)
        else handleErr e
    Left e -> handleErr e
  where
    getQueryFields :: D.User -> D.Password -> (UUID.UUID, Text, Text, Text)
    getQueryFields (D.User uid' (D.Name name') (D.Email email')) (D.Password password') = (uid', name', email', password')
    insertUserStmt =
      [TH.rowsAffectedStatement|
                      insert into "users" (uid, name, email, password)
                      values ($1::uuid, $2::text, $3::text, $4::text)
                      |]
    handleErr e = do
      UC.log [D.ErrorMsg e]
      pure $ Just UC.AnyErr

getUserByID :: (MonadIO m, UC.Logger m) => HConn.Connection -> UC.GetUserByID m
getUserByID c id' = sharedGetUserBy c id' userByIDStmt
  where
    userByIDStmt =
      [TH.maybeStatement|
            select uid :: uuid, name :: text, email :: text
            from users
            where uid = $1 :: uuid
            |]

getUserByEmail :: (MonadIO m, UC.Logger m) => HConn.Connection -> UC.GetUserByEmail m
getUserByEmail c (D.Email email') = sharedGetUserBy c email' userByEmailStmt
  where
    userByEmailStmt =
      [TH.maybeStatement|
        select uid :: uuid, name :: text, email :: text
        from users
        where email = $1 :: text
        |]

getUserByName :: (MonadIO m, UC.Logger m) => HConn.Connection -> UC.GetUserByName m
getUserByName c (D.Name name') = sharedGetUserBy c name' userByNameStmt
  where
    userByNameStmt =
      [TH.maybeStatement|
        select uid :: uuid, name :: text, email :: text
        from users
        where name = $1 :: text
        |]

sharedGetUserBy ::
  (MonadIO m, UC.Logger m) =>
  HConn.Connection ->
  a ->
  Statement a (Maybe (UUID.UUID, Text, Text)) ->
  m (Either UC.TechErrOnly (Maybe D.User))
sharedGetUserBy conn param stmt = do
  result <- execQuery conn param stmt
  case result of
    Right (Just (uuid, name, email)) ->
      pure $ Right . Just $ D.User uuid (D.Name name) (D.Email email)
    Right Nothing -> pure $ Right Nothing
    Left err -> do
      UC.log [D.ErrorMsg err]
      pure $ Left UC.AnyErr
