{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Redundant bracket" #-}

module Adapter.Storage.Hasql.User where

import qualified Data.UUID as UUID
import qualified Domain.Messages as D
import qualified Domain.User as D
import qualified Hasql.Connection as HConn
import qualified Hasql.Session as Session
import qualified Hasql.TH as TH
import qualified PostgreSQL.ErrorCodes as PgErr
import RIO hiding (trace)
import qualified Usecase.Interactor as UC

insertUserPswd :: (MonadIO m, UC.Logger m) => HConn.Connection -> UC.InsertUserPswd m
insertUserPswd c (D.User uid' (D.Name name') (D.Email email')) (D.Password password') = do
  result <- liftIO $ Session.run (Session.statement (uid', name', email', password') insertUserStmt) c
  case result of
    -- ugly pattern matching
    Left (Session.QueryError _ _ (Session.ResultError (Session.ServerError code e _ _))) ->
      if code == PgErr.unique_violation
        then do
          UC.log [D.ErrorMsg e]
          pure $ Just (UC.SpecificErr UC.InsertUserConflict)
        else handleErr e
    Left e -> handleErr e
    Right _ -> pure Nothing
  where
    insertUserStmt =
      [TH.resultlessStatement|
                      insert into "users" (uid, name, email, password)
                      values ($1::uuid, $2::text, $3::text, $4::text)
                      |]
    handleErr e = do
      UC.log [D.ErrorMsg e]
      pure $ Just UC.AnyErr

getUserByID :: (MonadIO m, UC.Logger m) => HConn.Connection -> UC.GetUserByID m
getUserByID c id' = sharedGetUserBy (Session.statement id' userByIDStmt) c
  where
    userByIDStmt =
      [TH.maybeStatement|
            select uid :: uuid, name :: text, email :: text
            from users
            where uid = $1 :: uuid
            |]

getUserByEmail :: (MonadIO m, UC.Logger m) => HConn.Connection -> UC.GetUserByEmail m
getUserByEmail c (D.Email email') = sharedGetUserBy (Session.statement email' userByEmailStmt) c
  where
    userByEmailStmt =
      [TH.maybeStatement|
        select uid :: uuid, name :: text, email :: text
        from users
        where email = $1 :: text
        |]

getUserByName :: (MonadIO m, UC.Logger m) => HConn.Connection -> UC.GetUserByName m
getUserByName c (D.Name name') = sharedGetUserBy (Session.statement name' userByNameStmt) c
  where
    userByNameStmt =
      [TH.maybeStatement|
        select uid :: uuid, name :: text, email :: text
        from users
        where name = $1 :: text
        |]

sharedGetUserBy ::
  (MonadIO m, UC.Logger m) =>
  Session.Session (Maybe (UUID.UUID, Text, Text)) ->
  HConn.Connection ->
  m (Either (UC.TechErrOnly) (Maybe D.User))
sharedGetUserBy stmt c = do
  result <- liftIO $ Session.run stmt c
  case result of
    Right (Just (uuid, name, email)) ->
      pure (Right $ Just $ D.User uuid (D.Name name) (D.Email email))
    Right Nothing -> pure (Right Nothing)
    Left err -> do
      UC.log [D.ErrorMsg err]
      pure (Left UC.AnyErr)
