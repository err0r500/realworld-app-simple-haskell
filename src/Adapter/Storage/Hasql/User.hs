{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}

module Adapter.Storage.Hasql.User where

import           RIO

import qualified Data.UUID                     as UUID

import qualified Hasql.Statement               as HS
import qualified Hasql.Encoders                as HE
import qualified Hasql.Decoders                as HD
import qualified Hasql.TH                      as TH
import qualified Hasql.Connection              as HConn
import qualified Hasql.Session                 as Session

import qualified Domain.User                   as D
import qualified Domain.Messages               as D
import qualified Usecase.Interactor            as UC

insertUserPswd :: (MonadIO m, UC.Logger m) => HConn.Connection -> UC.InsertUserPswd m
insertUserPswd conn (D.User uid' name' email') password' = case UUID.fromText uid' of
  Nothing -> do
    UC.log [D.ErrorMsg (uid' <> " is not a valid UUID")]
    pure $ Just D.ErrMalformed
  Just uuid -> do
    result <- liftIO
      $ Session.run (Session.statement (uuid, name', email', password') insertUserStmt) conn
    case result of
      Left e -> do
        UC.log [D.ErrorMsg e]
        pure $ Just D.ErrUserConflict
      Right _ -> pure Nothing


getUserByID :: (MonadIO m, UC.Logger m) => HConn.Connection -> Text -> m (Maybe D.User)
getUserByID conn userID = case UUID.fromText userID of
  Nothing -> do
    UC.log [D.ErrorMsg (userID <> " is not a valid UUID")]
    pure Nothing
  Just uuid -> do
    result <- liftIO $ Session.run (Session.statement uuid selectUserByIDStmt) conn
    handleFindUserResult result

getUserByEmail :: (MonadIO m, UC.Logger m) => HConn.Connection -> Text -> m (Maybe D.User)
getUserByEmail conn email' = do
  result <- liftIO $ Session.run (Session.statement email' selectUserByEmailStmt) conn
  handleFindUserResult result

getUserByName :: (MonadIO m, UC.Logger m) => HConn.Connection -> Text -> m (Maybe D.User)
getUserByName conn name' = do
  result <- liftIO $ Session.run (Session.statement name' selectUserByNameStmt) conn
  handleFindUserResult result

handleFindUserResult
  :: (MonadIO m, UC.Logger m)
  => Either Session.QueryError (Maybe (UUID.UUID, Text, Text))
  -> m (Maybe D.User)
handleFindUserResult result = case result of
  Right (Just (uuid, name, email)) ->
    pure $ Just D.User { D._id = UUID.toText uuid, D._name = name, D._email = email }
  Right Nothing -> pure Nothing
  Left  err     -> do
    UC.log [D.ErrorMsg err]
    pure Nothing



-- statements
insertUserStmt :: HS.Statement (UUID.UUID, Text, Text, Text) ()
insertUserStmt = [TH.resultlessStatement|
                  insert into "users" (uid, name, email, password)
                  values ($1::uuid, $2::text, $3::text, $4::text)
                  |]


selectUserByIDStmt :: HS.Statement UUID.UUID (Maybe (UUID.UUID, Text, Text))
selectUserByIDStmt = [TH.maybeStatement|
    select uid :: uuid, name :: text, email :: text
    from "users"
    where uid = $1 :: uuid
    |]

selectUserByEmailStmt :: HS.Statement Text (Maybe (UUID.UUID, Text, Text))
selectUserByEmailStmt = [TH.maybeStatement|
    select uid :: uuid, name :: text, email :: text
    from "users"
    where email = $1 :: text
    |]


selectUserByNameStmt :: HS.Statement Text (Maybe (UUID.UUID, Text, Text))
selectUserByNameStmt = [TH.maybeStatement|
    select uid :: uuid, name :: text, email :: text
    from "users"
    where name = $1 :: text
    |]


-- tests only
truncateTable :: (MonadIO m, UC.Logger m) => HConn.Connection -> m (Either () ())
truncateTable conn = do
  res <- liftIO $ Session.run (Session.statement () truncateStmt) conn
  case res of
    Right _   -> pure $ Right ()
    Left  err -> do
      UC.log [D.ErrorMsg err]
      pure $ Left ()


truncateStmt :: HS.Statement () ()
truncateStmt = HS.Statement "truncate table users" HE.noParams HD.noResult True
