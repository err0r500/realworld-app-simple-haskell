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
insertUserPswd c (D.User uid' name' email') password' = case UUID.fromText uid' of
  Nothing -> do
    UC.log [D.ErrorMsg (uid' <> " is not a valid UUID")]
    pure $ Just (UC.SpecificErr UC.InsertUserConflict) -- TODO fixme, should be removed
  Just uuid -> do
    result <- liftIO
      $ Session.run (Session.statement (uuid, name', email', password') insertUserStmt) c
    case result of
      Left e -> do
        UC.log [D.ErrorMsg e] -- TODO : actually handle conflict
        pure $ Just (UC.SpecificErr UC.InsertUserConflict)
      Right _ -> pure Nothing

getUserByID :: (MonadIO m, UC.Logger m) => HConn.Connection -> UC.GetUserByID m
getUserByID c userID = case UUID.fromText userID of
  Nothing -> do
    UC.log [D.ErrorMsg (userID <> " is not a valid UUID")]
    pure $ Right Nothing
  Just uuid -> findUserStmtRunner (Session.statement uuid userByIDStmt) c

getUserByEmail :: (MonadIO m, UC.Logger m) => HConn.Connection -> UC.GetUserByEmail m
getUserByEmail c email' = findUserStmtRunner (Session.statement email' userByEmailStmt) c

getUserByName :: (MonadIO m, UC.Logger m) => HConn.Connection -> UC.GetUserByName m
getUserByName c name' = findUserStmtRunner (Session.statement name' userByNameStmt) c

findUserStmtRunner
  :: (MonadIO m, UC.Logger m)
  => Session.Session (Maybe (UUID.UUID, Text, Text))
  -> HConn.Connection
  -> m (Either (UC.Err Void) (Maybe D.User))
findUserStmtRunner stmt c = do
  result <- liftIO $ Session.run stmt c
  case result of
    Right (Just (uuid, name, email)) ->
      pure (Right $ Just D.User { D._id = UUID.toText uuid, D._name = name, D._email = email })
    Right Nothing -> pure (Right Nothing)
    Left  err     -> do
      UC.log [D.ErrorMsg err]
      pure (Left UC.AnyErr)



-- statements
insertUserStmt :: HS.Statement (UUID.UUID, Text, Text, Text) ()
insertUserStmt = [TH.resultlessStatement|
                  insert into "users" (uid, name, email, password)
                  values ($1::uuid, $2::text, $3::text, $4::text)
                  |]


userByIDStmt :: HS.Statement UUID.UUID (Maybe (UUID.UUID, Text, Text))
userByIDStmt = [TH.maybeStatement|
    select uid :: uuid, name :: text, email :: text
    from "users"
    where uid = $1 :: uuid
    |]

userByEmailStmt :: HS.Statement Text (Maybe (UUID.UUID, Text, Text))
userByEmailStmt = [TH.maybeStatement|
    select uid :: uuid, name :: text, email :: text
    from "users"
    where email = $1 :: text
    |]


userByNameStmt :: HS.Statement Text (Maybe (UUID.UUID, Text, Text))
userByNameStmt = [TH.maybeStatement|
    select uid :: uuid, name :: text, email :: text
    from "users"
    where name = $1 :: text
    |]


-- tests only
truncateTable :: (MonadIO m, UC.Logger m) => HConn.Connection -> () -> m ()
truncateTable c _ = do
  res <- liftIO $ Session.run (Session.statement () truncateStmt) c
  case res of
    Right _   -> pure ()
    Left  err -> do
      UC.log [D.ErrorMsg err]
      pure ()


truncateStmt :: HS.Statement () ()
truncateStmt = HS.Statement "truncate table users" HE.noParams HD.noResult True
