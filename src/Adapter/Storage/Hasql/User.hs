{-# LANGUAGE QuasiQuotes #-}

module Adapter.Storage.Hasql.User where

import           RIO
import           Data.UUID                      ( UUID
                                                , fromText
                                                )

import           Hasql.Statement                ( Statement )
import qualified Hasql.TH                      as TH
import qualified Hasql.Connection              as Connection
import qualified Hasql.Session                 as Session

import qualified Domain.User                   as D
import qualified Domain.Messages               as D
import qualified Usecase.Interactor            as UC

insertUser :: Monad m => Connection.Connection -> Text -> Text -> Text -> Text -> m (Either D.Error ())
insertUser _ uid' name' email' password' = pure $ Right ()


getUserByID :: (MonadIO m, UC.Logger m) => Connection.Connection -> Text -> m (Maybe D.User)
getUserByID conn userID = case fromText userID of
  Nothing -> do
    UC.log [D.ErrorMsg (userID <> " is not a valid UUID")]
    pure Nothing
  Just uuid -> do
    result <- liftIO $ Session.run (Session.statement uuid selectUserByID) conn
    case result of
      Right (Just (a, b)) -> pure $ Just D.User {D._id = userID, D._name = a, D._email = b}
      Right Nothing       -> pure Nothing
      Left  err           -> do
        UC.log [D.ErrorMsg err]
        pure Nothing


selectUserByID :: Statement UUID (Maybe (Text, Text))
selectUserByID = [TH.maybeStatement|
    select name :: text, email :: text
    from "users"
    where uid = $1 :: uuid
    |]
