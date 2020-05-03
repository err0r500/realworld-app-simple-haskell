{-# LANGUAGE RankNTypes         #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE DeriveGeneric #-}

module Adapter.Http.Lib where

import           RIO

import           Data.Aeson
import qualified Usecase.Interactor            as UC
import qualified Usecase.LogicHandler          as UC
import qualified Network.Wai                   as Wai

type Router m
  =  (MonadUnliftIO m, MonadIO m, UC.Logger m)
  => UC.LogicHandler m
  -> (forall a . m a -> IO a)
  -> IO Wai.Application


-- TYPES

data User a = User a
  deriving (Eq, Show, Generic)

instance ToJSON a => ToJSON (User a) where
  toJSON (User a) = object ["user" .= a]

instance FromJSON a => FromJSON (User a) where
  parseJSON (Object o) = User <$> o .: "user"

data RegisterDetails = RegisterDetails
  { email    :: Text
  , username :: Text
  , password :: Text
  } deriving (Eq, Show, Generic)

instance FromJSON RegisterDetails where
  parseJSON = genericParseJSON defaultOptions

