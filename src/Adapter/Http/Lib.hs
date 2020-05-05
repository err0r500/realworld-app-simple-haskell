{-# LANGUAGE RankNTypes         #-}
{-# LANGUAGE DeriveGeneric #-}

module Adapter.Http.Lib where

import           RIO

import           Data.Aeson
import           Data.List                      ( stripPrefix )
import qualified Network.Wai                   as Wai

import qualified Domain.User                   as D
import qualified Usecase.Interactor            as UC
import qualified Usecase.LogicHandler          as UC


-- the shared Router type
type Router m
  =  (MonadUnliftIO m, MonadIO m, UC.Logger m)
  => UC.LogicHandler m
  -> (forall a . m a -> IO a)
  -> IO Wai.Application


-- User (wrapper)
newtype User a = User a
  deriving (Eq, Show, Generic)

instance ToJSON a => ToJSON (User a) where
  toJSON (User a) = object ["user" .= a]

instance FromJSON a => FromJSON (User a) where
  parseJSON (Object o) = User <$> o .: "user"
  parseJSON _          = fail "user must be an object"


-- UserDetails
data UserDetails = UserDetails
  { user_username :: Text
  , user_email    :: Text
  } deriving (Eq, Show, Generic)

instance FromJSON UserDetails where
  parseJSON = genericParseJSON $ stripping "user_"

instance ToJSON UserDetails where
  toJSON = genericToJSON $ stripping "user_"

fromDomain :: D.User -> UserDetails
fromDomain user = UserDetails (D._name user) (D._email user)


-- RegisterDetails
data RegisterDetails = RegisterDetails
  { register_email    :: Text
  , register_username :: Text
  , register_password :: Text
  } deriving (Eq, Show, Generic)

instance FromJSON RegisterDetails where
  parseJSON = genericParseJSON $ stripping "register_"

instance ToJSON RegisterDetails where
  toJSON = genericToJSON $ stripping "register_"


-- LoginDetails
data LoginDetails = LoginDetails
  { login_email    :: Text
  , login_password :: Text
  } deriving (Eq, Show, Generic)

instance FromJSON LoginDetails where
  parseJSON = genericParseJSON $ stripping "login_"

instance ToJSON LoginDetails where
  toJSON = genericToJSON $ stripping "register_"


-- helper
-- removes the given prefix from field names
stripping :: String -> Options
stripping str = defaultOptions { fieldLabelModifier = strip str }
  where strip pref s = fromMaybe "" (stripPrefix pref s)
