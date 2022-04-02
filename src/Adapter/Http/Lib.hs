{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RankNTypes #-}

module Adapter.Http.Lib where

import Data.Aeson
import Data.List (stripPrefix)
import qualified Domain.User as D
import qualified Network.Wai as Wai
import RIO
import qualified Usecase.Interactor as UC
import qualified Usecase.LogicHandler as UC

-- the shared Router type
type Router m =
  (MonadUnliftIO m, UC.Logger m) =>
  UC.LogicHandler m ->
  (forall a. m a -> IO a) ->
  IO Wai.Application

-- User (wrapper)
newtype User a = User a
  deriving (Eq, Show, Generic)

instance ToJSON a => ToJSON (User a) where
  toJSON (User a) = object ["user" .= a]

instance FromJSON a => FromJSON (User a) where
  parseJSON (Object o) = User <$> o .: "user"
  parseJSON _ = fail "user must be an object"

-- UserDetails
data UserDetails = UserDetails
  { user_username :: D.Name,
    user_email :: D.Email
  }
  deriving (Eq, Show, Generic)

instance FromJSON UserDetails where
  parseJSON = genericParseJSON $ stripping "user_"

instance ToJSON UserDetails where
  toJSON = genericToJSON $ stripping "user_"

fromDomain :: D.User -> UserDetails
fromDomain user = UserDetails (D._name user) (D._email user)

-- RegisterDetails
data RegisterDetails = RegisterDetails
  { register_email :: D.Email,
    register_username :: D.Name,
    register_password :: D.Password
  }
  deriving (Eq, Show, Generic)

instance FromJSON RegisterDetails where
  parseJSON = genericParseJSON $ stripping "register_"

instance ToJSON RegisterDetails where
  toJSON = genericToJSON $ stripping "register_"

-- LoginDetails
data LoginDetails = LoginDetails
  { login_email :: D.Email,
    login_password :: D.Password
  }
  deriving (Eq, Show, Generic)

instance FromJSON LoginDetails where
  parseJSON = genericParseJSON $ stripping "login_"

instance ToJSON LoginDetails where
  toJSON = genericToJSON $ stripping "register_"

-- helper
-- removes the given prefix from field names
stripping :: String -> Options
stripping str = defaultOptions {fieldLabelModifier = strip str}
  where
    strip pref s = fromMaybe "" (stripPrefix pref s)
