module Adapter.Storage.InMem.User where

import           RIO
import qualified RIO.Map                       as Map
import qualified Data.Has                      as DH
import qualified Domain.User                   as D

type InMemory r m = (DH.Has (TVar Store) r, MonadReader r m, MonadIO m)


type Name = Text


data User = User {
  _id :: !Text
  , _name :: !Text
  , _email :: !Text
  , _password :: !Text
  } deriving ( Show, Eq )


newtype Store = Store {
  users :: Map Text User
  }


fromDomain :: D.User -> User
fromDomain d = User (D._id d) (D._name d) (D._email d) ""


toDomain :: User -> D.User
toDomain u = D.User (_id u) (_name u) (_email u)


insertUser :: InMemory r m => Text -> Name -> Text -> Text -> m (Either D.Error ())
insertUser uid' name' email' password' = do
  tvar <- asks DH.getter
  atomically $ do
    state <- readTVar tvar
    writeTVar tvar
              state { users = Map.insert uid' (User uid' name' email' password') $ users state }
    pure $ Right ()


getUserByID :: InMemory r m => Text -> m (Maybe D.User)
getUserByID userID = do
  tvar <- asks DH.getter
  atomically $ do
    state <- readTVar tvar
    pure $ toDomain <$> Map.lookup userID (users state)


getUserByEmail :: InMemory r m => Text -> m (Maybe D.User)
getUserByEmail email' = commonSearch (\u -> email' == _email u)


getUserByName :: InMemory r m => Text -> m (Maybe D.User)
getUserByName name' = commonSearch (\u -> name' == _name u)


getUserByEmailAndHashedPassword :: InMemory r m => Text -> Text -> m (Maybe D.User)
getUserByEmailAndHashedPassword email' pass' =
  commonSearch (\u -> email' == _email u && pass' == _password u)


commonSearch :: InMemory r m => (User -> Bool) -> m (Maybe D.User)
commonSearch filter_ = do
  tvar <- asks DH.getter
  atomically $ do
    state <- readTVar tvar
    case filter filter_ $ map snd $ Map.toList (users state) of
      []      -> pure Nothing
      (x : _) -> pure (Just (toDomain x))

