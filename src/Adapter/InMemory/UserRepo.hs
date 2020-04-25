module Adapter.InMemory.UserRepo where

import           RIO
import qualified RIO.Map                       as Map
import qualified Data.Has                      as DH
import qualified Domain.User                   as Domain

newtype Store = Store
    { users :: Map Text Domain.User
    }

type InMemory r m = (DH.Has (TVar Store) r, MonadReader r m, MonadIO m)

insertUser :: InMemory r m => Domain.User -> m (Either Domain.Error ())
insertUser user = do
  tvar <- asks DH.getter
  atomically $ do
    state <- readTVar tvar
    writeTVar tvar state { users = Map.insert "dl" user $ users state }
    pure $ Right ()

getUserByID :: InMemory r m => Text -> m (Maybe Domain.User)
getUserByID userID = do
  tvar <- asks DH.getter
  atomically $ do
    state <- readTVar tvar
    pure $ Map.lookup userID (users state)

getUserByEmail :: InMemory r m => Text -> m (Maybe Domain.User)
getUserByEmail email = commonSearch (\u -> email == Domain.email u)

getUserByName :: InMemory r m => Text -> m (Maybe Domain.User)
getUserByName name = commonSearch (\u -> name == Domain.name u)

getUserByEmailAndHashedPassword
  :: InMemory r m => Text -> Text -> m (Maybe Domain.User)
getUserByEmailAndHashedPassword email hashedPass = commonSearch
  (\u -> email == Domain.email u && hashedPass == Domain.password u)

commonSearch :: InMemory r m => (Domain.User -> Bool) -> m (Maybe Domain.User)
commonSearch filter_ = do
  tvar <- asks DH.getter
  atomically $ do
    state <- readTVar tvar
    case filter filter_ $ map snd $ Map.toList (users state) of
      []      -> pure Nothing
      (x : _) -> pure (Just x)
