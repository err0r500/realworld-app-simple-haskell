module Adapter.InMemory.UserRepo where

import           ClassyPrelude
import qualified Data.Has             as DH
import qualified Domain.User          as Domain

newtype UsersState = UsersState
    { users :: Map String Domain.User
    }

type InMemory r m = (DH.Has (TVar UsersState) r, MonadReader r m, MonadIO m)

insertUser :: InMemory r m => Domain.User -> m (Either Domain.Error ())
insertUser user = do
    tvar <- asks DH.getter
    atomically $ do
        state <- readTVar tvar
        writeTVar tvar state {users = insertMap ("dl") user $ users state}
        pure $ Right ()

getUserByID :: InMemory r m => String -> m (Maybe Domain.User)
getUserByID userID = do
    tvar <- asks DH.getter
    atomically $ do
        state <- readTVar tvar
        pure $ lookup userID (users state)

getUserByEmail :: InMemory r m => Text -> m (Maybe Domain.User)
getUserByEmail email = commonSearch (\u -> email == Domain.email u)

getUserByName :: InMemory r m => Text -> m (Maybe Domain.User)
getUserByName name = commonSearch (\u -> name == Domain.name u)

commonSearch :: InMemory r m => (Domain.User -> Bool) -> m (Maybe Domain.User)
commonSearch filter_ = do
    tvar <- asks DH.getter
    atomically $ do
        state <- readTVar tvar
        let mayUser = filter filter_ $ toList (users state)
         in case mayUser of
                []     -> pure Nothing
                (x:_) -> pure (Just x)
