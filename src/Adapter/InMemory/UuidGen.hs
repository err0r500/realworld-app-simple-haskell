module Adapter.InMemory.UuidGen where

import           ClassyPrelude
import qualified Data.Has      as DH


newtype UUIDGen = UUIDGen
    { _uuid :: Text
    }

type Fake r m = (MonadReader r m, MonadIO m, DH.Has (TVar UUIDGen) r)

genUUIDv4 :: Fake r m => m Text
genUUIDv4 = do
    tvar <- asks DH.getter
    atomically $ do
        state <- readTVar tvar
        return $ _uuid state


