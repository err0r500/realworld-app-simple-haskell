module Adapter.InMemory.Hasher where

import           ClassyPrelude

hashText :: Monad m => Text -> m Text
hashText text = pure $ "hashed" ++ text
