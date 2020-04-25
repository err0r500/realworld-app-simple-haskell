module Adapter.Fake.UUID where

import           RIO

newtype UUIDGen = UUIDGen
    { _uuid :: Text
    }

genUUID :: Monad m => Text -> m Text
genUUID = pure
