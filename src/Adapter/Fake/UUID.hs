module Adapter.Fake.UUID where

import qualified Data.UUID                     as UUID
import           RIO

newtype UUIDGen = UUIDGen
    { _uuid :: UUID.UUID
    }

genUUID :: Monad m => UUID.UUID -> m UUID.UUID
genUUID = pure
