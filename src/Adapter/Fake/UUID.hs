module Adapter.Fake.UUID where

import           RIO
import qualified Data.UUID                     as UUID

newtype UUIDGen = UUIDGen
    { _uuid :: UUID.UUID
    }

genUUID :: Monad m => UUID.UUID -> m UUID.UUID
genUUID = pure
