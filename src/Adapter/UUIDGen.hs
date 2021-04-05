module Adapter.UUIDGen where

import qualified Data.UUID                     as UUID
import qualified Data.UUID.V4                  as UUID
import           RIO

genUUIDv4 :: MonadIO m => m UUID.UUID
genUUIDv4 = liftIO UUID.nextRandom
