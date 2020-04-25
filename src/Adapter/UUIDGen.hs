module Adapter.UUIDGen where

import           RIO

import qualified Data.UUID                     as UUID
import qualified Data.UUID.V4                  as UUID

genUUIDv4 :: MonadIO m => m Text
genUUIDv4 = liftIO $ UUID.toText <$> UUID.nextRandom
