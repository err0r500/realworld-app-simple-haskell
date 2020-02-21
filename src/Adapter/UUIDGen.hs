module Adapter.UUIDGen where

import           ClassyPrelude

import qualified Data.UUID                     as UUID
import qualified Data.UUID.V4                  as UUID

newtype UUIDGen = UUIDGen
    { _uuid :: Text
    }

genUUIDv4 :: MonadIO m => m Text
genUUIDv4 = liftIO $ UUID.toText <$> UUID.nextRandom

genUUIDFake :: Monad m => (Text) -> m Text
genUUIDFake t = pure t
