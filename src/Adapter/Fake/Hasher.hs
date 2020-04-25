module Adapter.Fake.Hasher where

import           RIO
import qualified RIO.Text                      as T

hash :: (Monad m) => Text -> m Text
hash text = pure $ T.pack $ "hashed" ++ show text
