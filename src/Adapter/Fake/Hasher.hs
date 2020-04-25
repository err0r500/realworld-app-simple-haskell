module Adapter.Fake.Hasher where

import           RIO
import qualified RIO.Text                      as T

hashText :: (Monad m) => Text -> m Text
hashText text = pure $ T.pack $ "hashed" ++ show text
