module Config.Config
  ( getIntFromEnv
  )
where

import           RIO
import           System.Environment
import qualified System.IO.Error               as IOError

getIntFromEnv :: String -> Int -> IO Int
getIntFromEnv key defaultValue = do
  result <- IOError.tryIOError $ getEnv key
  case result of
    Left  _           -> pure defaultValue
    Right portFromEnv -> case readMaybe portFromEnv :: Maybe Int of
      Just x  -> pure x
      Nothing -> pure defaultValue

