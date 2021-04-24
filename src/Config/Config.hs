module Config.Config
  ( getIntFromEnv,
    getStringFromEnv,
  )
where

import RIO
import System.Environment
import qualified System.IO.Error as IOError

getIntFromEnv :: String -> Int -> IO Int
getIntFromEnv key defaultValue = do
  result <- IOError.tryIOError $ getEnv key
  case result of
    Left _ -> pure defaultValue
    Right fromEnv -> case readMaybe fromEnv :: Maybe Int of
      Just x -> pure x
      Nothing -> pure defaultValue

getStringFromEnv :: String -> String -> IO String
getStringFromEnv key defaultValue = do
  result <- IOError.tryIOError $ getEnv key
  case result of
    Left _ -> pure defaultValue
    Right fromEnv -> pure fromEnv
