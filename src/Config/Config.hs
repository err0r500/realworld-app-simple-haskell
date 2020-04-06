module Config.Config
        ( getIntFromEnv
        )
where

import           System.Environment
import           ClassyPrelude

getIntFromEnv :: String -> Int -> IO Int
getIntFromEnv key defaultValue = do
        result <- tryIOError $ getEnv key
        case result of
                Left  _           -> pure defaultValue
                Right portFromEnv -> case readMay portFromEnv :: Maybe Int of
                        Just x  -> pure x
                        Nothing -> pure defaultValue

