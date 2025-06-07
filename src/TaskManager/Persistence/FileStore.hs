module TaskManager.Persistence.FileStore
  ( saveTasksToFile
  , loadTasksFromFile
  ) where

import TaskManager.Controller.TaskController
import Data.Maybe (fromMaybe)

-- Placeholder implementation for file persistence
-- In a real implementation, this would use JSON/Aeson

saveTasksToFile :: FilePath -> TaskController -> IO ()
saveTasksToFile filePath controller = do
  writeFile filePath (show controller)
  putStrLn $ "Tasks saved to " ++ filePath

loadTasksFromFile :: FilePath -> IO TaskController
loadTasksFromFile filePath = do
  content <- readFile filePath
  -- This is a simplified implementation
  -- In practice, you'd use proper JSON parsing
  return $ fromMaybe newController (readMaybe content)
  where
    readMaybe :: String -> Maybe TaskController
    readMaybe s = case reads s of
      [(controller, "")] -> Just controller
      _                  -> Nothing