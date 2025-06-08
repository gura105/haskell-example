module TaskManager.Model.Status
  ( Status(..)
  , fromString
  , toString
  , isCompleted
  , nextStatus
  ) where

import Data.Char (toLower)

data Status = Todo | InProgress | Done
  deriving (Show, Read, Eq, Ord)

fromString :: String -> Maybe Status
fromString s = case map toLower s of
  "todo"        -> Just Todo
  "in-progress" -> Just InProgress
  "done"        -> Just Done
  _             -> Nothing

toString :: Status -> String
toString Todo       = "todo"
toString InProgress = "in-progress"
toString Done       = "done"

isCompleted :: Status -> Bool
isCompleted Done = True
isCompleted _    = False

nextStatus :: Status -> Status
nextStatus Todo       = InProgress
nextStatus InProgress = Done
nextStatus Done       = Todo