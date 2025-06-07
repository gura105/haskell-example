module TaskManager.Model.Status
  ( Status(..)
  , fromString
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