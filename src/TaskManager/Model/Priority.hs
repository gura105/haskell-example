module TaskManager.Model.Priority
  ( Priority(..)
  , fromString
  ) where

import Data.Char (toLower)

data Priority = Low | Medium | High
  deriving (Show, Read, Eq, Ord)

fromString :: String -> Maybe Priority
fromString s = case map toLower s of
  "low"    -> Just Low
  "medium" -> Just Medium
  "high"   -> Just High
  _        -> Nothing