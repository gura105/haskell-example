module TaskManager.Model.Priority
  ( Priority(..)
  , fromString
  , toString
  , priorityOrder
  , nextPriority
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

toString :: Priority -> String
toString Low    = "low"
toString Medium = "medium"
toString High   = "high"

priorityOrder :: [Priority]
priorityOrder = [High, Medium, Low]

nextPriority :: Priority -> Priority
nextPriority Low    = Medium
nextPriority Medium = High
nextPriority High   = Low