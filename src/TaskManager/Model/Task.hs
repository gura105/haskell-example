module TaskManager.Model.Task
  ( Task
  , TaskId
  , newTask
  , newTaskWithTime
  , getTitle
  , getStatus
  , getPriority
  , getCreatedAt
  , getTaskId
  , setStatus
  , setPriority
  , setTitle
  ) where

import Data.Time
import TaskManager.Model.Priority
import TaskManager.Model.Status

type TaskId = Int

data Task = Task
  { taskId     :: TaskId
  , title      :: String
  , status     :: Status
  , priority   :: Priority
  , createdAt  :: UTCTime
  } deriving (Show, Eq)

newTask :: String -> Task
newTask t = Task
  { taskId = 0  -- Will be assigned by controller
  , title = t
  , status = Todo
  , priority = Medium
  , createdAt = UTCTime (fromGregorian 1970 1 1) 0  -- Will be set properly
  }

newTaskWithTime :: String -> UTCTime -> Task
newTaskWithTime t time = Task
  { taskId = 0  -- Will be assigned by controller
  , title = t
  , status = Todo
  , priority = Medium
  , createdAt = time
  }

getTitle :: Task -> String
getTitle = title

getStatus :: Task -> Status
getStatus = status

getPriority :: Task -> Priority
getPriority = priority

getCreatedAt :: Task -> UTCTime
getCreatedAt = createdAt

getTaskId :: Task -> TaskId
getTaskId = taskId

setStatus :: Status -> Task -> Task
setStatus s task = task { status = s }

setPriority :: Priority -> Task -> Task
setPriority p task = task { priority = p }

setTitle :: String -> Task -> Task
setTitle t task = task { title = t }