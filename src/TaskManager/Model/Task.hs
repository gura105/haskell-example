module TaskManager.Model.Task
  ( Task(..)
  , TaskId
  , newTask
  , newTaskWithTime
  , emptyTask
  , updateTask
  , getTitle
  , getStatus
  , getPriority
  , getCreatedAt
  , getTaskId
  , setStatus
  , setPriority
  , setTitle
  , setTaskId
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
  } deriving (Show, Eq, Read)

emptyTask :: Task
emptyTask = Task
  { taskId = 0
  , title = ""
  , status = Todo
  , priority = Medium
  , createdAt = UTCTime (fromGregorian 1970 1 1) 0
  }

newTask :: String -> Task
newTask t = emptyTask { title = t }

newTaskWithTime :: String -> UTCTime -> Task
newTaskWithTime t time = emptyTask 
  { title = t
  , createdAt = time
  }

updateTask :: (Task -> Task) -> Task -> Task
updateTask f = f

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

setTaskId :: TaskId -> Task -> Task
setTaskId newId task = task { taskId = newId }