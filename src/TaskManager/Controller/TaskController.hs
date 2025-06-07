module TaskManager.Controller.TaskController
  ( TaskController
  , newController
  , addTask
  , addTaskIO
  , completeTask
  , deleteTask
  , updateTaskPriority
  , getTasks
  , getTask
  ) where

import Data.Time
import TaskManager.Model.Task
import TaskManager.Model.Priority
import TaskManager.Model.Status

data TaskController = TaskController
  { tasks      :: [Task]
  , nextTaskId :: TaskId
  } deriving (Show)

newController :: TaskController
newController = TaskController [] 1

addTask :: String -> TaskController -> (TaskController, TaskId)
addTask title controller = 
  let taskId = nextTaskId controller
      task = (newTask title) { taskId = taskId }
      newTasks = task : tasks controller
      newController = controller 
        { tasks = newTasks
        , nextTaskId = taskId + 1
        }
  in (newController, taskId)

addTaskIO :: String -> TaskController -> IO (TaskController, TaskId)
addTaskIO title controller = do
  currentTime <- getCurrentTime
  let taskId = nextTaskId controller
      task = (newTaskWithTime title currentTime) { taskId = taskId }
      newTasks = task : tasks controller
      newController = controller 
        { tasks = newTasks
        , nextTaskId = taskId + 1
        }
  return (newController, taskId)

completeTask :: TaskId -> TaskController -> TaskController
completeTask taskId controller =
  let updatedTasks = map updateIfMatch (tasks controller)
      updateIfMatch task
        | getTaskId task == taskId = setStatus Done task
        | otherwise = task
  in controller { tasks = updatedTasks }

deleteTask :: TaskId -> TaskController -> TaskController
deleteTask taskId controller =
  let filteredTasks = filter (\task -> getTaskId task /= taskId) (tasks controller)
  in controller { tasks = filteredTasks }

updateTaskPriority :: TaskId -> Priority -> TaskController -> TaskController
updateTaskPriority taskId priority controller =
  let updatedTasks = map updateIfMatch (tasks controller)
      updateIfMatch task
        | getTaskId task == taskId = setPriority priority task
        | otherwise = task
  in controller { tasks = updatedTasks }

getTasks :: TaskController -> [Task]
getTasks = tasks

getTask :: TaskId -> TaskController -> Maybe Task
getTask taskId controller = 
  let matchingTasks = filter (\task -> getTaskId task == taskId) (tasks controller)
  in case matchingTasks of
       (task:_) -> Just task
       []       -> Nothing