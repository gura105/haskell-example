module TaskManager.Controller.TaskController
  ( TaskController(..)
  , newController
  , addTask
  , addTaskIO
  , completeTask
  , deleteTask
  , updateTaskPriority
  , updateTaskStatus
  , updateTaskTitle
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
  } deriving (Show, Read)

newController :: TaskController
newController = TaskController [] 1

addTask :: String -> TaskController -> (TaskController, TaskId)
addTask title controller = 
  let newId = nextTaskId controller
      task = setTaskId newId (newTask title)
      newTasks = task : tasks controller
      newController = controller 
        { tasks = newTasks
        , nextTaskId = newId + 1
        }
  in (newController, newId)

addTaskIO :: String -> TaskController -> IO (TaskController, TaskId)
addTaskIO title controller = do
  currentTime <- getCurrentTime
  let newId = nextTaskId controller
      task = setTaskId newId (newTaskWithTime title currentTime)
      newTasks = task : tasks controller
      newController = controller 
        { tasks = newTasks
        , nextTaskId = newId + 1
        }
  return (newController, newId)

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

updateTaskStatus :: TaskId -> Status -> TaskController -> TaskController
updateTaskStatus taskId status controller =
  let updatedTasks = map updateIfMatch (tasks controller)
      updateIfMatch task
        | getTaskId task == taskId = setStatus status task
        | otherwise = task
  in controller { tasks = updatedTasks }

updateTaskTitle :: TaskId -> String -> TaskController -> TaskController
updateTaskTitle taskId title controller =
  let updatedTasks = map updateIfMatch (tasks controller)
      updateIfMatch task
        | getTaskId task == taskId = setTitle title task
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