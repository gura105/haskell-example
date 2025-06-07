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
addTask taskTitle controller = 
  let newId = nextTaskId controller
      task = setTaskId newId (newTask taskTitle)
      newTasks = task : tasks controller
      updatedController = controller 
        { tasks = newTasks
        , nextTaskId = newId + 1
        }
  in (updatedController, newId)

addTaskIO :: String -> TaskController -> IO (TaskController, TaskId)
addTaskIO taskTitle controller = do
  currentTime <- getCurrentTime
  let newId = nextTaskId controller
      task = setTaskId newId (newTaskWithTime taskTitle currentTime)
      newTasks = task : tasks controller
      updatedController = controller 
        { tasks = newTasks
        , nextTaskId = newId + 1
        }
  return (updatedController, newId)

completeTask :: TaskId -> TaskController -> TaskController
completeTask targetTaskId controller =
  let updatedTasks = map updateIfMatch (tasks controller)
      updateIfMatch task
        | getTaskId task == targetTaskId = setStatus Done task
        | otherwise = task
  in controller { tasks = updatedTasks }

deleteTask :: TaskId -> TaskController -> TaskController
deleteTask targetTaskId controller =
  let filteredTasks = filter (\task -> getTaskId task /= targetTaskId) (tasks controller)
  in controller { tasks = filteredTasks }

updateTaskPriority :: TaskId -> Priority -> TaskController -> TaskController
updateTaskPriority targetTaskId newPriority controller =
  let updatedTasks = map updateIfMatch (tasks controller)
      updateIfMatch task
        | getTaskId task == targetTaskId = setPriority newPriority task
        | otherwise = task
  in controller { tasks = updatedTasks }

updateTaskStatus :: TaskId -> Status -> TaskController -> TaskController
updateTaskStatus targetTaskId newStatus controller =
  let updatedTasks = map updateIfMatch (tasks controller)
      updateIfMatch task
        | getTaskId task == targetTaskId = setStatus newStatus task
        | otherwise = task
  in controller { tasks = updatedTasks }

updateTaskTitle :: TaskId -> String -> TaskController -> TaskController
updateTaskTitle targetTaskId newTitle controller =
  let updatedTasks = map updateIfMatch (tasks controller)
      updateIfMatch task
        | getTaskId task == targetTaskId = setTitle newTitle task
        | otherwise = task
  in controller { tasks = updatedTasks }

getTasks :: TaskController -> [Task]
getTasks = tasks

getTask :: TaskId -> TaskController -> Maybe Task
getTask targetTaskId controller = 
  let matchingTasks = filter (\task -> getTaskId task == targetTaskId) (tasks controller)
  in case matchingTasks of
       (task:_) -> Just task
       []       -> Nothing