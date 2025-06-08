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
  , updateTaskInController
  , getTasks
  , getTask
  , findTaskById
  , mapTasks
  ) where

import Data.Time
import Data.List (find)
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
completeTask targetTaskId = updateTaskStatus targetTaskId Done

deleteTask :: TaskId -> TaskController -> TaskController
deleteTask targetTaskId controller =
  let filteredTasks = filter (\task -> getTaskId task /= targetTaskId) (tasks controller)
  in controller { tasks = filteredTasks }

updateTaskInController :: TaskId -> (Task -> Task) -> TaskController -> TaskController
updateTaskInController targetTaskId updateFunc controller =
  let updatedTasks = map updateIfMatch (tasks controller)
      updateIfMatch task
        | getTaskId task == targetTaskId = updateFunc task
        | otherwise = task
  in controller { tasks = updatedTasks }

updateTaskPriority :: TaskId -> Priority -> TaskController -> TaskController
updateTaskPriority targetTaskId newPriority = 
  updateTaskInController targetTaskId (setPriority newPriority)

updateTaskStatus :: TaskId -> Status -> TaskController -> TaskController
updateTaskStatus targetTaskId newStatus = 
  updateTaskInController targetTaskId (setStatus newStatus)

updateTaskTitle :: TaskId -> String -> TaskController -> TaskController
updateTaskTitle targetTaskId newTitle = 
  updateTaskInController targetTaskId (setTitle newTitle)

getTasks :: TaskController -> [Task]
getTasks = tasks

getTask :: TaskId -> TaskController -> Maybe Task
getTask = findTaskById

findTaskById :: TaskId -> TaskController -> Maybe Task
findTaskById targetTaskId controller =
  find (\task -> getTaskId task == targetTaskId) (tasks controller)

mapTasks :: (Task -> Task) -> TaskController -> TaskController
mapTasks f controller = controller { tasks = map f (tasks controller) }