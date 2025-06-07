module TaskManager.View.CLI
  ( runCLI
  , displayTask
  , displayTasks
  , parseCommand
  , Command(..)
  ) where

import TaskManager.Model.Task
import qualified TaskManager.Model.Priority as Priority
import TaskManager.Model.Priority (Priority(..))
import TaskManager.Model.Status
import TaskManager.Controller.TaskController

data Command
  = AddTask String
  | ListTasks
  | CompleteTask TaskId
  | DeleteTask TaskId
  | SetPriority TaskId Priority
  | Help
  | Quit
  deriving (Show, Eq)

runCLI :: TaskController -> IO ()
runCLI controller = do
  putStrLn "Task Manager CLI"
  putStrLn "Type 'help' for commands"
  cliLoop controller

cliLoop :: TaskController -> IO ()
cliLoop controller = do
  putStr "> "
  input <- getLine
  case parseCommand input of
    Just cmd -> do
      updatedController <- handleCommand cmd controller
      case cmd of
        Quit -> putStrLn "Goodbye!"
        _    -> cliLoop updatedController
    Nothing -> do
      putStrLn "Invalid command. Type 'help' for usage."
      cliLoop controller

handleCommand :: Command -> TaskController -> IO TaskController
handleCommand cmd controller = case cmd of
  AddTask taskTitle -> do
    (updatedController, newTaskId) <- addTaskIO taskTitle controller
    putStrLn $ "Added task #" ++ show newTaskId ++ ": " ++ taskTitle
    return updatedController
  
  ListTasks -> do
    displayTasks (getTasks controller)
    return controller
  
  CompleteTask targetTaskId -> do
    let updatedController = completeTask targetTaskId controller
    putStrLn $ "Completed task #" ++ show targetTaskId
    return updatedController
  
  DeleteTask targetTaskId -> do
    let updatedController = deleteTask targetTaskId controller
    putStrLn $ "Deleted task #" ++ show targetTaskId
    return updatedController
  
  SetPriority targetTaskId newPriority -> do
    let updatedController = updateTaskPriority targetTaskId newPriority controller
    putStrLn $ "Set priority of task #" ++ show targetTaskId ++ " to " ++ show newPriority
    return updatedController
  
  Help -> do
    putStrLn "Commands:"
    putStrLn "  add <title>           - Add a new task"
    putStrLn "  list                  - List all tasks"
    putStrLn "  complete <id>         - Mark task as completed"
    putStrLn "  delete <id>           - Delete a task"
    putStrLn "  priority <id> <level> - Set task priority (low/medium/high)"
    putStrLn "  help                  - Show this help"
    putStrLn "  quit                  - Exit the program"
    return controller
  
  Quit -> return controller

parseCommand :: String -> Maybe Command
parseCommand input = case words input of
  ["add"]              -> Nothing
  ("add":titleWords)   -> Just $ AddTask (unwords titleWords)
  ["list"]             -> Just ListTasks
  ["complete", idStr]  -> readTaskId idStr >>= Just . CompleteTask
  ["delete", idStr]    -> readTaskId idStr >>= Just . DeleteTask
  ["priority", idStr, priorityStr] -> do
    targetTaskId <- readTaskId idStr
    taskPriority <- Priority.fromString priorityStr
    return $ SetPriority targetTaskId taskPriority
  ["help"]             -> Just Help
  ["quit"]             -> Just Quit
  []                   -> Nothing
  _                    -> Nothing

readTaskId :: String -> Maybe TaskId
readTaskId s = case reads s of
  [(n, "")] -> Just n
  _         -> Nothing

displayTasks :: [Task] -> IO ()
displayTasks [] = putStrLn "No tasks found."
displayTasks taskList = mapM_ displayTask taskList

displayTask :: Task -> IO ()
displayTask task = do
  let statusStr = case getStatus task of
        Todo       -> "[ ]"
        InProgress -> "[~]"
        Done       -> "[✓]"
  let priorityStr = case getPriority task of
        Low    -> "L"
        Medium -> "M"
        High   -> "H"
  putStrLn $ statusStr ++ " #" ++ show (getTaskId task) 
           ++ " (" ++ priorityStr ++ ") " ++ getTitle task