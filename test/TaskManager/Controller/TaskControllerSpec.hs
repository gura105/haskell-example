module TaskManager.Controller.TaskControllerSpec (spec) where

import Test.Hspec
import TaskManager.Controller.TaskController
import TaskManager.Model.Task
import TaskManager.Model.Status
import TaskManager.Model.Priority

spec :: Spec
spec = do
  describe "TaskController" $ do
    it "should add a new task" $ do
      let controller = newController
      let (controller', _) = addTask "Test Task" controller
      length (getTasks controller') `shouldBe` 1

    it "should complete a task" $ do
      let controller = newController
      let (controller', addedTaskId) = addTask "Test Task" controller
      let controller'' = completeTask addedTaskId controller'
      let taskList = getTasks controller''
      getStatus (head taskList) `shouldBe` Done

    it "should delete a task" $ do
      let controller = newController
      let (controller', addedTaskId) = addTask "Test Task" controller
      let controller'' = deleteTask addedTaskId controller'
      length (getTasks controller'') `shouldBe` 0

    it "should find task by ID" $ do
      let controller = newController
      let (controller', addedTaskId) = addTask "Test Task" controller
      let foundTask = findTaskById addedTaskId controller'
      foundTask `shouldSatisfy` (/= Nothing)
      case foundTask of
        Just task -> getTitle task `shouldBe` "Test Task"
        Nothing   -> expectationFailure "Task not found"

    it "should update task priority" $ do
      let controller = newController
      let (controller', addedTaskId) = addTask "Test Task" controller
      let controller'' = updateTaskPriority addedTaskId High controller'
      case findTaskById addedTaskId controller'' of
        Just task -> getPriority task `shouldBe` High
        Nothing   -> expectationFailure "Task not found"

    it "should update task using generic update function" $ do
      let controller = newController
      let (controller', addedTaskId) = addTask "Test Task" controller
      let controller'' = updateTaskInController addedTaskId (setStatus InProgress . setPriority High) controller'
      case findTaskById addedTaskId controller'' of
        Just task -> do
          getStatus task `shouldBe` InProgress
          getPriority task `shouldBe` High
        Nothing   -> expectationFailure "Task not found"