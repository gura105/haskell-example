module TaskManager.Controller.TaskControllerSpec (spec) where

import Test.Hspec
import TaskManager.Controller.TaskController
import TaskManager.Model.Task
import TaskManager.Model.Status

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