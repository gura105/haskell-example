module TaskManager.Controller.TaskControllerSpec (spec) where

import Test.Hspec
import TaskManager.Controller.TaskController
import TaskManager.Model.Task
import TaskManager.Model.Priority
import TaskManager.Model.Status

spec :: Spec
spec = do
  describe "TaskController" $ do
    it "should add a new task" $ do
      let controller = newController
      let (controller', taskId) = addTask "Test Task" controller
      length (getTasks controller') `shouldBe` 1

    it "should complete a task" $ do
      let controller = newController
      let (controller', taskId) = addTask "Test Task" controller
      let controller'' = completeTask taskId controller'
      let tasks = getTasks controller''
      getStatus (head tasks) `shouldBe` Done

    it "should delete a task" $ do
      let controller = newController
      let (controller', taskId) = addTask "Test Task" controller
      let controller'' = deleteTask taskId controller'
      length (getTasks controller'') `shouldBe` 0