module TaskManager.Model.TaskSpec (spec) where

import Test.Hspec
import TaskManager.Model.Task
import qualified TaskManager.Model.Priority as Priority
import TaskManager.Model.Priority (Priority(..))
import qualified TaskManager.Model.Status as Status
import TaskManager.Model.Status (Status(..))

spec :: Spec
spec = do
  describe "Priority" $ do
    it "should have proper ordering (Low < Medium < High)" $ do
      Low < Medium `shouldBe` True
      Medium < High `shouldBe` True
      Low < High `shouldBe` True

    it "should parse from string correctly" $ do
      Priority.fromString "low" `shouldBe` Just Low
      Priority.fromString "medium" `shouldBe` Just Medium
      Priority.fromString "high" `shouldBe` Just High
      Priority.fromString "invalid" `shouldBe` Nothing

  describe "Status" $ do
    it "should parse from string correctly" $ do
      Status.fromString "todo" `shouldBe` Just Todo
      Status.fromString "in-progress" `shouldBe` Just InProgress
      Status.fromString "done" `shouldBe` Just Done
      Status.fromString "invalid" `shouldBe` Nothing

  describe "Task" $ do
    it "should create a new task with default values" $ do
      let task = newTask "Test Task"
      getTitle task `shouldBe` "Test Task"
      getStatus task `shouldBe` Todo
      getPriority task `shouldBe` Medium

    it "should update task status" $ do
      let task = newTask "Test Task"
      let updatedTask = setStatus Done task
      getStatus updatedTask `shouldBe` Done

    it "should update task priority" $ do
      let task = newTask "Test Task"
      let updatedTask = setPriority High task
      getPriority updatedTask `shouldBe` High