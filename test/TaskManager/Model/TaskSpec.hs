module TaskManager.Model.TaskSpec (spec) where

import Test.Hspec
import Test.QuickCheck
import Data.Time
import TaskManager.Model.Task
import TaskManager.Model.Priority
import TaskManager.Model.Status

spec :: Spec
spec = do
  describe "Priority" $ do
    it "should have proper ordering (Low < Medium < High)" $ do
      Low < Medium `shouldBe` True
      Medium < High `shouldBe` True
      Low < High `shouldBe` True

    it "should parse from string correctly" $ do
      fromString "low" `shouldBe` Just Low
      fromString "medium" `shouldBe` Just Medium
      fromString "high" `shouldBe` Just High
      fromString "invalid" `shouldBe` Nothing

  describe "Status" $ do
    it "should parse from string correctly" $ do
      fromString "todo" `shouldBe` Just Todo
      fromString "in-progress" `shouldBe` Just InProgress
      fromString "done" `shouldBe` Just Done
      fromString "invalid" `shouldBe` Nothing

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