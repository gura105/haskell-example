module TaskManager.Model.TaskSpec (spec) where

import Test.Hspec
import TaskManager.Model.Task
import qualified TaskManager.Model.Priority as Priority
import TaskManager.Model.Priority (Priority(..))
import qualified TaskManager.Model.Status as Status
import TaskManager.Model.Status (Status(..))
import Data.Time

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

  describe "Priority" $ do
    it "should convert to string correctly" $ do
      Priority.toString Low `shouldBe` "low"
      Priority.toString Medium `shouldBe` "medium"
      Priority.toString High `shouldBe` "high"

    it "should cycle through priorities" $ do
      Priority.nextPriority Low `shouldBe` Medium
      Priority.nextPriority Medium `shouldBe` High
      Priority.nextPriority High `shouldBe` Low

  describe "Status" $ do
    it "should convert to string correctly" $ do
      Status.toString Todo `shouldBe` "todo"
      Status.toString InProgress `shouldBe` "in-progress"
      Status.toString Done `shouldBe` "done"

    it "should check completion status" $ do
      Status.isCompleted Done `shouldBe` True
      Status.isCompleted Todo `shouldBe` False
      Status.isCompleted InProgress `shouldBe` False

    it "should cycle through statuses" $ do
      Status.nextStatus Todo `shouldBe` InProgress
      Status.nextStatus InProgress `shouldBe` Done
      Status.nextStatus Done `shouldBe` Todo

  describe "Task" $ do
    it "should create a new task with default values" $ do
      let task = newTask "Test Task"
      getTitle task `shouldBe` "Test Task"
      getStatus task `shouldBe` Todo
      getPriority task `shouldBe` Medium

    it "should create task with time" $ do
      let testTime = UTCTime (fromGregorian 2023 1 1) 0
      let task = newTaskWithTime "Test Task" testTime
      getTitle task `shouldBe` "Test Task"
      getCreatedAt task `shouldBe` testTime

    it "should update task status" $ do
      let task = newTask "Test Task"
      let updatedTask = setStatus Done task
      getStatus updatedTask `shouldBe` Done

    it "should update task priority" $ do
      let task = newTask "Test Task"
      let updatedTask = setPriority High task
      getPriority updatedTask `shouldBe` High

    it "should update task through updateTask function" $ do
      let task = newTask "Test Task"
      let updatedTask = updateTask (setStatus InProgress . setPriority High) task
      getStatus updatedTask `shouldBe` InProgress
      getPriority updatedTask `shouldBe` High