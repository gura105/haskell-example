import Test.Hspec
import qualified TaskManager.Model.TaskSpec
import qualified TaskManager.Controller.TaskControllerSpec

main :: IO ()
main = hspec $ do
  describe "TaskManager.Model.Task" TaskManager.Model.TaskSpec.spec
  describe "TaskManager.Controller.TaskController" TaskManager.Controller.TaskControllerSpec.spec