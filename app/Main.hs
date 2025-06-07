module Main where

import TaskManager.View.CLI
import TaskManager.Controller.TaskController

main :: IO ()
main = do
  let controller = newController
  runCLI controller