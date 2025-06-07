module Main where

import System.Environment (getArgs)
import TaskManager.View.CLI
import TaskManager.View.TUI
import TaskManager.Controller.TaskController

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["--cli"] -> runCLIMode
    ["--tui"] -> runTUI
    [] -> runTUI  -- Default to TUI
    ["--help"] -> printHelp
    _ -> printHelp

runCLIMode :: IO ()
runCLIMode = do
  let controller = newController
  runCLI controller

printHelp :: IO ()
printHelp = do
  putStrLn "タスク管理アプリケーション"
  putStrLn ""
  putStrLn "使用方法:"
  putStrLn "  task-manager        -- リッチなTUIモードで起動 (デフォルト)"
  putStrLn "  task-manager --tui  -- TUIモードで起動"
  putStrLn "  task-manager --cli  -- 従来のCLIモードで起動"
  putStrLn "  task-manager --help -- このヘルプを表示"
  putStrLn ""
  putStrLn "TUIモード操作:"
  putStrLn "  ↑/↓, j/k : タスク選択"
  putStrLn "  a         : タスク追加"
  putStrLn "  d         : タスク削除"
  putStrLn "  c         : タスク完了"
  putStrLn "  p         : 優先度変更"
  putStrLn "  Enter     : タスク編集"
  putStrLn "  ?         : ヘルプ表示"
  putStrLn "  q         : 終了"