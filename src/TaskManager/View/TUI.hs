{-# LANGUAGE OverloadedStrings #-}

module TaskManager.View.TUI
  ( runTUI
  ) where

import Brick
import Brick.Widgets.Border
import Brick.Widgets.Center
import Brick.Widgets.List
import qualified Graphics.Vty as V
import qualified Data.Vector as Vec
import Control.Monad

import TaskManager.Model.Task
import TaskManager.Model.Priority
import TaskManager.Model.Status
import TaskManager.Controller.TaskController

-- Application state
data AppState = AppState
  { appController :: TaskController
  , appTaskList :: List ResourceName Task
  , appMode :: AppMode
  , appInputText :: String
  }

data AppMode
  = ViewMode
  | AddMode
  | HelpMode
  deriving (Show, Eq)

data ResourceName
  = TaskListName
  deriving (Show, Eq, Ord)

-- Initial state
initialState :: AppState
initialState = AppState
  { appController = newController
  , appTaskList = list TaskListName Vec.empty 1
  , appMode = ViewMode
  , appInputText = ""
  }

-- Attribute map for styling
theMap :: AttrMap
theMap = attrMap V.defAttr
  [ (listSelectedAttr, V.black `on` V.cyan)
  , (listAttr, V.defAttr)
  , (attrName "high", fg V.red)
  , (attrName "medium", fg V.yellow)
  , (attrName "low", fg V.green)
  , (attrName "done", fg V.brightBlack)
  , (attrName "todo", V.defAttr)
  , (attrName "in-progress", fg V.cyan)
  , (attrName "header", V.defAttr `V.withStyle` V.bold)
  , (attrName "help", fg V.brightBlue)
  , (attrName "cursor", V.black `on` V.white)
  ]

-- Draw the UI
drawUI :: AppState -> [Widget ResourceName]
drawUI st = case appMode st of
  ViewMode -> [drawMainUI st]
  AddMode -> [drawInputDialog st, drawMainUI st]
  HelpMode -> [drawHelp st, drawMainUI st]

drawMainUI :: AppState -> Widget ResourceName
drawMainUI st = 
  vBox [ drawHeader
       , hBorder
       , drawTaskList st
       , hBorder
       , drawFooter st
       ]

drawHeader :: Widget ResourceName
drawHeader = 
  withAttr (attrName "header") $ 
  center $ str "📋 タスク管理 - TUI版"

drawTaskList :: AppState -> Widget ResourceName
drawTaskList st = 
  let taskList = getTasks (appController st)
      taskVec = Vec.fromList taskList
      updatedList = listReplace taskVec Nothing (appTaskList st)
      hasFocus = appMode st == ViewMode
  in vLimit 15 $ 
     renderList drawTask hasFocus updatedList

drawTask :: Bool -> Task -> Widget ResourceName
drawTask isSelected task =
  let statusIcon = case getStatus task of
        Todo -> "[ ]"
        InProgress -> "[~]"
        Done -> "[✓]"
      
      priorityStr = case getPriority task of
        Low -> "L"
        Medium -> "M"
        High -> "H"
      
      priorityAttr = case getPriority task of
        Low -> attrName "low"
        Medium -> attrName "medium"
        High -> attrName "high"
      
      statusAttr = case getStatus task of
        Todo -> attrName "todo"
        InProgress -> attrName "in-progress"
        Done -> attrName "done"
      
      taskIdStr = "#" ++ show (getTaskId task)
      titleStr = getTitle task
      
      cursor = if isSelected then str "► " else str "  "
      
      widget = hBox [ if isSelected then withAttr (attrName "cursor") cursor else cursor
                    , withAttr statusAttr $ str statusIcon
                    , str " "
                    , str taskIdStr
                    , str " "
                    , withAttr priorityAttr $ str ("(" ++ priorityStr ++ ")")
                    , str " "
                    , withAttr statusAttr $ str titleStr
                    ]
  in if isSelected
     then withAttr listSelectedAttr widget
     else widget

drawFooter :: AppState -> Widget ResourceName
drawFooter st =
  let helpText = case appMode st of
        ViewMode -> "a:追加 d:削除 c:完了 p:優先度 q:終了 ?:ヘルプ"
        _ -> "ESC:戻る"
  in withAttr (attrName "help") $ center $ str helpText

drawInputDialog :: AppState -> Widget ResourceName
drawInputDialog st =
  center $ 
  borderWithLabel (str "新しいタスク") $
  hLimit 50 $ vLimit 7 $
  vBox [ str "タスク名を入力してください:"
       , str " "
       , str (appInputText st ++ "_")
       , str " "
       , hCenter $ hBox [ str "[Enter]追加  [ESC]キャンセル" ]
       ]

drawHelp :: AppState -> Widget ResourceName
drawHelp _ =
  center $ 
  borderWithLabel (str "ヘルプ") $
  hLimit 60 $ vLimit 20 $
  vBox [ withAttr (attrName "header") $ str "キーボードショートカット:"
       , str " "
       , str "基本操作:"
       , str "  ↑/↓, j/k     : タスク選択"
       , str "  a            : 新しいタスク追加"
       , str "  d            : 選択したタスクを削除"
       , str "  c            : 選択したタスクを完了"
       , str "  p            : 優先度変更"
       , str "  q            : 終了"
       , str " "
       , str "優先度: H(高) M(中) L(低)"
       , str "ステータス: [ ]未完了 [~]進行中 [✓]完了"
       , str " "
       , hCenter $ str "[ESC]戻る"
       ]

-- Event handling
handleEvent :: BrickEvent ResourceName e -> EventM ResourceName AppState ()
handleEvent ev = case ev of
  VtyEvent vtev -> case vtev of
    V.EvKey V.KEsc [] -> do
      st <- get
      case appMode st of
        ViewMode -> halt
        _ -> modify $ \s -> s { appMode = ViewMode, appInputText = "" }
    
    V.EvKey (V.KChar 'q') [] -> do
      st <- get
      when (appMode st == ViewMode) halt
    
    V.EvKey (V.KChar '?') [] -> do
      st <- get
      when (appMode st == ViewMode) $ 
        modify $ \s -> s { appMode = HelpMode }
    
    V.EvKey (V.KChar 'a') [] -> do
      st <- get
      case appMode st of
        ViewMode -> 
          modify $ \s -> s { appMode = AddMode, appInputText = "" }
        AddMode ->
          -- In AddMode, 'a' should be added to input text
          modify $ \s -> s { appInputText = appInputText s ++ "a" }
        _ -> return ()
    
    V.EvKey V.KEnter [] -> do
      st <- get
      case appMode st of
        AddMode -> do
          unless (null $ appInputText st) $ do
            let (newCtrl, _) = addTask (appInputText st) (appController st)
            modify $ \s -> s { appController = newCtrl }
            updateTaskList
          modify $ \s -> s { appMode = ViewMode, appInputText = "" }
        _ -> return ()
    
    V.EvKey (V.KChar c) [] -> do
      st <- get
      case appMode st of
        AddMode -> 
          -- In AddMode, all characters should be added to input text
          modify $ \s -> s { appInputText = appInputText s ++ [c] }
        
        ViewMode -> case c of
          'd' -> deleteSelectedTask
          'c' -> completeSelectedTask
          'p' -> cyclePriority
          'k' -> moveListUp    -- Vim-like navigation
          'j' -> moveListDown  -- Vim-like navigation
          _ -> return ()
        
        _ -> return ()
    
    V.EvKey V.KBS [] -> do
      st <- get
      when (appMode st == AddMode) $ 
        modify $ \s -> s { appInputText = 
          if null (appInputText s) then "" else init (appInputText s) }
    
    V.EvKey V.KUp [] -> moveListUp
    V.EvKey V.KDown [] -> moveListDown
    
    _ -> return ()
  
  _ -> return ()

-- Helper functions
updateTaskList :: EventM ResourceName AppState ()
updateTaskList = do
  st <- get
  let taskList = getTasks (appController st)
      newList = listReplace (Vec.fromList taskList) Nothing (appTaskList st)
  modify $ \s -> s { appTaskList = newList }

moveListUp :: EventM ResourceName AppState ()
moveListUp = do
  st <- get
  when (appMode st == ViewMode) $ do
    let newList = listMoveUp (appTaskList st)
    modify $ \s -> s { appTaskList = newList }

moveListDown :: EventM ResourceName AppState ()
moveListDown = do
  st <- get
  when (appMode st == ViewMode) $ do
    let newList = listMoveDown (appTaskList st)
    modify $ \s -> s { appTaskList = newList }

deleteSelectedTask :: EventM ResourceName AppState ()
deleteSelectedTask = do
  st <- get
  case listSelectedElement (appTaskList st) of
    Just (_, task) -> do
      let currentTaskId = getTaskId task
          newCtrl = deleteTask currentTaskId (appController st)
      modify $ \s -> s { appController = newCtrl }
      updateTaskList
    Nothing -> return ()

completeSelectedTask :: EventM ResourceName AppState ()
completeSelectedTask = do
  st <- get
  case listSelectedElement (appTaskList st) of
    Just (_, task) -> do
      let currentTaskId = getTaskId task
          newCtrl = completeTask currentTaskId (appController st)
      modify $ \s -> s { appController = newCtrl }
      updateTaskList
    Nothing -> return ()

cyclePriority :: EventM ResourceName AppState ()
cyclePriority = do
  st <- get
  case listSelectedElement (appTaskList st) of
    Just (_, task) -> do
      let currentTaskId = getTaskId task
          currentPriority = getPriority task
          newPriority = case currentPriority of
            Low -> Medium
            Medium -> High
            High -> Low
          newCtrl = updateTaskPriority currentTaskId newPriority (appController st)
      modify $ \s -> s { appController = newCtrl }
      updateTaskList
    Nothing -> return ()

-- Main TUI function
runTUI :: IO ()
runTUI = do
  let buildVty = V.mkVty V.defaultConfig
  initialVty <- buildVty
  void $ customMain initialVty buildVty Nothing theApp initialState

theApp :: App AppState e ResourceName
theApp = App
  { appDraw = drawUI
  , appChooseCursor = neverShowCursor
  , appHandleEvent = handleEvent
  , appStartEvent = return ()
  , appAttrMap = const theMap
  }