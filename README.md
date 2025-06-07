# タスク管理CLI

Haskellで構築された型安全なタスク管理コマンドラインアプリケーション。関数型プログラミングの原則とテスト駆動開発（TDD）を実践しています。

## 特徴

- **型安全設計**: Haskellの型システムを活用してランタイムエラーを防止
- **MVCアーキテクチャ**: Model、View、Controllerレイヤーの明確な分離
- **テスト駆動開発**: HSpecによる包括的な単体テストカバレッジ
- **CLIインターフェース**: タスク管理のためのインタラクティブなコマンドライン
- **優先度システム**: Low、Medium、Highの優先度レベルと適切な順序付け
- **状態追跡**: Todo、進行中、完了の状態管理

## アーキテクチャ

```
src/
├── TaskManager/
│   ├── Model/          # ドメインモデル (Task, Priority, Status)
│   ├── Controller/     # ビジネスロジック (TaskController)
│   ├── View/          # CLIインターフェースとユーザー操作
│   └── Persistence/   # ファイルストレージ層
├── app/Main.hs        # アプリケーションエントリーポイント
└── test/              # 単体テスト
```

## インストール

### 前提条件

- [GHCup](https://www.haskell.org/ghcup/) (Haskellツールチェーンインストーラー)
- GHC 9.6.7+
- Cabal 3.12+

### セットアップ

```bash
# Haskellツールチェーンをインストール
curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | sh
source ~/.ghcup/env

# リポジトリをクローン
git clone https://github.com/gura105/haskell-example.git
cd haskell-example

# プロジェクトをビルド
cabal build

# テストを実行
cabal test
```

## 使用方法

### CLIアプリケーションの実行

```bash
cabal run task-manager
```

### 利用可能なコマンド

```
add <タイトル>          - 新しいタスクを追加
list                   - すべてのタスクを一覧表示
complete <id>          - タスクを完了としてマーク
delete <id>            - タスクを削除
priority <id> <レベル>  - タスクの優先度を設定 (low/medium/high)
help                   - ヘルプを表示
quit                   - プログラムを終了
```

### 使用例

```
Task Manager CLI
Type 'help' for commands
> add "Haskellを学ぶ"
Added task #1: Haskellを学ぶ
> add "単体テストを書く"
Added task #2: 単体テストを書く
> priority 2 high
Set priority of task #2 to High
> list
[ ] #1 (M) Haskellを学ぶ
[ ] #2 (H) 単体テストを書く
> complete 1
Completed task #1
> list
[✓] #1 (M) Haskellを学ぶ
[ ] #2 (H) 単体テストを書く
```

## テスト

このプロジェクトは包括的な単体テストを伴うテスト駆動開発に従っています：

```bash
# すべてのテストを実行
cabal test

# ビルドとテストを一括実行
cabal build && cabal test
```

### テストカバレッジ

- **Priority**: 順序付けと文字列解析
- **Status**: 状態遷移と解析
- **Task**: 作成、更新、プロパティゲッター
- **TaskController**: CRUD操作とビジネスロジック

## 型安全機能

### ドメインモデル

```haskell
data Priority = Low | Medium | High
  deriving (Show, Read, Eq, Ord)

data Status = Todo | InProgress | Done
  deriving (Show, Read, Eq, Ord)

data Task = Task
  { taskId     :: TaskId
  , title      :: String
  , status     :: Status
  , priority   :: Priority
  , createdAt  :: UTCTime
  } deriving (Show, Eq, Read)
```

### 型安全な操作

```haskell
-- 型安全なタスク作成
newTask :: String -> Task

-- 型安全な状態更新
setStatus :: Status -> Task -> Task

-- 型安全な優先度管理
setPriority :: Priority -> Task -> Task
```

## 開発

### プロジェクト構造

このプロジェクトはいくつかのHaskellベストプラクティスを実証しています：

- **MVCアーキテクチャ**: 関心の明確な分離
- **型安全性**: ビジネスロジックのコンパイル時保証
- **純粋関数**: 不変データ構造と参照透明性
- **修飾インポート**: 名前空間の競合回避
- **包括的テスト**: プロパティベーステストを用いたTDDアプローチ

### ビルド

```bash
# クリーンビルド
cabal clean && cabal build

# 警告付きビルド
cabal build --ghc-options="-Wall"

# ウォッチモード（ghcidが必要）
ghcid
```

## 依存関係

- **base**: 核となるHaskellライブラリ
- **time**: 日付と時刻の処理
- **aeson**: JSON解析（将来の永続化用）
- **text**: 効率的なテキスト処理
- **hspec**: テストフレームワーク
- **QuickCheck**: プロパティベーステスト

## 貢献

1. リポジトリをフォーク
2. 機能ブランチを作成
3. 新機能のテストを記述
4. 機能を実装
5. すべてのテストが通ることを確認
6. プルリクエストを送信

## ライセンス

MIT License

## 今後の拡張予定

- [ ] JSONファイル永続化
- [ ] タスクの期限と リマインダー
- [ ] タスクカテゴリとタグ
- [ ] エクスポート/インポート機能
- [ ] 設定ファイルサポート
- [ ] カラーコード化された出力