# タスク管理CLI

Haskellで構築された型安全なタスク管理コマンドラインアプリケーション。関数型プログラミングの原則とテスト駆動開発（TDD）を実践しています。

## 特徴

- **型安全設計**: Haskellの型システムを活用してランタイムエラーを防止
- **MVCアーキテクチャ**: Model、View、Controllerレイヤーの明確な分離
- **テスト駆動開発**: HSpecによる包括的な単体テストカバレッジ
- **リッチTUIインターフェース**: Brickライブラリによるインタラクティブな端末UI
- **従来CLIモード**: シンプルなコマンドライン操作も利用可能
- **優先度システム**: Low、Medium、Highの優先度レベルと適切な順序付け
- **状態追跡**: Todo、進行中、完了の状態管理
- **日本語対応**: 完全な日本語インターフェース

## 開発環境

### VSCode IDE サポート

このプロジェクトは VSCode での開発に最適化されています：

#### 必要な拡張機能
- `haskell.haskell` (Haskell Language Server)
- `justusadam.language-haskell` (Haskell Syntax)

#### IDE機能
- ✅ **コードジャンプ**: 関数定義へのジャンプ（F12）
- ✅ **参照検索**: 変数・関数の使用箇所検索（Shift+F12）
- ✅ **型情報表示**: ホバーで型情報表示
- ✅ **自動補完**: インテリジェントなコード補完
- ✅ **リアルタイム診断**: HLintとGHCエラー表示
- ✅ **コードアクション**: 自動インポート、型注釈追加
- ✅ **シンボル検索**: ワークスペース全体での検索（Ctrl+T）
- ✅ **リファクタリング**: 安全な名前変更

#### セットアップ手順
```bash
# 1. VSCodeで拡張機能をインストール
# 2. プロジェクトを開く
code .

# 3. Haskell Language Serverが自動で起動
# 4. 依存関係の解決（初回のみ時間がかかります）
```

#### ビルドタスク
VSCodeで `Ctrl+Shift+P` → `Tasks: Run Task` で以下が利用可能：
- `cabal build` - プロジェクトビルド
- `cabal test` - テスト実行
- `hlint check` - リンターチェック
- `make dev` - ビルド・テスト・リント一括実行
- `run TUI` - TUIモード実行
- `run CLI` - CLIモード実行

## アーキテクチャ

```
src/
├── TaskManager/
│   ├── Model/          # ドメインモデル (Task, Priority, Status)
│   ├── Controller/     # ビジネスロジック (TaskController)
│   ├── View/          # CLIとTUIインターフェース
│   │   ├── CLI.hs     # 従来のコマンドラインインターフェース
│   │   └── TUI.hs     # リッチな端末ユーザーインターフェース
│   └── Persistence/   # ファイルストレージ層
├── app/Main.hs        # アプリケーションエントリーポイント
└── test/              # 単体テスト
```

## インストール

### 前提条件

- [GHCup](https://www.haskell.org/ghcup/) (Haskellツールチェーンインストーラー)
- GHC 9.6.7+
- Cabal 3.12+
- Haskell Language Server (IDE機能用)

### セットアップ

```bash
# Haskellツールチェーンをインストール
curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | sh
source ~/.ghcup/env

# Haskell Language Server をインストール (IDE機能用)
ghcup install hls

# リポジトリをクローン
git clone https://github.com/gura105/haskell-example.git
cd haskell-example

# プロジェクトをビルド
cabal build

# テストを実行
cabal test
```

## 使用方法

### アプリケーションの実行

```bash
# リッチなTUIモードで起動（デフォルト）
cabal run task-manager

# 明示的にTUIモードを指定
cabal run task-manager -- --tui

# 従来のCLIモードで起動
cabal run task-manager -- --cli

# ヘルプを表示
cabal run task-manager -- --help
```

### TUIモード（推奨）

リッチな端末インターフェースでのタスク管理：

#### キーボードショートカット

```
基本操作:
  ↑/↓, j/k     : タスク選択
  a            : 新しいタスク追加
  d            : 選択したタスクを削除
  c            : 選択したタスクを完了
  p            : 優先度変更（L→M→H→L...）
  q            : 終了
  ?            : ヘルプ表示
  ESC          : モードを戻る

入力モード:
  Enter        : タスク追加確定
  Backspace    : 文字削除
  ESC          : キャンセル
```

#### 表示項目

- **ステータス**: `[ ]`未完了 `[~]`進行中 `[✓]`完了
- **優先度**: `(H)`高 `(M)`中 `(L)`低（色分け表示）
- **タスクID**: `#番号`
- **タイトル**: タスク名

### CLIモード

従来のコマンドライン操作：

#### 利用可能なコマンド

```
add <タイトル>          - 新しいタスクを追加
list                   - すべてのタスクを一覧表示
complete <id>          - タスクを完了としてマーク
delete <id>            - タスクを削除
priority <id> <レベル>  - タスクの優先度を設定 (low/medium/high)
help                   - ヘルプを表示
quit                   - プログラムを終了
```

#### 使用例

```
Task Manager CLI
Type 'help' for commands
> add "Haskellを学ぶ"
Added task #1: "Haskellを学ぶ"
> add "単体テストを書く"
Added task #2: "単体テストを書く"
> priority 2 high
Set priority of task #2 to High
> list
[ ] #1 (M) "Haskellを学ぶ"
[ ] #2 (H) "単体テストを書く"
> complete 1
Completed task #1
> list
[✓] #1 (M) "Haskellを学ぶ"
[ ] #2 (H) "単体テストを書く"
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
- **リンティング**: HLintによるコード品質管理

### ビルドとリンティング

```bash
# 基本的な開発ワークフロー
make dev              # ビルド、テスト、リント実行

# 個別の操作
make build           # プロジェクトをビルド
make test            # テストを実行  
make lint            # HLintでコード品質チェック (HTML レポート付き)
make lint-quick      # 迅速なリントチェック

# アプリケーション実行
make run-tui         # TUIモードで実行
make run-cli         # CLIモードで実行

# クリーンアップ
make clean           # ビルド成果物を削除

# ヘルプ
make help            # 利用可能なコマンドを表示
```

### リンティング

このプロジェクトはHLintを使用してコード品質を管理しています：

```bash
# リンティングの実行
hlint src app test

# 設定ファイル
.hlint.yaml          # HLint設定（プロジェクトルート）
```

HLintは以下をチェックします：
- コードの簡略化提案
- 不要なインポートや変数
- より良いHaskellイディオムの提案
- カスタムルールに基づく警告

### 従来のCabalコマンド

```bash
# クリーンビルド
cabal clean && cabal build

# 警告付きビルド
cabal build --ghc-options="-Wall"

# ウォッチモード（ghcidが必要）
ghcid
```

## 依存関係

### 主要ライブラリ
- **base**: 核となるHaskellライブラリ
- **time**: 日付と時刻の処理
- **aeson**: JSON解析（将来の永続化用）
- **text**: 効率的なテキスト処理

### TUI関連
- **brick**: 端末ユーザーインターフェース構築
- **vty**: 低レベル端末制御
- **microlens**: 軽量レンズライブラリ
- **vector**: 効率的な配列操作

### テスト
- **hspec**: テストフレームワーク
- **QuickCheck**: プロパティベーステスト

## 貢献

1. リポジトリをフォーク
2. 機能ブランチを作成
3. 新機能のテストを記述
4. 機能を実装
5. `make dev`でローカルチェック実行
6. すべてのテストとリンターが通ることを確認
7. プルリクエストを送信

## ライセンス

MIT License

## 今後の拡張予定

- [ ] JSONファイル永続化
- [ ] タスクの期限とリマインダー
- [ ] タスクカテゴリとタグ
- [ ] エクスポート/インポート機能
- [ ] 設定ファイルサポート
- [ ] TUIでのタスク編集機能
- [ ] タスクの並び替え（ドラッグ&ドロップ風）
- [ ] 検索・フィルター機能