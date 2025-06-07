# VSCode Haskell 開発環境セットアップガイド

## 必要な手順

### 1. Haskell Language Server のインストール

```bash
# Homebrewでインストール（推奨）
brew install haskell-language-server

# または ghcup でインストール（時間がかかる場合があります）
ghcup install hls
```

### 2. VSCode 拡張機能のインストール

VSCodeで以下の拡張機能をインストール：

1. **Haskell** (`haskell.haskell`) - メイン拡張機能
2. **Haskell Syntax Highlighting** (`justusadam.language-haskell`) - シンタックスハイライト

### 3. プロジェクトを開く

```bash
# プロジェクトディレクトリでVSCodeを開く
code .
```

### 4. 初回セットアップ

1. VSCodeを開くと、右下に「拡張機能をインストールしますか？」と表示される
2. 「Install」をクリック
3. Haskell Language Server が自動的に起動
4. 初回は依存関係の解析に数分かかります

## トラブルシューティング

### エラー: "Could not find a HLS binary"

**✅ 解決済み: ghcup版HLSを使用**
```bash
# GHC 9.6.7と互換性のあるHLSをインストール
ghcup install hls --set
```

VSCode設定は自動で以下に設定されています：
```json
{
  "haskell.serverExecutablePath": "/Users/gura105/.ghcup/bin/haskell-language-server-wrapper"
}
```

### HLS が起動しない場合

1. **VSCodeを再起動**
2. **コマンドパレット** (`Cmd+Shift+P`) → `Haskell: Restart language server`
3. **ターミナルでテスト**:
   ```bash
   haskell-language-server-wrapper --version
   ```

### パフォーマンスが遅い場合

`.vscode/settings.json` で以下を調整：
```json
{
  "haskell.maxNumberOfProblems": 50,
  "haskell.diagnosticsOnChange": false
}
```

## 利用可能な機能

### ✅ 基本機能
- **F12**: 定義へジャンプ
- **Shift+F12**: 参照検索
- **Ctrl+T**: シンボル検索
- **Hover**: 型情報表示
- **Ctrl+Space**: 自動補完

### ✅ コードアクション
- 自動インポート追加
- 型注釈の追加
- HLint 修正提案の適用

### ✅ ビルドタスク
`Ctrl+Shift+P` → `Tasks: Run Task`:
- `cabal build`
- `cabal test`
- `hlint check`
- `make dev`

## 推奨ワークフロー

1. **プロジェクトを開く**: `code .`
2. **HLSの起動を待つ** (初回は数分)
3. **コードを編集** → リアルタイムエラー表示
4. **F12でジャンプ** → 関数定義を確認
5. **Ctrl+Space** → 自動補完を活用
6. **Ctrl+Shift+P** → ビルドタスク実行

## 設定ファイル

プロジェクトには以下の設定ファイルが含まれています：

- `.vscode/settings.json` - VSCode設定
- `.vscode/tasks.json` - ビルドタスク
- `.vscode/extensions.json` - 推奨拡張機能
- `hie.yaml` - HLS プロジェクト構造定義

これらの設定により、最適化されたHaskell開発環境が提供されます。