# Haskell Task Manager - Development Makefile

# Variables
PATH := $(HOME)/.ghcup/bin:$(PATH)
GHC_BIN = ghc
CABAL_BIN = cabal
HLINT_BIN = hlint

# Default target
.PHONY: all
all: build test lint

# Build targets
.PHONY: build
build:
	$(CABAL_BIN) build

.PHONY: clean
clean:
	$(CABAL_BIN) clean

# Test targets
.PHONY: test
test:
	$(CABAL_BIN) test

# Linting targets
.PHONY: lint
lint: hlint ghc-warnings

.PHONY: hlint
hlint:
	@echo "Running HLint..."
	$(HLINT_BIN) src app test --report=hlint-report.html

.PHONY: ghc-warnings
ghc-warnings:
	@echo "Running GHC with all warnings..."
	$(CABAL_BIN) build --ghc-options="-Wall -Wextra -Wcompat -Widentities -Wincomplete-record-updates -Wincomplete-uni-patterns -Wmissing-export-lists -Wmissing-home-modules -Wpartial-fields -Wredundant-constraints -Wname-shadowing"

.PHONY: ghc-strict
ghc-strict:
	@echo "Running GHC with strict warnings (treats warnings as errors)..."
	$(CABAL_BIN) build --ghc-options="-Wall -Werror -Wextra -Wcompat -Widentities -Wincomplete-record-updates -Wincomplete-uni-patterns -Wmissing-export-lists -Wmissing-home-modules -Wpartial-fields -Wredundant-constraints -Wname-shadowing"

# Development workflow
.PHONY: dev
dev: build test lint

.PHONY: dev-strict
dev-strict: build test ghc-strict hlint

# Run targets
.PHONY: run-tui
run-tui:
	$(CABAL_BIN) run task-manager

.PHONY: run-cli
run-cli:
	$(CABAL_BIN) run task-manager -- --cli

# Linter check only (fast)
.PHONY: lint-quick
lint-quick:
	@echo "Quick lint check..."
	$(HLINT_BIN) src app test

# Watch mode (requires entr)
.PHONY: watch
watch:
	find src app test -name "*.hs" | entr -c make dev

# Help
.PHONY: help
help:
	@echo "Available targets:"
	@echo "  build         - Build the project"
	@echo "  test          - Run tests"
	@echo "  lint          - Run all linters (HLint + GHC warnings)"
	@echo "  hlint         - Run HLint only"
	@echo "  ghc-warnings  - Run GHC with all warnings"
	@echo "  ghc-strict    - Run GHC treating warnings as errors"
	@echo "  dev           - Build + test + lint"
	@echo "  dev-strict    - Build + test + strict linting"
	@echo "  run-tui       - Run in TUI mode"
	@echo "  run-cli       - Run in CLI mode"
	@echo "  lint-quick    - Quick HLint check"
	@echo "  clean         - Clean build artifacts"
	@echo "  watch         - Watch files and run dev on changes"
	@echo "  help          - Show this help"