# Task Manager CLI

A type-safe task management command-line application built with Haskell, demonstrating functional programming principles and Test-Driven Development (TDD).

## Features

- **Type-Safe Design**: Leverages Haskell's type system to prevent runtime errors
- **MVC Architecture**: Clean separation of Model, View, and Controller layers
- **Test-Driven Development**: Comprehensive unit test coverage with HSpec
- **CLI Interface**: Interactive command-line interface for task management
- **Priority System**: Low, Medium, High priority levels with proper ordering
- **Status Tracking**: Todo, In-Progress, Done status management

## Architecture

```
src/
├── TaskManager/
│   ├── Model/          # Domain models (Task, Priority, Status)
│   ├── Controller/     # Business logic (TaskController)
│   ├── View/          # CLI interface and user interaction
│   └── Persistence/   # File storage layer
├── app/Main.hs        # Application entry point
└── test/              # Unit tests
```

## Installation

### Prerequisites

- [GHCup](https://www.haskell.org/ghcup/) (Haskell toolchain installer)
- GHC 9.6.7+
- Cabal 3.12+

### Setup

```bash
# Install Haskell toolchain
curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | sh
source ~/.ghcup/env

# Clone the repository
git clone https://github.com/gura105/haskell-example.git
cd haskell-example

# Build the project
cabal build

# Run tests
cabal test
```

## Usage

### Run the CLI Application

```bash
cabal run task-manager
```

### Available Commands

```
add <title>           - Add a new task
list                  - List all tasks
complete <id>         - Mark task as completed
delete <id>           - Delete a task
priority <id> <level> - Set task priority (low/medium/high)
help                  - Show help
quit                  - Exit the program
```

### Example Session

```
Task Manager CLI
Type 'help' for commands
> add "Learn Haskell"
Added task #1: Learn Haskell
> add "Write unit tests"
Added task #2: Write unit tests
> priority 2 high
Set priority of task #2 to High
> list
[ ] #1 (M) Learn Haskell
[ ] #2 (H) Write unit tests
> complete 1
Completed task #1
> list
[✓] #1 (M) Learn Haskell
[ ] #2 (H) Write unit tests
```

## Testing

The project follows Test-Driven Development with comprehensive unit tests:

```bash
# Run all tests
cabal test

# Build and test in one command
cabal build && cabal test
```

### Test Coverage

- **Priority**: Ordering and string parsing
- **Status**: State transitions and parsing
- **Task**: Creation, updates, and property getters
- **TaskController**: CRUD operations and business logic

## Type Safety Features

### Domain Models

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

### Type-Safe Operations

```haskell
-- Type-safe task creation
newTask :: String -> Task

-- Type-safe status updates
setStatus :: Status -> Task -> Task

-- Type-safe priority management
setPriority :: Priority -> Task -> Task
```

## Development

### Project Structure

This project demonstrates several Haskell best practices:

- **MVC Architecture**: Clear separation of concerns
- **Type Safety**: Compile-time guarantees for business logic
- **Pure Functions**: Immutable data structures and referential transparency
- **Qualified Imports**: Avoiding namespace conflicts
- **Comprehensive Testing**: TDD approach with property-based testing

### Building

```bash
# Clean build
cabal clean && cabal build

# Build with warnings
cabal build --ghc-options="-Wall"

# Watch mode (requires ghcid)
ghcid
```

## Dependencies

- **base**: Core Haskell libraries
- **time**: Date and time handling
- **aeson**: JSON parsing (for future persistence)
- **text**: Efficient text processing
- **hspec**: Testing framework
- **QuickCheck**: Property-based testing

## Contributing

1. Fork the repository
2. Create a feature branch
3. Write tests for new functionality
4. Implement the feature
5. Ensure all tests pass
6. Submit a pull request

## License

MIT License

## Future Enhancements

- [ ] JSON file persistence
- [ ] Task due dates and reminders
- [ ] Task categories and tags
- [ ] Export/import functionality
- [ ] Configuration file support
- [ ] Color-coded output