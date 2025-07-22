# LLM Function Calling Pattern - Two Actor System

A Guile Scheme implementation demonstrating the interaction between an Application (Actor 1) and an LLM Provider (Actor 2) for function calling capabilities.

## Overview

This project implements a two-actor pattern for LLM function calling, providing:
- Clear separation between application logic and LLM reasoning
- Asynchronous message passing between actors
- Extensible function registry
- Thread-safe communication channels

## Architecture

The system consists of two main actors:

1. **Application Actor**: Manages function definitions, executes requested functions, and handles results
2. **LLM Actor**: Processes prompts, decides whether to call functions or respond directly, and generates final answers

Communication happens through message queues with defined message types for each phase of interaction.

## Requirements

- GNU Guile 3.0 or later
- SRFI modules (srfi-1, srfi-9)
- ice-9 modules (match, format)

## Installation

```bash
git clone <repository>
cd llm-function-actors
```

## Usage

### Run the simulator
```bash
make run
```

### Run demo examples
```bash
make demo
# or directly:
./examples/function-calling-demo.scm
```

### Available Functions

The simulator comes with two built-in functions:
- `calculate`: Adds two numbers
- `get-time`: Returns current timestamp

Additional functions can be registered:
```scheme
(register-function! 'my-function
                    (lambda (arg1 arg2)
                      ;; function implementation
                      result))
```

## Project Structure

```
.
├── SETUP.org           # Original design document
├── README.md           # This file
├── Makefile            # Build automation
├── src/
│   └── function-calling-simulator.scm  # Main simulator
├── examples/
│   └── function-calling-demo.scm       # Usage examples
└── docs/
    ├── function-flow.org               # Sequence diagrams
    └── pattern-analysis.org            # Architecture analysis
```

## Message Flow

1. Application sends initial prompt with function definitions
2. LLM analyzes prompt and decides action
3. If function needed: LLM requests function execution
4. Application executes function and returns result
5. LLM incorporates result and generates final answer

## Extending the System

To add new capabilities:

1. Register new functions using `register-function!`
2. Extend message types for new interaction patterns
3. Add error handling for production use
4. Implement timeout mechanisms for long-running functions

## License

This project is part of the aygp-dr repository.