# LLM Function Calling Evaluation Framework

This experiment provides a structured framework for evaluating LLM function calling behavior across different tool sets and prompts.

## Structure

```
function-calling-evals/
├── eval-config.json          # Evaluation configuration and scoring weights
├── eval-runner.scm          # Guile Scheme evaluation runner
├── README.md               # This file
└── test-cases/            # Test suite JSON files
    ├── file-system-tools.json
    ├── calculator-tools.json
    └── mixed-tools.json
```

## Test Case Format

Each test case JSON file contains:

1. **Tool Definitions**: Available functions with parameters
2. **Test Cases**: Each with:
   - `id`: Unique identifier
   - `prompt`: User input
   - `expected`: Expected behavior including:
     - `should_call_function`: Boolean
     - `function_calls`: Array of expected calls (if applicable)
     - `acceptable_final_answers`: Array of valid responses

## Example Test Case

```json
{
  "id": "fs-001",
  "prompt": "What are the current targets for gmake?",
  "expected": {
    "should_call_function": true,
    "function_calls": [
      {
        "function": "list_files",
        "parameters": {},
        "purpose": "Find Makefile in current directory"
      },
      {
        "function": "read_file",
        "parameters": {
          "path": "Makefile"
        },
        "purpose": "Read Makefile contents to extract targets"
      }
    ],
    "acceptable_final_answers": [
      "The gmake targets are: [list of targets from Makefile]",
      "The available make targets are: [extracted targets]"
    ]
  }
}
```

## Running Evaluations

```bash
# Make the runner executable
chmod +x eval-runner.scm

# Run a specific test suite
./eval-runner.scm test-cases/file-system-tools.json

# Or use Guile directly
guile eval-runner.scm test-cases/calculator-tools.json
```

## Test Suites

### 1. File System Tools
Tests file operations like listing, reading, and writing files. Includes cases that should and shouldn't trigger function calls.

### 2. Calculator Tools
Tests mathematical calculations and statistics. Distinguishes between calculation requests and conceptual questions.

### 3. Mixed Tools
Tests complex scenarios requiring multiple tools and chaining operations.

## Evaluation Criteria

From `eval-config.json`:
- **Function Selection** (30%): Correctly identifies when to use functions
- **Parameter Extraction** (25%): Accurately extracts parameters from prompt
- **Chain Logic** (20%): Logical sequence of function calls
- **No False Positives** (15%): Avoids unnecessary function calls
- **Error Handling** (10%): Handles missing tools or parameters gracefully

## Key Patterns

### Should Call Functions:
- Direct requests: "Calculate 1 + 2"
- Information retrieval: "What's in config.json?"
- Multi-step tasks: "Count functions and calculate average"

### Should NOT Call Functions:
- Conceptual questions: "What is a Makefile?"
- General advice: "How do I write good documentation?"
- Explanations: "Explain hermeneutics"

## Extending the Framework

1. Add new test suites in `test-cases/`
2. Follow the existing JSON structure
3. Include both positive and negative test cases
4. Document the purpose of each function call
5. Provide multiple acceptable final answers

## Integration with LLM Simulator

This evaluation framework can be integrated with the main LLM function calling simulator to automatically test different scenarios and measure performance.