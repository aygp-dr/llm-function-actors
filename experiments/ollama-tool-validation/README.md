# Ollama Tool Calling Validation Experiment

## Overview
This experiment validates Ollama's tool calling capabilities using Llama 3.2 models with a custom Guile Scheme implementation. The goal is to demonstrate reliable function calling for local LLMs following a formal sequence diagram pattern.

## Objectives
1. Validate that Llama 3.2 (1B/3B) models support tool calling through Ollama's API
2. Implement a minimal file operations toolset in Guile Scheme
3. Verify the implementation follows the standard tool calling sequence diagram
4. Test remote execution via SSH tunnel (localhost:11434 forwarding)
5. Compare capabilities against production AI coding assistants

## Architecture
- **LLM Backend**: Ollama running Llama 3.2 models (preference for 3B over 1B)
- **Client**: Custom Guile Scheme modules (`ollama-client` and `file-tools`)
- **Tools**: Minimal set of 4 file operations (read, write, list, search)
- **Infrastructure**: SSH RemoteForward tunnel allowing remote Guile code to use local Ollama

## Test Scenarios
1. **Basic Tool Recognition**: Verify LLM correctly identifies when to use tools
2. **File Operations**: Test each tool (read_file, write_file, list_files, search_code)
3. **Complex Workflows**: Multi-tool sequences for real tasks
4. **Error Handling**: Validate graceful failures and recovery
5. **Performance**: Measure latency through SSH tunnel

## Validation Approach
- Comprehensive logging to trace each step of the sequence diagram
- Side-by-side comparison of expected vs actual flow
- Success rate metrics for each tool type
- Comparison with industry tool usage patterns

## Expected Outcomes
- Proof that Llama 3.2 can reliably perform tool calling for file operations
- Reusable Guile modules for Ollama integration
- Performance baseline for local LLM tool calling
- Gap analysis vs production coding assistants (Claude Code, Copilot, etc.)

## Repository Structure
```
ollama-tool-validation/
├── README.md           # This document
├── src/
│   ├── ollama-client.scm
│   ├── file-tools.scm
│   └── integration.scm
├── tests/
│   ├── test-data/
│   └── validation-suite.scm
├── docs/
│   ├── sequence-diagram.md
│   └── tool-analytics.md
└── experiments.org     # Detailed implementation and results
```

## Key Insights to Validate
1. Llama 3.2 successfully upgraded from 3.1 with tool support intact
2. 4-tool minimal set covers ~25% of typical coding assistant operations
3. SSH tunnel approach enables remote development with local LLM
4. Guile Scheme provides clean abstraction for tool implementation

## Next Steps
- [ ] Set up test environment with Ollama + Llama 3.2
- [ ] Implement core modules
- [ ] Create test suite with logging
- [ ] Document results and performance metrics
- [ ] Identify gaps and potential expansions

## Related Resources
- [GitHub Issue #3](https://github.com/aygp-dr/llm-function-actors/issues/3)
- [LLM Actor Mock Experiment](../llm-actor-mock/)
- [Main Project README](../../README.org)