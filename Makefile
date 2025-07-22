.PHONY: all run demo test lint clean tangle detangle deps help

# Use gmake if available
MAKE := gmake

# Default target
all: help

# Run the main simulator
run:
	@echo "Running function calling simulator..."
	@cd src && guile function-calling-simulator.scm

# Run the demo examples
demo:
	@echo "Running demo examples..."
	@cd examples && guile function-calling-demo.scm

# Run tests (placeholder for now)
test:
	@echo "Running tests..."
	@echo "TODO: Add test suite"

# Lint Scheme files
lint:
	@echo "Linting Scheme files..."
	@find . -name "*.scm" -exec guile -c "(use-modules (ice-9 format)) (load \"{}\")" \; 2>&1 | grep -E "(error|warning)" || echo "No syntax errors found"

# Clean generated files
clean:
	@echo "Cleaning generated files..."
	@find . -name "*.go" -o -name "*.png" -o -name "*~" | xargs rm -f

# Tangle README.org
tangle:
	@echo "Tangling README.org..."
	@emacs --batch --eval "(require 'org)" --eval "(org-babel-tangle-file \"README.org\")"

# Detangle README.org
detangle:
	@echo "Detangling README.org..."
	@emacs --batch --eval "(require 'org)" --eval "(org-babel-detangle \"README.org\")"

# Check dependencies
deps:
	@echo "Checking dependencies..."
	@command -v guile3 >/dev/null 2>&1 || { echo "ERROR: guile3 not found"; exit 1; }
	@echo "  ✓ Guile 3 found: $$(guile3 --version | head -1)"
	@guile3 -c "(use-modules (ice-9 match) (ice-9 format) (ice-9 threads) (srfi srfi-1) (srfi srfi-9))" 2>/dev/null || { echo "ERROR: Required Guile modules not available"; exit 1; }
	@echo "  ✓ Required Guile modules available"
	@command -v emacs >/dev/null 2>&1 && echo "  ✓ Emacs found: $$(emacs --version | head -1)" || echo "  ⚠ Emacs not found (optional for tangling)"
	@command -v mmdc >/dev/null 2>&1 && echo "  ✓ Mermaid CLI found" || echo "  ⚠ Mermaid CLI not found (optional for diagrams)"

# Show help
help:
	@echo "LLM Function Calling Pattern - Makefile targets:"
	@echo ""
	@echo "  gmake run      - Run the main function calling simulator"
	@echo "  gmake demo     - Run the demo examples"
	@echo "  gmake test     - Run tests (TODO)"
	@echo "  gmake lint     - Lint Scheme files for syntax errors"
	@echo "  gmake clean    - Clean generated files"
	@echo "  gmake tangle   - Tangle code from README.org"
	@echo "  gmake detangle - Update README.org from tangled files"
	@echo "  gmake deps     - Check dependencies"
	@echo "  gmake help     - Show this help message"
	@echo ""
	@echo "Prerequisites:"
	@echo "  - GNU Guile 3.0+ installed"
	@echo "  - ice-9 and srfi modules available"
	@echo "  - Emacs (optional, for tangling)"
	@echo "  - Mermaid CLI (optional, for diagrams)"