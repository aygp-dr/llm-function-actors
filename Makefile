.PHONY: all run demo demo-file-tools test lint lint-scheme lint-org lint-shell clean build dist tangle detangle deps help

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
	@echo "Running function calling demo..."
	@guile examples/function-calling-demo.scm

# Run file tools demo
demo-file-tools:
	@echo "Running file tools demo..."
	@guile examples/file-tools-demo.scm

# Run tests (placeholder for now)
test:
	@echo "Running tests..."
	@echo "TODO: Add test suite"

# Lint all file types
lint: lint-scheme lint-org lint-shell
	@echo ""
	@echo "=== All linting complete ==="

# Lint Scheme files
lint-scheme:
	@echo "=== Linting Scheme files ==="
	@find . -name "*.scm" -type f -exec guile -c "(primitive-load \"{}\")" \;

# Lint Org files
lint-org:
	@echo "=== Linting Org files ==="
	@find . -name "*.org" -type f -exec emacs --batch --eval "(progn (require 'org) (find-file \"{}\") (org-lint) (kill-emacs))" \;

# Lint Shell scripts and Makefile
lint-shell:
	@echo "=== Linting Shell scripts ==="
	@find . -name "*.sh" -o -name "*.bash" | xargs -r shellcheck
	@echo "=== Checking Makefile ==="
	@gmake -n -f Makefile

# Clean generated files
clean:
	@echo "Cleaning generated files..."
	@find . -name "*.go" -o -name "*.png" -o -name "*~" | xargs rm -f
	@rm -rf build/ dist/

# Build bytecode
build:
	@echo "Building Guile bytecode..."
	@mkdir -p build/bin build/lib
	@echo "Compiling Scheme files..."
	@guild compile -o build/lib/function-calling-simulator.go src/function-calling-simulator.scm
	@guild compile -o build/lib/file-tools-simulator.go src/file-tools-simulator.scm
	@echo "Creating launcher scripts..."
	@echo '#!/bin/sh' > build/bin/llm-function-actors
	@echo 'exec guile -L "$$(dirname "$$0")/../lib" -c "(primitive-load \"$$(dirname \"$$0\")/../lib/function-calling-simulator.go\")"' >> build/bin/llm-function-actors
	@chmod +x build/bin/llm-function-actors
	@echo "Build complete in build/"

# Create distribution package
dist: build
	@echo "Creating distribution package..."
	@mkdir -p dist
	@tar czf dist/llm-function-actors-$$(date +%Y%m%d).tar.gz -C build .
	@echo "Distribution package created in dist/"

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
	@guile3 --version | head -1
	@guile3 -c "(use-modules (ice-9 match) (ice-9 format) (ice-9 threads) (srfi srfi-1) (srfi srfi-9))"
	@echo "Required Guile modules available"

# Show help
help:
	@echo "LLM Function Calling Pattern - Makefile targets:"
	@echo ""
	@echo "  gmake run           - Run the main function calling simulator"
	@echo "  gmake demo          - Run the function calling demo"
	@echo "  gmake demo-file-tools - Run the file tools demo"
	@echo "  gmake test          - Run tests (TODO)"
	@echo "  gmake lint          - Lint all files (Scheme, Org, Shell)"
	@echo "  gmake lint-scheme   - Lint only Scheme files"
	@echo "  gmake lint-org      - Lint only Org files"
	@echo "  gmake lint-shell    - Lint only Shell scripts and Makefile"
	@echo "  gmake build         - Build bytecode files"
	@echo "  gmake dist          - Create distribution package"
	@echo "  gmake clean         - Clean generated files"
	@echo "  gmake tangle        - Tangle code from README.org"
	@echo "  gmake detangle      - Update README.org from tangled files"
	@echo "  gmake deps          - Check dependencies"
	@echo "  gmake help          - Show this help message"
	@echo ""
	@echo "Prerequisites:"
	@echo "  - GNU Guile 3.0+ installed"
	@echo "  - ice-9 and srfi modules available"
	@echo "  - Emacs (optional, for tangling and Org linting)"
	@echo "  - shellcheck (optional, for shell script linting)"
	@echo "  - Mermaid CLI (optional, for diagrams)"