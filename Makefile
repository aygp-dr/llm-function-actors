.PHONY: all run demo test lint lint-scheme lint-org lint-shell clean tangle detangle deps help

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

# Lint all file types
lint: lint-scheme lint-org lint-shell
	@echo ""
	@echo "=== All linting complete ==="

# Lint Scheme files
lint-scheme:
	@echo "=== Linting Scheme files ==="
	@for file in $$(find . -name "*.scm" -type f); do \
		echo "  Checking $$file..."; \
		guile -c "(primitive-load \"$$file\")" 2>&1 | grep -E "(error|warning)" || true; \
	done
	@echo "✓ Scheme linting complete"

# Lint Org files
lint-org:
	@echo "=== Linting Org files ==="
	@if command -v emacs >/dev/null 2>&1; then \
		for file in $$(find . -name "*.org" -type f); do \
			echo "  Checking $$file..."; \
			emacs --batch --eval "(progn (require 'org) (find-file \"$$file\") (org-lint) (kill-emacs))" 2>&1 | grep -v "Loading" | grep -E "(warning|error)" || true; \
		done; \
		echo "✓ Org linting complete"; \
	else \
		echo "⚠ Emacs not available for Org linting"; \
	fi

# Lint Shell scripts and Makefile
lint-shell:
	@echo "=== Linting Shell scripts ==="
	@if command -v shellcheck >/dev/null 2>&1; then \
		find . -name "*.sh" -o -name "*.bash" | xargs -r shellcheck || true; \
		echo "✓ Shell linting complete"; \
	else \
		echo "⚠ shellcheck not installed"; \
	fi
	@echo ""
	@echo "=== Checking Makefile ==="
	@gmake -n -f Makefile > /dev/null 2>&1 && echo "✓ Makefile syntax OK" || echo "✗ Makefile has syntax errors"

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
	@echo "  gmake run         - Run the main function calling simulator"
	@echo "  gmake demo        - Run the demo examples"
	@echo "  gmake test        - Run tests (TODO)"
	@echo "  gmake lint        - Lint all files (Scheme, Org, Shell)"
	@echo "  gmake lint-scheme - Lint only Scheme files"
	@echo "  gmake lint-org    - Lint only Org files"
	@echo "  gmake lint-shell  - Lint only Shell scripts and Makefile"
	@echo "  gmake clean       - Clean generated files"
	@echo "  gmake tangle      - Tangle code from README.org"
	@echo "  gmake detangle    - Update README.org from tangled files"
	@echo "  gmake deps        - Check dependencies"
	@echo "  gmake help        - Show this help message"
	@echo ""
	@echo "Prerequisites:"
	@echo "  - GNU Guile 3.0+ installed"
	@echo "  - ice-9 and srfi modules available"
	@echo "  - Emacs (optional, for tangling and Org linting)"
	@echo "  - shellcheck (optional, for shell script linting)"
	@echo "  - Mermaid CLI (optional, for diagrams)"