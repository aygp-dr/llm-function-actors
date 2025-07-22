.PHONY: all run demo test clean help

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

# Clean generated files
clean:
	@echo "Cleaning generated files..."
	@find . -name "*.go" -o -name "*.png" -o -name "*~" | xargs rm -f

# Show help
help:
	@echo "LLM Function Calling Pattern - Makefile targets:"
	@echo ""
	@echo "  make run    - Run the main function calling simulator"
	@echo "  make demo   - Run the demo examples"
	@echo "  make test   - Run tests (TODO)"
	@echo "  make clean  - Clean generated files"
	@echo "  make help   - Show this help message"
	@echo ""
	@echo "Prerequisites:"
	@echo "  - GNU Guile 3.0+ installed"
	@echo "  - ice-9 and srfi modules available"