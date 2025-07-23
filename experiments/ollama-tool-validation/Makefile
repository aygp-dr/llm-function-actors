# Ollama Tool Validation Makefile

GUILE := guile3
SOURCES := src/ollama-client.scm src/file-tools.scm src/integration.scm
TEST_SOURCES := tests/validation-suite.scm

.PHONY: help
help: ## Show this help
	@awk 'BEGIN {FS = ":.*##"; printf "\nUsage:\n  make <target>\n\nTargets:\n"} /^[a-zA-Z_-]+:.*?##/ { printf "  %-20s %s\n", $$1, $$2 }' $(MAKEFILE_LIST)

.PHONY: check-ollama
check-ollama: ## Check if Ollama is running
	@echo "Checking Ollama status..."
	@curl -s http://localhost:11434/api/tags >/dev/null && echo "✓ Ollama is running" || echo "✗ Ollama is not running"
	@echo ""
	@echo "Available models:"
	@curl -s http://localhost:11434/api/tags | jq -r '.models[]?.name' 2>/dev/null || echo "Could not list models"

.PHONY: tangle
tangle: ## Extract source from experiments.org
	@echo "Tangling source files..."
	@emacs --batch --eval "(progn (require 'org) (org-babel-tangle-file \"experiments.org\"))"

.PHONY: test
test: ## Run validation tests
	@echo "Running validation suite..."
	@$(GUILE) tests/validation-suite.scm

.PHONY: run
run: ## Run the main integration demo
	@echo "Running Ollama tool calling demo..."
	@$(GUILE) src/integration.scm

.PHONY: run-basic
run-basic: ## Run basic client test
	@echo "Testing basic Ollama connection..."
	@$(GUILE) -c '(add-to-load-path "src") (use-modules (ollama-client)) (define client (make-ollama-client)) (display "Client created successfully\n")'

.PHONY: clean
clean: ## Clean generated files
	@echo "Cleaning..."
	@find . -name "*.go" -o -name "*~" -o -name "test-output.txt" | xargs rm -f

.PHONY: setup
setup: ## Set up test environment
	@echo "Setting up test environment..."
	@mkdir -p tests/test-data
	@echo "Test environment ready"

# Special targets
.DEFAULT_GOAL := help