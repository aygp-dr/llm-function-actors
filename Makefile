# LLM Function Actors - GNU Makefile
# Version and naming
VERSION := 0.1.0
DIST_NAME := llm-function-actors-$(VERSION)

# Tools
MAKE := gmake
GUILE := guile3
GUILD := guild

# Directories
BUILD_DIR := build
DIST_DIR := dist
SRC_DIR := src
BIN_DIR := bin
LIB_DIR := $(BUILD_DIR)/lib
BUILD_BIN_DIR := $(BUILD_DIR)/bin

# Files
SCHEME_SOURCES := $(wildcard $(SRC_DIR)/*.scm)
COMPILED_FILES := $(patsubst $(SRC_DIR)/%.scm,$(LIB_DIR)/%.go,$(SCHEME_SOURCES))
SCRIPT_SOURCES := $(wildcard $(BIN_DIR)/*)
SCRIPT_TARGETS := $(patsubst $(BIN_DIR)/%,$(BUILD_BIN_DIR)/%,$(SCRIPT_SOURCES))

# Default target
.DEFAULT_GOAL := help

##@ Development

.PHONY: run
run: ## Run the main function calling simulator
	@echo "Running function calling simulator..."
	@$(GUILE) $(SRC_DIR)/function-calling-simulator.scm

.PHONY: demo
demo: ## Run the function calling demo
	@echo "Running function calling demo..."
	@$(GUILE) examples/function-calling-demo.scm

.PHONY: demo-file-tools
demo-file-tools: ## Run the file tools demo
	@echo "Running file tools demo..."
	@$(GUILE) examples/file-tools-demo.scm

##@ Testing & Quality

.PHONY: test
test: ## Run tests (TODO)
	@echo "Running tests..."
	@echo "TODO: Add test suite"

.PHONY: lint
lint: lint-scheme lint-org lint-shell ## Lint all files
	@echo ""
	@echo "=== All linting complete ==="

.PHONY: lint-scheme
lint-scheme: ## Lint Scheme files
	@echo "=== Linting Scheme files ==="
	@find . -name "*.scm" -type f -exec $(GUILE) -c "(primitive-load \"{}\")" \;

.PHONY: lint-org
lint-org: ## Lint Org files
	@echo "=== Linting Org files ==="
	@find . -name "*.org" -type f -exec emacs --batch --eval "(progn (require 'org) (find-file \"{}\") (org-lint) (kill-emacs))" \;

.PHONY: lint-shell
lint-shell: ## Lint shell scripts and Makefile
	@echo "=== Linting Shell scripts ==="
	@find . -name "*.sh" -o -name "*.bash" | xargs -r shellcheck
	@echo "=== Checking Makefile ==="
	@$(MAKE) -n -f Makefile

##@ Build & Distribution

# Directory targets (not .PHONY!)
$(LIB_DIR) $(BUILD_BIN_DIR):
	@mkdir -p $@

$(DIST_DIR):
	@install -d $@

# Pattern rule for compiling Scheme to bytecode
$(LIB_DIR)/%.go: $(SRC_DIR)/%.scm | $(LIB_DIR)
	@echo "Compiling $<..."
	@$(GUILD) compile -o $@ $<

# Pattern rule for copying scripts
$(BUILD_BIN_DIR)/%: $(BIN_DIR)/% | $(BUILD_BIN_DIR)
	@cp $< $@

.PHONY: build
build: $(COMPILED_FILES) $(SCRIPT_TARGETS) ## Build bytecode and copy scripts
	@echo "Build complete in $(BUILD_DIR)/"

# Distribution target
$(DIST_DIR)/$(DIST_NAME).tar.gz: $(COMPILED_FILES) $(SCRIPT_TARGETS) | $(DIST_DIR) ## Create distribution package
	@echo "Creating distribution package..."
	@tar czf $@ -C $(BUILD_DIR) .
	@echo "Distribution package created: $@"

.PHONY: clean
clean: ## Clean generated files
	@echo "Cleaning generated files..."
	@rm -rf $(BUILD_DIR) $(DIST_DIR)
	@find . -name "*.go" -o -name "*.png" -o -name "*~" | xargs rm -f

##@ Documentation

.PHONY: tangle
tangle: ## Tangle code from README.org
	@echo "Tangling README.org..."
	@emacs --batch --eval "(require 'org)" --eval "(org-babel-tangle-file \"README.org\")"

.PHONY: detangle  
detangle: ## Update README.org from tangled files
	@echo "Detangling README.org..."
	@emacs --batch --eval "(require 'org)" --eval "(org-babel-detangle \"README.org\")"

.PHONY: diagrams
diagrams: ## Generate diagram images
	@echo "Generating diagrams..."
	@echo "Running org-babel tangle to extract and generate images..."
	@emacs --batch --eval "(progn \
		(require 'org) \
		(require 'ob-ditaa) \
		(require 'ob-dot) \
		(setq org-confirm-babel-evaluate nil) \
		(setq org-ditaa-jar-path \"/usr/local/share/java/ditaa/ditaa.jar\") \
		(find-file \"README.org\") \
		(org-babel-execute-buffer) \
		(save-buffer))" 2>/dev/null || echo "Note: Some diagrams may require additional setup"
	@scripts/generate-diagrams.sh 2>/dev/null || true
	@echo "Diagram generation complete. Check for .png files."

##@ Utilities

.PHONY: deps
deps: ## Check dependencies
	@echo "Checking dependencies..."
	@$(GUILE) --version | head -1
	@$(GUILE) -c "(use-modules (ice-9 match) (ice-9 format) (ice-9 threads) (srfi srfi-1) (srfi srfi-9))"
	@echo "Required Guile modules available"
	@echo ""
	@echo "Checking optional diagram tools..."
	@command -v dot >/dev/null 2>&1 && echo "✓ Graphviz (dot) found" || echo "⚠ Graphviz not found"
	@command -v ditaa >/dev/null 2>&1 && echo "✓ Ditaa found" || echo "⚠ Ditaa not found" 
	@command -v plantuml >/dev/null 2>&1 && echo "✓ PlantUML found" || echo "⚠ PlantUML not found"
	@command -v mmdc >/dev/null 2>&1 && echo "✓ Mermaid CLI found" || echo "⚠ Mermaid CLI not found"

.PHONY: help
help: ## Show this help
	@awk 'BEGIN {FS = ":.*##"; printf "\nUsage:\n  make \033[36m<target>\033[0m\n"} /^[a-zA-Z_-]+:.*?##/ { printf "  \033[36m%-20s\033[0m %s\n", $$1, $$2 } /^##@/ { printf "\n\033[1m%s\033[0m\n", substr($$0, 5) } ' $(MAKEFILE_LIST)

# Special targets
.SUFFIXES:              # Delete default suffixes
.DELETE_ON_ERROR:       # Delete target file on error
.SECONDARY:             # Don't delete intermediate files
