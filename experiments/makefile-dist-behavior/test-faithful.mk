# Faithful replication of our Makefile's dist functionality
DIST_DIR := dist
DIST_NAME := myapp-1.0
BUILD_DIR := build

# Mock compiled files
COMPILED_FILES := $(BUILD_DIR)/lib/file1.go $(BUILD_DIR)/lib/file2.go
SCRIPT_TARGETS := $(BUILD_DIR)/bin/script1

# Directory targets
$(DIST_DIR):
	@install -d $@

$(BUILD_DIR)/lib $(BUILD_DIR)/bin:
	@mkdir -p $@

# Mock build targets
$(BUILD_DIR)/lib/%.go: | $(BUILD_DIR)/lib
	@echo "Mock compiling $@"
	@touch $@

$(BUILD_DIR)/bin/%: | $(BUILD_DIR)/bin
	@echo "Mock copying $@"
	@touch $@

# This is EXACTLY what we have - no dist: target, just the file target
$(DIST_DIR)/$(DIST_NAME).tar.gz: $(COMPILED_FILES) $(SCRIPT_TARGETS) | $(DIST_DIR)
	@echo "Creating distribution package..."
	@tar czf $@ -C $(BUILD_DIR) .
	@echo "Distribution package created: $@"