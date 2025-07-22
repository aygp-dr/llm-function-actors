# Test Makefile to understand dist behavior

# This mimics our situation
DIST_DIR := testdist
DIST_NAME := myapp-1.0

# Create directory
$(DIST_DIR):
	@echo "Creating directory $@"
	@mkdir -p $@

# File target
$(DIST_DIR)/$(DIST_NAME).tar.gz: | $(DIST_DIR)
	@echo "Creating $@"
	@touch $@

# Test 1: No dist target at all
# Test 2: Uncomment below to add .PHONY dist
# .PHONY: dist
# dist: $(DIST_DIR)/$(DIST_NAME).tar.gz