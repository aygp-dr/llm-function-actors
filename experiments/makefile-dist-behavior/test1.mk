# Test 1: Directory exists, no dist target defined
DIST_DIR := dist
DIST_NAME := myapp-1.0

# File target only
$(DIST_DIR)/$(DIST_NAME).tar.gz: | $(DIST_DIR)
	@echo "Creating $@"
	@touch $@

$(DIST_DIR):
	@mkdir -p $@