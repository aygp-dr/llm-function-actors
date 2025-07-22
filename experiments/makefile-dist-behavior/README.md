# Experiment: Makefile dist Target Behavior

## Hypothesis

When running `remake -n dist` on our Makefile that has:
- NO `.PHONY: dist` declaration  
- A file target `$(DIST_DIR)/$(DIST_NAME).tar.gz`
- An existing `dist/` directory

I hypothesize that Make will:
1. Look for a target named `dist`
2. Find the `dist` directory exists
3. Consider it "up to date" since it's a directory that exists
4. Do nothing and report "dist is up to date"

## Experiment Design

We'll create test cases to verify this behavior:

1. **Test 1**: Directory exists, no dist target defined
2. **Test 2**: Directory doesn't exist, no dist target defined  
3. **Test 3**: Directory exists, with .PHONY dist target
4. **Test 4**: File target with full path

## Expected Results

- Test 1: "make: 'dist' is up to date"
- Test 2: "make: *** No rule to make target 'dist'"
- Test 3: Should execute the recipe
- Test 4: Should only work with full path `dist/myapp-1.0.tar.gz`