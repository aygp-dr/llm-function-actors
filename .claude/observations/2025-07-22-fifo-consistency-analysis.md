# Observation: 2025-07-22 - FIFO Communication Consistency Analysis

## Summary
The three agent commands (builder, observer, TRUE-OBSERVER-v2) have inconsistent FIFO definitions and communication patterns, which could cause issues in multi-agent collaboration.

## Detailed Analysis

### FIFO Definitions

#### 1. Observer Command (observer.md)
- **FIFO Path**: `/tmp/${PROJECT_NAME}-observer-comments.fifo`
- **Purpose**: Observer → Builder communication
- **Creation**: `mkfifo "$FIFO_PATH" 2>/dev/null`
- **Usage**: `echo "message" > "$FIFO_PATH" &`

#### 2. Builder Command (builder.md)
- **FIFO Reference**: Mentions checking observer messages via `claude /check-observer`
- **No explicit FIFO definition**
- **Missing**: Direct FIFO path specification or creation logic

#### 3. TRUE-OBSERVER-v2 Command
- **FIFO Paths Referenced**:
  - `/tmp/${PROJECT_NAME}-builder-excuses.fifo`
  - `/tmp/${PROJECT_NAME}-observer-comments.fifo`
- **Usage**: Read-only monitoring with `timeout 1 cat`
- **No creation logic**

### Inconsistencies Found

1. **Missing Builder FIFO**: No explicit builder-to-observer communication channel defined
2. **Builder Excuses FIFO**: Referenced in TRUE-OBSERVER but not created anywhere
3. **Check-Observer Command**: Referenced but not implemented in visible commands
4. **Timeout Variations**: TRUE-OBSERVER uses 1s timeout, builder startup suggests 2s

### Recommended Standardization

```bash
# Standard FIFO definitions for all agents
PROJECT_NAME=$(basename $(pwd))
OBSERVER_TO_BUILDER_FIFO="/tmp/${PROJECT_NAME}-observer-comments.fifo"
BUILDER_TO_OBSERVER_FIFO="/tmp/${PROJECT_NAME}-builder-excuses.fifo"
SYSTEM_STATUS_FIFO="/tmp/${PROJECT_NAME}-system-status.fifo"

# Creation function (add to all commands)
setup_communication_channels() {
    mkfifo "$OBSERVER_TO_BUILDER_FIFO" 2>/dev/null
    mkfifo "$BUILDER_TO_OBSERVER_FIFO" 2>/dev/null
    mkfifo "$SYSTEM_STATUS_FIFO" 2>/dev/null
}

# Standard timeout for reading
FIFO_READ_TIMEOUT=2
```

### Implementation Recommendations

1. **Update builder.md**:
   - Add explicit FIFO definitions
   - Implement builder-to-observer communication
   - Define the `/check-observer` command logic

2. **Update observer.md**:
   - Add builder-to-observer FIFO monitoring
   - Standardize timeout values

3. **Update TRUE-OBSERVER-v2.md**:
   - Add FIFO creation logic (even if just for verification)
   - Use consistent timeout values

4. **Create shared command**:
   - `/setup-comms` - Initialize all FIFOs for a project
   - `/check-comms` - Verify communication channels

### Benefits of Standardization

- **Portability**: Same communication pattern works across all projects
- **Reliability**: Consistent timeouts prevent hanging
- **Debugging**: Clear channel definitions aid troubleshooting
- **Scalability**: Easy to add more agents with defined patterns

## Conclusion

The current FIFO definitions are incomplete and inconsistent. Standardizing these across all three agent types would enable reliable multi-agent collaboration on any project.