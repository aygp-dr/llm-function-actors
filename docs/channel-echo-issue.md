# Channel Echo Issue

## Problem

Our current implementation uses a single shared channel, causing messages to echo back to the sender.

## Current Behavior

```mermaid
sequenceDiagram
    participant App as Application Actor
    participant Ch as Shared Channel
    participant LLM as LLM Actor
    
    Note over App,LLM: Single shared channel causes echo
    
    LLM->>Ch: function-call message
    Ch-->>App: function-call (intended)
    Ch-->>LLM: function-call (ECHO - unintended!)
    
    App->>Ch: function-result message  
    Ch-->>LLM: function-result (intended)
    Ch-->>App: function-result (ECHO - unintended!)
    
    Note over App: "Unexpected message" warning
    Note over LLM: "Unexpected message" warning
```

## Root Cause

Both actors read from the same channel queue, so they see their own messages as well as messages from the other actor.

## Solution Options

### Option 1: Two Unidirectional Channels (Proper Fix)

```mermaid
sequenceDiagram
    participant App as Application Actor
    participant Ch1 as App→LLM Channel
    participant Ch2 as LLM→App Channel
    participant LLM as LLM Actor
    
    Note over App,LLM: Two channels prevent echo
    
    LLM->>Ch2: function-call
    Ch2->>App: function-call ✓
    
    App->>Ch1: function-result
    Ch1->>LLM: function-result ✓
    
    Note over App,LLM: No echo issues!
```

### Option 2: Message Filtering (Quick Fix)

Filter messages by sender to ignore our own messages.

## Implementation

We need to create two separate channels for proper actor communication.