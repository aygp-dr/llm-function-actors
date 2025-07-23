# LLM Actor Flow Visualization
Generated on: 2025-07-23 04:51:11

## Scenario: Function Call Flow
Prompt: "Can you calculate 2 + 2?"

### Sequence Diagram
```mermaid
sequenceDiagram
    participant User
    participant App as Application Actor
    participant LLM as LLM Actor
    participant Func as Function Registry

    User->>App: Can you calculate 2 + 2?
    App->>LLM: Forward prompt
    Note over LLM: Analyze intent<br/>Decision: function-call
    LLM->>App: Request function: calculate
    App->>Func: Execute calculate(2+2)
    Func-->>App: Result: 4
    App->>LLM: Function result
    LLM->>App: Final answer
    App->>User: The result is 4
```

### Flow Chart
```mermaid
graph TD
    Start([User Input])
    A3[Analyze Intent]
    A3 -->|Function needed| F3[Function Call]
    F5[Execute: calculate(2+2)]
    R6[Result: 4]
    End([Final Answer])
```


## Scenario: Direct Answer Flow
Prompt: "What is the capital of France?"

### Sequence Diagram
```mermaid
sequenceDiagram
    participant User
    participant App as Application Actor
    participant LLM as LLM Actor
    participant Func as Function Registry

    User->>App: What is the capital of France?
    App->>LLM: Forward prompt
    Note over LLM: Analyze intent<br/>Decision: direct-answer
    LLM->>App: Direct answer
    App->>User: Here's your answer...
```

### Flow Chart
```mermaid
graph TD
    Start([User Input])
    A3[Analyze Intent]
    A3 -->|Direct answer| D3[Generate Answer]
    End([Final Answer])
```


---
These diagrams show the actual control flow as specified in the sequence diagram.
