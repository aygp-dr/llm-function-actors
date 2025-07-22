# LLM Function Calling Architecture

This diagram shows the interaction flow between actors in the LLM function calling pattern.

```mermaid
sequenceDiagram
    participant User
    participant App as Application Actor
    participant LLM as LLM Actor
    participant Registry as Function Registry
    
    Note over User,Registry: Setup Phase
    App->>Registry: Register Functions
    Registry-->>App: Functions Available
    
    Note over User,Registry: Interaction Phase
    User->>App: Send Prompt
    App->>App: Add Function Definitions
    App->>LLM: Prompt + Available Functions
    
    Note over LLM: Decision Point
    alt Needs Function
        LLM->>App: Request Function Call<br/>(name + arguments)
        App->>Registry: Lookup Function
        Registry-->>App: Function Reference
        App->>App: Execute Function
        App->>LLM: Return Result
        LLM->>LLM: Process Result
        LLM->>App: Final Answer
    else Direct Answer
        LLM->>App: Final Answer
    end
    
    App->>User: Response
    
    Note over User,Registry: Optional: Multiple Function Calls
    opt Additional Tools Needed
        LLM->>App: Request Another Function
        App->>App: Execute
        App->>LLM: Result
        Note right of LLM: Loop until complete
    end
```

## Key Components

- **User**: Initiates requests with prompts
- **Application Actor**: Manages function registry and execution
- **LLM Actor**: Decides whether to call functions or respond directly
- **Function Registry**: Stores available functions and their implementations

## Flow Description

1. **Setup Phase**: Application registers available functions
2. **Request Phase**: User sends prompt, app adds function definitions
3. **Decision Phase**: LLM analyzes request and available functions
4. **Execution Phase**: If needed, LLM requests function execution
5. **Response Phase**: Final answer is sent back to user
6. **Loop**: Multiple function calls possible in single interaction