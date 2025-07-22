# Observation: 2025-07-22 - Packaging Research for REPL Integration

## Summary
Research on packaging the Guile Scheme LLM function calling simulator for team usage, focusing on WASM and JavaScript conversion options.

## Packaging Options Analysis

### 1. Guile to WebAssembly (WASM)

#### Current State
- **No direct Guile→WASM compiler exists**
- GNU Guile's runtime is C-based, theoretically compilable to WASM
- Significant challenges with threading, POSIX calls, and runtime size

#### Potential Approaches
1. **Emscripten Route**:
   - Compile Guile's C runtime to WASM
   - Bundle Scheme code as resources
   - Challenges: Large runtime (~10MB+), threading issues

2. **Scheme→C→WASM**:
   - Use Gambit or Chicken Scheme (better C integration)
   - Port actor model to simpler threading model
   - More feasible but requires rewrite

### 2. Guile to JavaScript

#### Direct Transpilation Options

1. **BiwaScheme Integration**:
   - JavaScript-based Scheme interpreter
   - Would require porting from Guile-specific features
   - Missing: ice-9 modules, SRFI-9 records, threading

2. **Schism Compiler**:
   - Compiles subset of Scheme to WASM
   - Very limited - no threading, limited SRFIs
   - Would require major rewrite

3. **Manual Translation**:
   - Rewrite actor model in JavaScript/TypeScript
   - Use Web Workers for actor concurrency
   - Most control but highest effort

### 3. Alternative Packaging Strategies

#### A. Docker Container with API
```dockerfile
FROM guile:3.0-slim
WORKDIR /app
COPY src/ ./src/
COPY examples/ ./examples/
EXPOSE 8080
CMD ["guile", "src/api-server.scm"]
```

Benefits:
- No code changes needed
- Full Guile compatibility
- RESTful/WebSocket interface possible

#### B. Language Server Protocol (LSP)
- Package as LSP server
- Integrates with VSCode, Emacs, etc.
- Function calls via LSP commands

#### C. Node.js Foreign Function Interface (FFI)
```javascript
// Using node-ffi-napi
const ffi = require('ffi-napi');
const guileLib = ffi.Library('libguile', {
  'scm_boot_guile': ['void', ['int', 'pointer', 'pointer']]
});
```

### 4. Recommended Approach: JavaScript Port

Given team REPL requirements, a JavaScript/TypeScript port offers best compatibility:

```javascript
// actor-system.js
class ActorSystem {
  constructor() {
    this.actors = new Map();
    this.channels = new Map();
  }

  spawn(name, behavior) {
    const channel = new MessageChannel();
    const actor = new Worker(behavior, { 
      workerData: { channel: channel.port1 },
      transferList: [channel.port1]
    });
    this.actors.set(name, actor);
    this.channels.set(name, channel.port2);
  }

  send(actorName, message) {
    this.channels.get(actorName).postMessage(message);
  }
}

// function-registry.js
class FunctionRegistry {
  constructor() {
    this.functions = new Map();
  }

  register(name, fn) {
    this.functions.set(name, fn);
  }

  async execute(name, args) {
    const fn = this.functions.get(name);
    if (!fn) throw new Error(`Unknown function: ${name}`);
    return await fn(...args);
  }
}
```

### 5. Integration Patterns for Team REPLs

#### A. Universal Module Definition (UMD)
```javascript
(function (root, factory) {
  if (typeof define === 'function' && define.amd) {
    define(['exports'], factory);
  } else if (typeof module === 'object' && module.exports) {
    factory(module.exports);
  } else {
    factory((root.LLMFunctionCaller = {}));
  }
}(typeof self !== 'undefined' ? self : this, function (exports) {
  // Implementation here
}));
```

#### B. ES Modules for Modern REPLs
```javascript
// llm-function-caller.mjs
export class ApplicationActor { /* ... */ }
export class LLMActor { /* ... */ }
export class FunctionRegistry { /* ... */ }
export function createSimulation(options) { /* ... */ }
```

#### C. REPL-Specific Integrations

1. **Node.js REPL**:
```javascript
// .replrc.js
const repl = require('repl');
const { createSimulation } = require('./llm-function-caller');

const replServer = repl.start('> ');
replServer.context.sim = createSimulation();
replServer.context.registerFunction = (name, fn) => 
  replServer.context.sim.registry.register(name, fn);
```

2. **Browser DevTools**:
```javascript
// Auto-inject into window
window.LLMSim = {
  create: () => new LLMFunctionCaller(),
  register: (name, fn) => window.llmSim.register(name, fn),
  run: (prompt) => window.llmSim.run(prompt)
};
```

3. **Jupyter/Observable**:
```javascript
// As observablehq notebook
viewof prompt = Inputs.text({label: "Prompt", value: "Calculate 5 + 3"})
sim = {
  const s = new LLMFunctionCaller();
  s.register('calculate', (a, b) => a + b);
  return s;
}
result = sim.run(prompt)
```

### 6. Proof of Concept: Minimal JS Port

```javascript
// minimal-port.js
class MessageQueue {
  constructor() {
    this.queue = [];
    this.waiters = [];
  }

  async send(message) {
    if (this.waiters.length > 0) {
      const waiter = this.waiters.shift();
      waiter.resolve(message);
    } else {
      this.queue.push(message);
    }
  }

  async receive() {
    if (this.queue.length > 0) {
      return this.queue.shift();
    }
    return new Promise(resolve => {
      this.waiters.push({ resolve });
    });
  }
}

async function applicationActor(toApp, toLLM, registry) {
  while (true) {
    const msg = await toApp.receive();
    
    switch (msg.type) {
      case 'start':
        await toLLM.send({
          type: 'prompt',
          content: {
            prompt: msg.prompt,
            functions: Array.from(registry.keys())
          },
          sender: 'application'
        });
        break;
        
      case 'function-call':
        const result = await registry.execute(msg.name, msg.args);
        await toLLM.send({
          type: 'function-result',
          content: { function: msg.name, result },
          sender: 'application'
        });
        break;
        
      case 'final-answer':
        console.log('Final Answer:', msg.answer);
        return;
    }
  }
}
```

## Recommendations

### For Immediate Use (Week 1)
1. Create Docker container with REST API
2. Document API endpoints for REPL integration
3. Provide curl/fetch examples

### For Broader Adoption (Month 1)
1. Port core actor system to TypeScript
2. Maintain message protocol compatibility
3. Package as npm module with UMD/ESM builds
4. Create REPL-specific adapters

### For Long-term (Quarter 1)
1. Investigate Rust + wasm-bindgen for performance
2. Create language-agnostic protocol (gRPC/WebSocket)
3. Build plugins for popular REPLs/IDEs

## Technical Considerations

### Threading Model Translation
- Guile threads → Web Workers (browser) / Worker Threads (Node.js)
- Message passing remains similar
- Need to handle transferable objects

### Module System Differences
- ice-9 modules → ES modules or CommonJS
- SRFI-9 records → ES6 classes
- Pattern matching → switch or pattern matching library

### Performance Implications
- JavaScript likely faster for message passing
- Guile better for numeric computation
- Consider hybrid approach for compute-intensive functions

## Conclusion

While direct Guile→WASM/JS conversion is not practical, a JavaScript port maintaining the same actor model and message protocol would provide the best developer experience for team members using various REPLs. The architectural patterns translate well, and the resulting system would be more portable and easier to integrate.