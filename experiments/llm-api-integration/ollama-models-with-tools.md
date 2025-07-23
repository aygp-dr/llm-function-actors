# Ollama Models with Tool/Function Calling Support

Based on Ollama's tool support (v0.3.0+), here are models that support function calling:

## Officially Supported Models

### 1. **Llama 3.1**
```bash
ollama pull llama3.1
```
- Sizes: 8B, 70B, 405B
- Best for: General purpose with tool support
- Tool calling: Native support in 3.1+

### 2. **Mistral** 
```bash
ollama pull mistral
```
- Size: 7B
- Best for: Efficient inference with tools
- Tool calling: Supported

### 3. **Mixtral**
```bash
ollama pull mixtral
```
- Size: 8x7B 
- Best for: Complex reasoning with tools
- Tool calling: Supported

### 4. **Command-R**
```bash
ollama pull command-r
```
- Sizes: 35B, 104B
- Best for: RAG and tool use
- Tool calling: Optimized for it

### 5. **Qwen 2.5**
```bash
ollama pull qwen2.5
```
- Sizes: 0.5B to 72B
- Best for: Multilingual with tools
- Tool calling: Supported

### 6. **Hermes 3**
```bash
ollama pull hermes3
```
- Based on: Llama 3.1
- Best for: Advanced tool use
- Tool calling: Enhanced support

## Testing Tool Support

```bash
# Quick test script
cat > test-tools.sh << 'EOF'
#!/bin/bash

MODEL=${1:-llama3.1}

curl http://localhost:11434/api/chat -d '{
  "model": "'$MODEL'",
  "messages": [
    {
      "role": "user",
      "content": "What is the weather in San Francisco?"
    }
  ],
  "tools": [
    {
      "type": "function",
      "function": {
        "name": "get_weather",
        "description": "Get the weather for a location",
        "parameters": {
          "type": "object",
          "properties": {
            "location": {
              "type": "string",
              "description": "The location to get weather for"
            }
          },
          "required": ["location"]
        }
      }
    }
  ],
  "stream": false
}'
EOF

chmod +x test-tools.sh
./test-tools.sh llama3.1
```

## Model Comparison for Tool Calling

| Model | Size | Speed | Tool Accuracy | Best Use Case |
|-------|------|-------|---------------|---------------|
| llama3.1:8b | 8B | Fast | High | General purpose |
| mistral:7b | 7B | Very Fast | Good | Quick responses |
| mixtral:8x7b | 47B | Medium | Very High | Complex tools |
| command-r:35b | 35B | Medium | Excellent | RAG + Tools |
| qwen2.5:7b | 7B | Fast | Good | Multilingual |
| hermes3:8b | 8B | Fast | Very High | Advanced agents |

## Recommended Setup for Development

```bash
# Pull recommended models for tool calling
ollama pull llama3.1:8b      # Best overall
ollama pull mistral:7b        # Fastest
ollama pull command-r:35b     # Best accuracy (if you have RAM)

# Verify models
ollama list
```

## Memory Requirements

- 7B models: ~8GB RAM
- 8B models: ~10GB RAM  
- 35B models: ~40GB RAM
- 70B models: ~80GB RAM

Choose based on your system capabilities!