#!/bin/bash
# Test Ollama tool calling capabilities
# Run this on nexushive (or any machine with SSH tunnel to Mac's Ollama)

echo "==================================="
echo "Ollama Tool Calling Test"
echo "==================================="
echo ""

# Check if Ollama is accessible
echo "1. Checking Ollama connection..."
if curl -s http://localhost:11434/api/tags > /dev/null; then
    echo "✓ Ollama is accessible"
else
    echo "✗ Cannot connect to Ollama at localhost:11434"
    echo "  Make sure SSH tunnel is active: ssh -R 11434:localhost:11434 your-mac"
    exit 1
fi

echo ""
echo "2. Testing basic tool calling with calculate function..."
echo "   Question: What is 25 * 4?"
echo ""

# Test tool calling
RESPONSE=$(curl -s -X POST http://localhost:11434/api/chat \
  -d '{
    "model": "llama3.2:3b",
    "messages": [{"role": "user", "content": "What is 25 * 4?"}],
    "tools": [{
      "type": "function",
      "function": {
        "name": "calculate",
        "description": "Perform math calculations",
        "parameters": {
          "type": "object",
          "properties": {
            "expression": {"type": "string", "description": "Math expression to evaluate"}
          },
          "required": ["expression"]
        }
      }
    }],
    "stream": false
  }')

echo "Response:"
echo "$RESPONSE" | jq '.'

# Check if tool was called
if echo "$RESPONSE" | jq -e '.message.tool_calls' > /dev/null 2>&1; then
    echo ""
    echo "✓ Tool call detected!"
    echo "Tool calls:"
    echo "$RESPONSE" | jq '.message.tool_calls'
else
    echo ""
    echo "⚠ No tool calls detected in response"
fi

echo ""
echo "3. Testing file operation tools..."
echo "   Question: List all .scm files in the current directory"
echo ""

RESPONSE2=$(curl -s -X POST http://localhost:11434/api/chat \
  -d '{
    "model": "llama3.2:3b",
    "messages": [{"role": "user", "content": "List all .scm files in the current directory"}],
    "tools": [{
      "type": "function",
      "function": {
        "name": "list_files",
        "description": "List files in a directory",
        "parameters": {
          "type": "object",
          "properties": {
            "directory": {"type": "string", "description": "Directory path"},
            "pattern": {"type": "string", "description": "File pattern to match"}
          },
          "required": ["directory"]
        }
      }
    }],
    "stream": false
  }')

echo "Response:"
echo "$RESPONSE2" | jq '.message.content // .message' 2>/dev/null || echo "$RESPONSE2"

echo ""
echo "==================================="
echo "Test complete!"
echo ""
echo "Note: If tool calls aren't working:"
echo "- Try llama3.1:8b (older but stable)"
echo "- Or use llama3.2:3b (better tool support than 1b)"
echo "- Ensure model is fully downloaded: ollama pull llama3.2:3b"