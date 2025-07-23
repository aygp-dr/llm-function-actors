#!/bin/bash
# Quick start script for Ollama tool validation

echo "Ollama Tool Calling Validation - Quick Start"
echo "==========================================="
echo ""

# Check if Ollama is installed
if ! command -v ollama &> /dev/null; then
    echo "❌ Ollama is not installed"
    echo "Install with: curl -fsSL https://ollama.ai/install.sh | sh"
    exit 1
fi

echo "✓ Ollama is installed"

# Check if Ollama is running
if ! curl -s http://localhost:11434/api/tags >/dev/null 2>&1; then
    echo "❌ Ollama is not running"
    echo "Start with: ollama serve"
    exit 1
fi

echo "✓ Ollama is running"

# Check for Llama 3.2 models
echo ""
echo "Checking for Llama 3.2 models..."
if ollama list | grep -q "llama3.2:3b"; then
    echo "✓ llama3.2:3b is available"
else
    echo "⚠ llama3.2:3b not found"
    echo "Pull with: ollama pull llama3.2:3b"
fi

if ollama list | grep -q "llama3.2:1b"; then
    echo "✓ llama3.2:1b is available"
else
    echo "⚠ llama3.2:1b not found"
    echo "Pull with: ollama pull llama3.2:1b"
fi

echo ""
echo "For SSH tunnel usage:"
echo "ssh -R 11434:localhost:11434 remote-host"
echo ""
echo "Ready to run experiments with: make run"