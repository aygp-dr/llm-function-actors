#!/bin/sh
# Generate diagram images from mermaid files

# Method 1: Using mermaid-cli (if installed)
if command -v mmdc >/dev/null 2>&1; then
    echo "Generating diagrams with mermaid-cli..."
    mmdc -i docs/llm-function-calling-flow.mmd -o docs/llm-function-calling-flow.png -t dark -b white
    echo "Generated docs/llm-function-calling-flow.png"
    exit 0
fi

# Method 2: Using Docker (if available)
if command -v docker >/dev/null 2>&1; then
    echo "Generating diagrams with Docker..."
    docker run --rm -v "$PWD:/data" minlag/mermaid-cli:latest \
        -i /data/docs/llm-function-calling-flow.mmd \
        -o /data/docs/llm-function-calling-flow.png \
        -t dark -b white
    echo "Generated docs/llm-function-calling-flow.png"
    exit 0
fi

# Method 3: Using npm/npx (if available)
if command -v npx >/dev/null 2>&1; then
    echo "Generating diagrams with npx..."
    npx -p @mermaid-js/mermaid-cli mmdc \
        -i docs/llm-function-calling-flow.mmd \
        -o docs/llm-function-calling-flow.png \
        -t dark -b white
    echo "Generated docs/llm-function-calling-flow.png"
    exit 0
fi

echo "Error: No mermaid renderer found!"
echo "Please install one of:"
echo "  - mermaid-cli: npm install -g @mermaid-js/mermaid-cli"
echo "  - Docker: docker pull minlag/mermaid-cli"
echo ""
echo "Alternative: Use https://mermaid.live/ to render online"
exit 1