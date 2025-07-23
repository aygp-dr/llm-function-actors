#!/bin/sh
# Run LLM Actor Mock Experiments

echo "================================"
echo "LLM Actor Mock Experiments"
echo "================================"
echo ""

# Basic mock
echo "1. Running basic LLM actor mock..."
echo "-----------------------------------"
guile3 llm-actor-mock.scm

echo ""
echo ""

# Multi-step mock
echo "2. Running multi-step scenarios..."
echo "-----------------------------------"
guile3 multi-step-mock.scm

echo ""
echo ""

# Flow visualization
echo "3. Generating flow visualizations..."
echo "------------------------------------"
guile3 flow-visualizer.scm > flow-diagrams.md

echo ""
echo "Flow diagrams saved to flow-diagrams.md"
echo ""
echo "Experiments complete!"