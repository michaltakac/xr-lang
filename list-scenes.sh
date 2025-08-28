#!/bin/bash

# List all available XR-DSL scenes with descriptions

echo "📋 Available XR-DSL Scenes:"
echo "=========================="
echo ""

for file in examples/*.xrdsl; do
    if [ -f "$file" ]; then
        name=$(basename "$file" .xrdsl)
        # Extract first comment line as description
        desc=$(grep -m1 "^;" "$file" | sed 's/^; *//')
        printf "%-20s - %s\n" "$name" "$desc"
    fi
done

echo ""
echo "🎮 Run a scene with:"
echo "  ./run-scene.sh <scene_name>"
echo ""
echo "Or directly with cargo:"
echo "  cargo run -p desktop -- examples/<scene_name>.xrdsl"