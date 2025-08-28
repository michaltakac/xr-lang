#!/bin/bash

# Helper script to run XR-DSL desktop app with different scenes

if [ $# -eq 0 ]; then
    echo "Usage: ./run-scene.sh <scene_name>"
    echo ""
    echo "Available scenes:"
    ls examples/*.xrdsl 2>/dev/null | xargs -n1 basename | sed 's/\.xrdsl$//'
    echo ""
    echo "Examples:"
    echo "  ./run-scene.sh spinning_cubes"
    echo "  ./run-scene.sh rotation_test"
    echo "  ./run-scene.sh interactive_test"
    echo "  ./run-scene.sh test_text"
    exit 1
fi

SCENE_NAME="$1"
SCENE_FILE="examples/${SCENE_NAME}.xrdsl"

if [ ! -f "$SCENE_FILE" ]; then
    echo "Error: Scene file '$SCENE_FILE' not found"
    echo ""
    echo "Available scenes:"
    ls examples/*.xrdsl 2>/dev/null | xargs -n1 basename | sed 's/\.xrdsl$//'
    exit 1
fi

echo "🚀 Loading scene: $SCENE_FILE"
cargo run -p desktop -- "$SCENE_FILE"