#!/bin/bash

# Quick 3D testing script for XR-DSL

set -e

echo "🎮 XR-DSL 3D Rendering Test Suite"
echo "================================="

# Function to test compilation without running
test_compile() {
    echo "🔧 Testing $1 compilation..."
    if cargo check -p "$1" --quiet; then
        echo "✅ $1 compiles successfully"
        return 0
    else
        echo "❌ $1 compilation failed"
        return 1
    fi
}

# Test core components
echo ""
echo "📦 Testing core components..."
test_compile "dsl" || exit 1
test_compile "gpu" || exit 1
test_compile "vm" || exit 1

# Test desktop app
echo ""
echo "🖥️  Testing desktop 3D app..."
test_compile "desktop" || exit 1

# Test web compilation (but don't build wasm yet)
echo ""
echo "🌐 Testing web components..."
test_compile "web" || exit 1

echo ""
echo "✨ All tests passed!"
echo ""
echo "🚀 Ready to run:"
echo "   Desktop: cargo run -p desktop"
echo "   Web:     cd app-hosts/web && ./build.sh"
echo ""
echo "📖 See TESTING_3D.md for detailed instructions"