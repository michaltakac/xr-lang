#!/bin/bash
# Benchmark runner script for XR-Lang renderer
# Tests both traditional and GPU-driven rendering modes

echo "====================================="
echo "XR-Lang Performance Benchmark Suite"
echo "====================================="
echo ""

# Build in release mode for accurate benchmarks
echo "Building in release mode..."
cargo build --release --bin xr-dsl-desktop

if [ $? -ne 0 ]; then
    echo "Build failed!"
    exit 1
fi

echo ""
echo "Starting benchmarks..."
echo ""

# Test scenes
SCENES=(
    "examples/stress_test_10k.xrdsl"
    "examples/massive_grid_test.xrdsl"
)

for scene in "${SCENES[@]}"; do
    echo "----------------------------------------"
    echo "Testing: $scene"
    echo "----------------------------------------"
    
    # Run with traditional renderer
    echo "Traditional renderer:"
    BENCHMARK=1 timeout 15 cargo run --release --bin xr-dsl-desktop -- "$scene" 2>&1 | grep -E "warmup|complete|Current" &
    wait
    
    # Note: GPU-driven renderer would be tested with F4 key toggle
    # This requires manual intervention for now
    
    echo ""
done

echo "====================================="
echo "Benchmark Results"
echo "====================================="
echo ""

# Display results
if [ -f benchmark_results.csv ]; then
    echo "Latest results:"
    tail -5 benchmark_results.csv | column -t -s,
else
    echo "No results file found"
fi

echo ""
echo "Full results saved to: benchmark_results.csv"
echo ""
echo "Performance Tips:"
echo "- Press F1 to toggle performance overlay"
echo "- Press F2 to start benchmark (10 seconds)"
echo "- Press F3 to stop benchmark early"
echo "- Press F4 to toggle GPU-driven renderer (experimental)"
echo ""