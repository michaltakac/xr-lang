#!/bin/bash
# Test runner script for XR-Lang
# Runs all unit and integration tests with proper reporting

set -e

echo "========================================="
echo "XR-Lang Test Suite"
echo "========================================="
echo ""

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
NC='\033[0m' # No Color

# Track test results
TOTAL_TESTS=0
PASSED_TESTS=0
FAILED_TESTS=0

# Function to run tests for a specific crate
run_crate_tests() {
    local crate=$1
    local description=$2
    
    echo -e "${YELLOW}Running $description tests...${NC}"
    echo "----------------------------------------"
    
    if cargo test -p $crate --lib --tests 2>&1 | tee test_output.tmp; then
        local tests_run=$(grep -E "test result:|running" test_output.tmp | tail -1)
        echo -e "${GREEN}✅ $description tests passed${NC}"
        echo "   $tests_run"
        PASSED_TESTS=$((PASSED_TESTS + 1))
    else
        echo -e "${RED}❌ $description tests failed${NC}"
        FAILED_TESTS=$((FAILED_TESTS + 1))
    fi
    
    TOTAL_TESTS=$((TOTAL_TESTS + 1))
    rm -f test_output.tmp
    echo ""
}

# Function to run integration tests
run_integration_tests() {
    echo -e "${YELLOW}Running integration tests...${NC}"
    echo "----------------------------------------"
    
    if cargo test --test '*' 2>&1 | tee test_output.tmp; then
        local tests_run=$(grep -E "test result:|running" test_output.tmp | tail -1)
        echo -e "${GREEN}✅ Integration tests passed${NC}"
        echo "   $tests_run"
        PASSED_TESTS=$((PASSED_TESTS + 1))
    else
        echo -e "${RED}❌ Integration tests failed${NC}"
        FAILED_TESTS=$((FAILED_TESTS + 1))
    fi
    
    TOTAL_TESTS=$((TOTAL_TESTS + 1))
    rm -f test_output.tmp
    echo ""
}

# Function to run example validation
validate_examples() {
    echo -e "${YELLOW}Validating example DSL files...${NC}"
    echo "----------------------------------------"
    
    local example_count=0
    local valid_count=0
    
    for file in examples/*.xrdsl; do
        if [ -f "$file" ]; then
            example_count=$((example_count + 1))
            echo -n "  Checking $(basename $file)... "
            
            # Try to parse the file (we'll need a validation tool for this)
            if cargo run --bin validate-dsl -- "$file" > /dev/null 2>&1; then
                echo -e "${GREEN}✓${NC}"
                valid_count=$((valid_count + 1))
            else
                echo -e "${RED}✗${NC}"
            fi
        fi
    done
    
    echo ""
    echo "  Examples validated: $valid_count / $example_count"
    
    if [ $valid_count -eq $example_count ]; then
        echo -e "${GREEN}✅ All examples valid${NC}"
        PASSED_TESTS=$((PASSED_TESTS + 1))
    else
        echo -e "${RED}❌ Some examples invalid${NC}"
        FAILED_TESTS=$((FAILED_TESTS + 1))
    fi
    
    TOTAL_TESTS=$((TOTAL_TESTS + 1))
    echo ""
}

# Main test execution
echo "🏃 Running test suite..."
echo ""

# Run tests for each crate
run_crate_tests "dsl" "DSL Parser"
run_crate_tests "vm" "VM/Interpreter"
run_crate_tests "gpu" "GPU/Rendering"

# Run integration tests
run_integration_tests

# Validate examples (if validation binary exists)
if cargo build --bin validate-dsl 2>/dev/null; then
    validate_examples
fi

# Summary
echo "========================================="
echo "Test Summary"
echo "========================================="
echo ""
echo "Total test categories: $TOTAL_TESTS"
echo -e "Passed: ${GREEN}$PASSED_TESTS${NC}"
echo -e "Failed: ${RED}$FAILED_TESTS${NC}"
echo ""

if [ $FAILED_TESTS -eq 0 ]; then
    echo -e "${GREEN}🎉 All tests passed!${NC}"
    exit 0
else
    echo -e "${RED}💔 Some tests failed${NC}"
    exit 1
fi