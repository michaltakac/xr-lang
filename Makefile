# XR-Lang Makefile
# Convenient commands for development and testing

.PHONY: help test test-unit test-integration test-dsl test-vm test-gpu build clean fmt lint check run examples ci

# Default target
help:
	@echo "XR-Lang Development Commands"
	@echo "============================"
	@echo ""
	@echo "Testing:"
	@echo "  make test          - Run all tests"
	@echo "  make test-unit     - Run unit tests only"
	@echo "  make test-integration - Run integration tests"
	@echo "  make test-dsl      - Run DSL parser tests"
	@echo "  make test-vm       - Run VM/interpreter tests"
	@echo "  make test-gpu      - Run GPU/rendering tests"
	@echo ""
	@echo "Building:"
	@echo "  make build         - Build all crates"
	@echo "  make clean         - Clean build artifacts"
	@echo ""
	@echo "Code Quality:"
	@echo "  make fmt           - Format code with rustfmt"
	@echo "  make lint          - Run clippy linter"
	@echo "  make check         - Check code without building"
	@echo ""
	@echo "Running:"
	@echo "  make run           - Run desktop app with default example"
	@echo "  make examples      - Validate all example files"
	@echo ""
	@echo "CI:"
	@echo "  make ci            - Run full CI suite locally"

# Testing targets
test:
	@echo "🧪 Running all tests..."
	@cargo test --all-features --workspace

test-unit:
	@echo "🧪 Running unit tests..."
	@cargo test --lib --all-features --workspace

test-integration:
	@echo "🧪 Running integration tests..."
	@cargo test --test '*' --all-features

test-dsl:
	@echo "🧪 Testing DSL parser..."
	@cargo test -p dsl --all-features

test-vm:
	@echo "🧪 Testing VM/interpreter..."
	@cargo test -p vm --all-features

test-gpu:
	@echo "🧪 Testing GPU/rendering..."
	@cargo test -p gpu --all-features

# Building targets
build:
	@echo "🔨 Building all crates..."
	@cargo build --all-features --workspace

clean:
	@echo "🧹 Cleaning build artifacts..."
	@cargo clean
	@rm -f test_output.tmp

# Code quality targets
fmt:
	@echo "✨ Formatting code..."
	@cargo fmt --all

lint:
	@echo "🔍 Running clippy..."
	@cargo clippy --all-features --workspace -- -D warnings

check:
	@echo "✅ Checking code..."
	@cargo check --all-features --workspace

# Running targets
run:
	@echo "🚀 Running desktop app..."
	@cargo run -p desktop -- examples/spinning_cubes.xrdsl

examples:
	@echo "📝 Validating example files..."
	@for file in examples/*.xrdsl; do \
		echo "  Checking $$file..."; \
		cargo run -p desktop -- "$$file" --dry-run 2>/dev/null || echo "    ❌ Failed"; \
	done

# CI target - runs everything
ci: clean
	@echo "🤖 Running full CI suite..."
	@echo ""
	@echo "Step 1: Checking code..."
	@make check
	@echo ""
	@echo "Step 2: Running formatter check..."
	@cargo fmt -- --check
	@echo ""
	@echo "Step 3: Running linter..."
	@make lint
	@echo ""
	@echo "Step 4: Running tests..."
	@make test
	@echo ""
	@echo "Step 5: Validating examples..."
	@make examples
	@echo ""
	@echo "✅ CI suite complete!"

# Quick test - runs only fast tests
quick-test:
	@echo "⚡ Running quick tests..."
	@cargo test -p dsl --lib
	@cargo test -p vm --lib

# Watch for changes and run tests
watch:
	@echo "👁️  Watching for changes..."
	@cargo watch -x test -x clippy

# Generate test coverage (requires cargo-tarpaulin)
coverage:
	@echo "📊 Generating test coverage..."
	@cargo tarpaulin --verbose --all-features --workspace --timeout 120 --out html
	@echo "Coverage report generated in tarpaulin-report.html"

# Benchmark tests (requires cargo-criterion)
bench:
	@echo "⏱️  Running benchmarks..."
	@cargo bench

# Documentation
docs:
	@echo "📚 Generating documentation..."
	@cargo doc --all-features --no-deps --open

# Install development tools
install-tools:
	@echo "🔧 Installing development tools..."
	@cargo install cargo-watch || true
	@cargo install cargo-tarpaulin || true
	@cargo install cargo-criterion || true
	@echo "Tools installed!"