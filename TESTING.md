# XR-Lang Testing Documentation

## Overview
XR-Lang includes comprehensive unit and integration tests to ensure the DSL parser, VM interpreter, and WebGPU compilation work correctly.

## Test Structure

```
xr-lang/
├── dsl/tests/
│   └── parser_tests.rs         # DSL parser unit tests
├── gpu/tests/
│   ├── code_sync_test.rs       # Code synchronization tests
│   └── integration_tests.rs    # WebGPU compilation integration tests
├── vm/tests/
│   └── interpreter_tests.rs    # VM interpreter tests
├── test_runner.sh              # Bash script for running all tests
├── Makefile                    # Convenient test commands
└── .github/workflows/ci.yml    # GitHub Actions CI configuration
```

## Running Tests

### Quick Start
```bash
# Run all tests
make test

# Run specific test categories
make test-dsl    # DSL parser tests only
make test-vm     # VM interpreter tests only
make test-gpu    # GPU/rendering tests only

# Run integration tests
make test-integration

# Run full CI suite locally
make ci
```

### Using Cargo Directly
```bash
# Run all tests
cargo test --all-features --workspace

# Run tests for specific crate
cargo test -p dsl
cargo test -p vm
cargo test -p gpu

# Run with verbose output
cargo test -- --nocapture

# Run specific test
cargo test test_parse_numbers
```

### Using Test Runner Script
```bash
# Make executable (first time only)
chmod +x test_runner.sh

# Run all tests with colored output
./test_runner.sh
```

## Test Categories

### 1. DSL Parser Tests (`dsl/tests/parser_tests.rs`)

#### Basic Expression Tests
- **Numbers**: Integer, float, negative numbers
- **Symbols**: Identifiers, operators, special symbols
- **Lists**: S-expressions, nested lists
- **Vectors**: Array literals
- **Strings**: String literals, escape sequences
- **Comments**: Line comments

#### Scene Construction Tests
- **Camera**: Position, target, FOV settings
- **Objects**: Cubes, spheres, planes with transforms
- **Behaviors**: State, event handlers, update functions
- **Scenes**: Complete scene definitions

#### Macro Expansion Tests
- **dotimes**: Loop expansion for procedural generation
- **loop**: Common Lisp-style loops
- **Math functions**: sin, cos, tan, sqrt, pow, abs, floor, ceil

#### Color Parsing Tests
- **RGB**: Direct color values
- **RGBA**: Colors with alpha
- **Hex**: Hexadecimal color strings
- **HSL/HSLA**: Hue, saturation, lightness

### 2. Integration Tests (`gpu/tests/integration_tests.rs`)

#### Scene Compilation Tests
- Basic scene to WebGPU structures
- Multiple objects handling
- Meta directives preservation
- Material compilation
- UI element compilation

#### Behavior Compilation Tests
- Basic behavior with state
- Multiple event handlers
- Complex state initialization

#### Macro Expansion Tests
- dotimes expansion verification
- loop macro expansion
- Nested macro handling

#### Math Function Tests
- Math functions in behaviors
- All trigonometric functions

#### Reconciliation Tests
- Scene update detection
- Object add/remove operations

### 3. Code Synchronization Tests (`gpu/tests/code_sync_test.rs`)
- Live mode code writing
- Manual edit detection
- Camera sync operations
- Object transform sync

## Validation Tool

The project includes a DSL validation binary for checking syntax and semantic correctness:

```bash
# Build the validation tool
cargo build --bin validate-dsl

# Validate a single file
cargo run --bin validate-dsl -- examples/spinning_cubes.xrdsl

# Validate with verbose output
cargo run --bin validate-dsl -- examples/test.xrdsl --verbose

# Validate all examples
for file in examples/*.xrdsl; do
    cargo run --bin validate-dsl -- "$file"
done
```

## Continuous Integration

The project uses GitHub Actions for CI with the following jobs:

1. **Test Suite**: Runs on multiple OS (Ubuntu, macOS, Windows) and Rust versions (stable, beta)
2. **Integration Tests**: Comprehensive integration testing
3. **Example Validation**: Validates all example DSL files
4. **Code Coverage**: Generates coverage reports with tarpaulin

### CI Workflow Triggers
- Push to `main` or `develop` branches
- Pull requests to `main` branch

## Code Coverage

Generate local coverage reports:

```bash
# Install tarpaulin
cargo install cargo-tarpaulin

# Generate HTML report
make coverage

# Generate XML report for CI
cargo tarpaulin --verbose --all-features --workspace --timeout 120 --out xml
```

## Best Practices

### Writing Tests

1. **Test Naming**: Use descriptive names that explain what is being tested
   ```rust
   #[test]
   fn test_parse_nested_lists_with_symbols() { ... }
   ```

2. **Test Organization**: Group related tests in modules
   ```rust
   mod parser_scene_tests {
       #[test]
       fn test_parse_camera() { ... }
       
       #[test]
       fn test_parse_object() { ... }
   }
   ```

3. **Assertions**: Use specific assertions for better error messages
   ```rust
   assert_eq!(result.len(), 3);
   assert!(matches!(result[0], Expr::I32(42)));
   ```

4. **Test Data**: Use realistic examples from actual use cases

### Running Tests in Development

1. **Watch Mode**: Automatically run tests on file changes
   ```bash
   make watch
   # or
   cargo watch -x test
   ```

2. **Quick Tests**: Run only fast unit tests during development
   ```bash
   make quick-test
   ```

3. **Specific Tests**: Focus on the area you're working on
   ```bash
   cargo test -p dsl test_parse_
   ```

## Troubleshooting

### Common Issues

1. **GPU Tests Failing in CI**: GPU tests may fail in CI environments without GPU support. These are marked with `continue-on-error: true`

2. **Platform-Specific Failures**: Some tests may behave differently on Windows vs Unix. Use conditional compilation when necessary:
   ```rust
   #[cfg(unix)]
   #[test]
   fn test_unix_specific() { ... }
   ```

3. **Async Test Issues**: For async tests, use appropriate runtime:
   ```rust
   #[tokio::test]
   async fn test_async_operation() { ... }
   ```

## Performance Testing

For performance-critical code, use criterion for benchmarking:

```bash
# Run benchmarks
cargo bench

# Compare with baseline
cargo bench -- --baseline master
```

## Test Coverage Goals

- **Parser**: >90% coverage for all parsing functions
- **VM**: >85% coverage for interpreter and behavior execution
- **GPU**: >70% coverage (excluding GPU-specific rendering)
- **Integration**: Key user workflows covered

## Contributing Tests

When adding new features:

1. Write tests BEFORE implementing the feature (TDD)
2. Include both positive and negative test cases
3. Add integration tests for user-facing features
4. Update this documentation if adding new test categories

## Resources

- [Rust Testing Book](https://doc.rust-lang.org/book/ch11-00-testing.html)
- [Cargo Test Documentation](https://doc.rust-lang.org/cargo/commands/cargo-test.html)
- [Property-Based Testing with PropTest](https://github.com/proptest-rs/proptest)
- [Criterion.rs Benchmarking](https://github.com/bheisler/criterion.rs)