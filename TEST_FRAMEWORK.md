# XR-Lang Test Framework Documentation

## Overview

The XR-Lang Test Framework provides a comprehensive headless testing system for XR-Lang programs. It enables programmatic testing, debugging, and introspection without requiring a graphical interface or manual interaction. Think of it as "Playwright for 3D/XR environments."

## Features

### 🎯 Core Capabilities

- **Headless Execution**: Run XR-Lang code without UI
- **Scene State Validation**: Capture and verify 3D scene state
- **Hot-Reload Testing**: Test live code updates
- **Interaction Chaining**: Sequence complex test scenarios
- **Performance Profiling**: Measure execution performance
- **Debug Introspection**: Deep inspection of runtime state
- **Output Capture**: Record all program output
- **Snapshot Comparison**: Compare scene states over time

## Architecture

```
┌─────────────────────────────────────────┐
│           Test Framework                 │
├─────────────────────────────────────────┤
│  TestHarness                            │
│  ├── Evaluator (XR-Lang runtime)        │
│  ├── Scene State Manager                │
│  ├── Assertion Engine                   │
│  ├── Performance Metrics                │
│  └── Output Capture                     │
├─────────────────────────────────────────┤
│  Introspector                           │
│  ├── Scene Graph Analysis               │
│  ├── Execution Tracing                  │
│  └── Memory Profiling                   │
├─────────────────────────────────────────┤
│  TestSuite & TestRunner                 │
│  ├── Test Organization                  │
│  ├── Report Generation                  │
│  └── CLI Interface                      │
└─────────────────────────────────────────┘
```

## Quick Start

### Writing a Simple Test

```rust
use vm::test_framework::*;

fn test_camera_creation(harness: &mut TestHarness) -> TestResult {
    // Execute XRL code
    harness.execute(r#"
        (define cam (create-perspective-camera 
            :position [0 5 10]
            :fov 60))
    "#)?;
    
    // Make assertions
    let scene = Introspector::get_scene_graph();
    assert_eq!(scene.stats.camera_count, 1);
    
    harness.get_results("Camera Test")
}
```

### Running Tests

```bash
# Run all tests
cargo test

# Run with verbose output
cargo test -- --nocapture

# Run specific test suite
cargo test camera_tests

# Use the test runner CLI
cargo run --bin test_runner -- --verbose
```

## Test Harness API

### Core Methods

#### `execute(source: &str) -> Result<Value, String>`
Execute XRL code and return the result.

```rust
let result = harness.execute("(+ 1 2 3)")?;
```

#### `execute_file(path: &str) -> Result<Value, String>`
Execute XRL code from a file.

```rust
harness.execute_file("examples/scene.xrl")?;
```

#### `capture_scene_snapshot(label: &str) -> SceneSnapshot`
Capture the current scene state.

```rust
let snapshot = harness.capture_scene_snapshot("Before changes");
println!("Nodes: {}", snapshot.nodes.len());
```

### Assertions

#### `assert_object_exists(id: ObjectId, description: &str) -> bool`
Verify an object exists in the scene.

```rust
harness.assert_object_exists(camera_id, "Camera should exist");
```

#### `assert_position(id: ObjectId, expected: Vec3, tolerance: f32) -> bool`
Verify object position within tolerance.

```rust
harness.assert_position(cube_id, Vec3::new(0.0, 0.0, 0.0), 0.01);
```

#### `assert_camera_fov(id: ObjectId, expected_fov: f32, tolerance: f32) -> bool`
Verify camera field of view.

```rust
harness.assert_camera_fov(cam_id, 60.0, 0.1);
```

### Interaction Chaining

Chain multiple interactions for complex test scenarios:

```rust
let interactions = vec![
    Interaction::Execute("(define cam (create-camera))".to_string()),
    Interaction::Wait(Duration::from_millis(100)),
    Interaction::Snapshot("After camera".to_string()),
    Interaction::Execute("(move-camera cam [5 0 0])".to_string()),
    Interaction::Assert(Box::new(|h| {
        // Custom assertion
        let scene = Introspector::get_scene_graph();
        assert_eq!(scene.stats.camera_count, 1);
    })),
];

harness.chain_interactions(interactions)?;
```

## Introspection API

### Scene Graph Introspection

```rust
let scene_graph = Introspector::get_scene_graph();

println!("Cameras: {}", scene_graph.stats.camera_count);
println!("Nodes: {}", scene_graph.stats.node_count);

for camera in &scene_graph.cameras {
    println!("Camera at {:?}, FOV: {}", camera.position, camera.fov);
}
```

### Execution Tracing

```rust
let trace = Introspector::trace_execution("(+ 1 2 3)");

println!("Parse time: {:?}", trace.parse_time);
println!("Eval time: {:?}", trace.total_time);

for step in &trace.evaluations {
    println!("{} -> {} ({:?})", step.expr, step.result, step.duration);
}
```

### Memory Profiling

```rust
let memory = Introspector::estimate_memory_usage();
println!("Total memory: {} bytes", memory.total);
```

## Test Organization

### Creating Test Suites

```rust
pub fn create_my_test_suite() -> TestSuite {
    let mut suite = TestSuite::new("My Tests");
    
    suite.add_test(test_function_1);
    suite.add_test(test_function_2);
    
    suite.set_setup(|harness| {
        // Setup code runs before each test
        harness.reset();
    });
    
    suite
}
```

### Test Runner

```rust
let mut runner = TestRunner::new();
runner.verbose = true;

runner.add_suite(create_camera_test_suite());
runner.add_suite(create_hot_reload_test_suite());

let report = runner.run_all();
runner.print_summary(&report);
```

## Hot-Reload Testing

Test hot-reload functionality:

```rust
fn test_hot_reload(harness: &mut TestHarness) -> TestResult {
    // Initial state
    harness.execute("(define cube (create-cube [0 0 0]))")?;
    let snapshot1 = harness.capture_scene_snapshot("Before");
    
    // Simulate hot reload with changes
    harness.simulate_hot_reload(
        "scene.xrl",
        "(define cube (create-cube [0 0 0]))\n(define sphere (create-sphere [3 0 0]))"
    )?;
    
    let snapshot2 = harness.capture_scene_snapshot("After");
    
    // Verify new object was added
    assert!(snapshot2.nodes.len() > snapshot1.nodes.len());
    
    harness.get_results("Hot Reload Test")
}
```

## Performance Testing

```rust
fn test_performance(harness: &mut TestHarness) -> TestResult {
    let start = Instant::now();
    
    // Execute performance-critical code
    harness.execute(complex_code)?;
    
    let elapsed = start.elapsed();
    
    // Check performance metrics
    let metrics = &harness.performance_metrics;
    assert!(metrics.eval_time < Duration::from_millis(100));
    assert!(metrics.parse_time < Duration::from_millis(10));
    
    harness.get_results("Performance Test")
}
```

## Debug Output

### Captured Output

All `println` calls are captured:

```rust
harness.execute(r#"
    (println "Debug message")
    (println "Value:" (+ 1 2))
"#)?;

let output = harness.captured_output.lock().unwrap();
assert_eq!(output[0], "Debug message");
assert_eq!(output[1], "Value: 3");
```

### Export Reports

```bash
# Export JSON report
cargo run --bin test_runner -- --json

# Export detailed debug log
cargo run --bin test_runner -- --debug-log
```

## Example Test Output

```
╔════════════════════════════════════════════════════════════╗
║           XR-Lang Test Runner (Headless Mode)             ║
╚════════════════════════════════════════════════════════════╝

=== Running Test Suite: Camera Tests ===

  Test: Camera Creation Test
  Status: ✅ PASSED
  Message: 3/3 assertions passed
  Duration: 12.5ms

  Test: Camera Manipulation Test
  Status: ✅ PASSED
  Message: 4/4 assertions passed
  Duration: 8.3ms

=== Running Test Suite: Hot Reload Tests ===

  Test: Basic Hot Reload Test
  Status: ✅ PASSED
  Message: 2/2 assertions passed
  Duration: 25.1ms

============================================================
TEST SUMMARY
============================================================
Total Tests: 7
Passed: 7 ✅
Failed: 0 ❌
Duration: 125.3ms

Pass Rate: 100.0%

🎉 All tests passed!
```

## Best Practices

1. **Reset Between Tests**: Always call `harness.reset()` to ensure clean state
2. **Use Snapshots**: Capture snapshots for debugging test failures
3. **Chain Complex Scenarios**: Use interaction chaining for integration tests
4. **Profile Performance**: Monitor execution times to catch regressions
5. **Capture Output**: Use captured output for debugging
6. **Export Reports**: Generate JSON/debug logs for CI/CD integration

## Debugging Failed Tests

When a test fails, the framework provides detailed information:

- Failed assertion details (expected vs actual)
- Captured output leading to failure
- Scene snapshots before/after failure
- Execution trace with timings
- Memory usage at failure point

## Integration with CI/CD

The test runner returns appropriate exit codes:
- `0`: All tests passed
- `1`: One or more tests failed

This makes it easy to integrate with CI/CD pipelines:

```yaml
# GitHub Actions example
- name: Run XR-Lang Tests
  run: cargo run --bin test_runner -- --json
  
- name: Upload test results
  uses: actions/upload-artifact@v2
  with:
    name: test-results
    path: test_results.json
```

## Advanced Usage

### Custom Assertions

```rust
Interaction::Assert(Box::new(|harness| {
    let scene = Introspector::get_scene_graph();
    
    // Complex custom assertion
    let cameras_valid = scene.cameras.values()
        .all(|c| c.fov > 10.0 && c.fov < 120.0);
    
    assert!(cameras_valid, "All cameras should have valid FOV");
}))
```

### Performance Benchmarking

```rust
fn benchmark_scene_creation(harness: &mut TestHarness) -> TestResult {
    let mut times = Vec::new();
    
    for i in 0..100 {
        harness.reset();
        let start = Instant::now();
        
        harness.execute(&format!(
            "(define cube{} (create-cube [{} 0 0]))", i, i
        ))?;
        
        times.push(start.elapsed());
    }
    
    let avg = times.iter().sum::<Duration>() / times.len() as u32;
    println!("Average creation time: {:?}", avg);
    
    harness.get_results("Benchmark")
}
```

## Future Enhancements

- [ ] Visual diff generation for scene changes
- [ ] Parallel test execution
- [ ] Property-based testing
- [ ] Mutation testing
- [ ] Coverage reporting
- [ ] Record/replay functionality
- [ ] Network testing for multiplayer
- [ ] VR controller input simulation

## Conclusion

The XR-Lang Test Framework provides comprehensive testing capabilities for XR-Lang programs without requiring manual interaction or visual inspection. It enables developers to:

- Write reliable, repeatable tests
- Debug complex 3D/XR scenarios
- Profile performance
- Validate hot-reload behavior
- Ensure code quality through CI/CD

This framework is essential for maintaining code quality and catching regressions in XR-Lang applications.