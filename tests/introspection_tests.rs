//! Introspection and Debugging Tests
//! Tests the ability to inspect, debug, and trace execution

use vm::test_framework::*;
use vm::intrinsics::Vec3;
use std::time::Duration;

/// Test scene graph introspection
fn test_scene_graph_introspection(harness: &mut TestHarness) -> TestResult {
    harness.reset();
    
    // Build a complex scene
    harness.execute(r#"
        ; Create multiple cameras
        (define cam1 (create-perspective-camera :position [0 5 10]))
        (define cam2 (create-orthographic-camera :position [0 20 0]))
        (define cam3 (orbit-camera :target [0 0 0] :radius 15))
        
        ; Create objects
        (define cube1 (create-cube [0 0 0]))
        (define cube2 (create-cube [3 0 0]))
        (define sphere1 (create-sphere [0 3 0]))
        
        ; Set active camera
        (set-active-camera cam1)
    "#).expect("Failed to build scene");
    
    // Introspect the scene
    let scene_graph = Introspector::get_scene_graph();
    
    // Verify scene structure
    assert_eq!(scene_graph.stats.camera_count, 3, "Should have 3 cameras");
    assert_eq!(scene_graph.stats.node_count, 3, "Should have 3 nodes");
    assert_eq!(scene_graph.stats.total_objects, 6, "Should have 6 total objects");
    assert!(scene_graph.active_camera.is_some(), "Should have active camera");
    
    // Verify we can access individual objects
    assert!(!scene_graph.nodes.is_empty(), "Should have nodes");
    assert!(!scene_graph.cameras.is_empty(), "Should have cameras");
    
    harness.get_results("Scene Graph Introspection Test")
}

/// Test execution tracing
fn test_execution_trace(harness: &mut TestHarness) -> TestResult {
    harness.reset();
    
    let code = r#"
        (define x 10)
        (define y 20)
        (define sum (+ x y))
        (println "Sum is:" sum)
        sum
    "#;
    
    // Trace execution
    let trace = Introspector::trace_execution(code);
    
    // Verify trace information
    assert!(trace.ast_nodes > 0, "Should have parsed AST nodes");
    assert!(!trace.evaluations.is_empty(), "Should have evaluation steps");
    assert!(trace.errors.is_empty(), "Should have no errors");
    assert!(trace.total_time > Duration::from_secs(0), "Should have execution time");
    
    // Check individual evaluation steps
    for step in &trace.evaluations {
        assert!(step.success, "All steps should succeed");
        assert!(step.duration >= Duration::from_secs(0), "Should have duration");
    }
    
    // Verify specific evaluations occurred
    let has_define = trace.evaluations.iter()
        .any(|s| s.expr.contains("define"));
    assert!(has_define, "Should have define expressions");
    
    harness.get_results("Execution Trace Test")
}

/// Test error tracing and debugging
fn test_error_debugging(harness: &mut TestHarness) -> TestResult {
    harness.reset();
    
    // Code with intentional errors
    let error_code = r#"
        (define x 10)
        (undefined-function x)  ; This will error
        (define y 20)           ; This won't execute
    "#;
    
    let trace = Introspector::trace_execution(error_code);
    
    // Should have captured the error
    assert!(!trace.errors.is_empty(), "Should have captured errors");
    
    // Find the failed evaluation
    let failed_step = trace.evaluations.iter()
        .find(|s| !s.success);
    assert!(failed_step.is_some(), "Should have a failed step");
    
    if let Some(step) = failed_step {
        assert!(step.result.contains("Undefined") || step.result.contains("undefined"),
            "Error should mention undefined function");
    }
    
    harness.get_results("Error Debugging Test")
}

/// Test memory usage estimation
fn test_memory_introspection(harness: &mut TestHarness) -> TestResult {
    harness.reset();
    
    // Get baseline memory
    let baseline = Introspector::estimate_memory_usage();
    
    // Create many objects
    for i in 0..10 {
        harness.execute(&format!(
            "(define cube{} (create-cube [{} 0 0]))", i, i
        )).expect("Failed to create cube");
    }
    
    // Get memory after creating objects
    let after_objects = Introspector::estimate_memory_usage();
    
    // Memory should have increased
    assert!(after_objects.total > baseline.total, 
        "Memory usage should increase after creating objects");
    assert!(after_objects.nodes > baseline.nodes,
        "Node memory should increase");
    
    println!("Memory growth: {} bytes", after_objects.total - baseline.total);
    
    harness.get_results("Memory Introspection Test")
}

/// Test snapshot comparison for debugging
fn test_snapshot_debugging(harness: &mut TestHarness) -> TestResult {
    harness.reset();
    
    // Initial state
    harness.execute(r#"
        (define cam (create-perspective-camera :position [0 5 10]))
        (define cube (create-cube [0 0 0]))
    "#).expect("Failed to create initial state");
    
    let snapshot1 = harness.capture_scene_snapshot("Initial");
    
    // Modify scene
    harness.execute(r#"
        (move-camera cam [5 0 0])
        (scale cube [2 2 2])
        (define sphere (create-sphere [3 0 0]))
    "#).expect("Failed to modify scene");
    
    let snapshot2 = harness.capture_scene_snapshot("Modified");
    
    // Compare snapshots
    assert_ne!(snapshot1.cameras.len(), 0, "Should have cameras in snapshot1");
    assert_eq!(snapshot1.nodes.len() + 1, snapshot2.nodes.len(),
        "Should have added one node");
    
    // Find camera that moved
    let cam_id = snapshot1.cameras.keys().next().unwrap();
    let cam1_pos = &snapshot1.cameras[cam_id].position;
    let cam2_pos = &snapshot2.cameras[cam_id].position;
    
    assert_ne!(cam1_pos.x, cam2_pos.x, "Camera X position should change");
    
    // Timestamps should be different
    assert!(snapshot2.timestamp > snapshot1.timestamp, 
        "Second snapshot should be later");
    
    harness.get_results("Snapshot Debugging Test")
}

/// Test output capture for debugging
fn test_output_capture(harness: &mut TestHarness) -> TestResult {
    harness.reset();
    
    // Generate various outputs
    harness.execute(r#"
        (println "Starting test...")
        (define x 10)
        (println "x =" x)
        (define y (* x 2))
        (println "y =" y)
        (define result (+ x y))
        (println "Result:" result)
        (println "Test complete!")
    "#).expect("Failed to execute");
    
    // Check captured output
    let output = harness.captured_output.lock().unwrap();
    
    assert_eq!(output.len(), 5, "Should have 5 output lines");
    assert!(output[0].contains("Starting"), "First line should be start message");
    assert!(output[1].contains("10"), "Should output x value");
    assert!(output[2].contains("20"), "Should output y value");
    assert!(output[3].contains("30"), "Should output result");
    assert!(output[4].contains("complete"), "Last line should be complete message");
    
    harness.get_results("Output Capture Test")
}

/// Test performance profiling
fn test_performance_profiling(harness: &mut TestHarness) -> TestResult {
    harness.reset();
    
    // Execute code with different complexities
    let simple_code = "(define x 10)";
    let complex_code = r#"
        (define factorial
          (lambda (n)
            (if (<= n 1)
              1
              (* n (factorial (- n 1))))))
        (factorial 5)
    "#;
    
    // Profile simple code
    harness.execute(simple_code).expect("Failed to execute simple");
    let simple_metrics = harness.performance_metrics.clone();
    
    harness.reset();
    
    // Profile complex code
    harness.execute(complex_code).expect("Failed to execute complex");
    let complex_metrics = harness.performance_metrics.clone();
    
    // Complex code should take longer
    assert!(complex_metrics.eval_time >= simple_metrics.eval_time,
        "Complex code should take at least as long as simple code");
    
    println!("Simple eval time: {:?}", simple_metrics.eval_time);
    println!("Complex eval time: {:?}", complex_metrics.eval_time);
    
    harness.get_results("Performance Profiling Test")
}

/// Create the introspection test suite
pub fn create_introspection_test_suite() -> TestSuite {
    let mut suite = TestSuite::new("Introspection & Debugging Tests");
    
    suite.add_test(test_scene_graph_introspection);
    suite.add_test(test_execution_trace);
    suite.add_test(test_error_debugging);
    suite.add_test(test_memory_introspection);
    suite.add_test(test_snapshot_debugging);
    suite.add_test(test_output_capture);
    suite.add_test(test_performance_profiling);
    
    suite
}

#[cfg(test)]
mod tests {
    use super::*;
    
    #[test]
    fn run_introspection_tests() {
        let suite = create_introspection_test_suite();
        let results = suite.run();
        
        for result in &results {
            println!("\nTest: {}", result.name);
            println!("Status: {}", if result.passed { "✅ PASSED" } else { "❌ FAILED" });
            println!("Duration: {:?}", result.duration);
            
            if !result.passed {
                println!("Message: {}", result.message);
                for assertion in &result.assertions {
                    if !assertion.passed {
                        println!("  ❌ {}: expected {}, got {}", 
                            assertion.description, assertion.expected, assertion.actual);
                    }
                }
            }
        }
        
        assert!(results.iter().all(|r| r.passed), "Some tests failed");
    }
}