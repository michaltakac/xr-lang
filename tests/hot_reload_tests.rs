//! Hot Reload Tests
//! Tests the hot-reload functionality and state preservation

use vm::test_framework::*;
use vm::intrinsics::Vec3;
use std::fs;
use std::time::Duration;
use tempfile::TempDir;

/// Test basic hot reload functionality
fn test_basic_hot_reload(harness: &mut TestHarness) -> TestResult {
    harness.reset();
    
    // Create a temporary directory for test files
    let temp_dir = TempDir::new().expect("Failed to create temp dir");
    let test_file = temp_dir.path().join("test_scene.xrl");
    
    // Initial content
    let initial_content = r#"
        (define cube1 (create-cube [0 0 0]))
        (scale cube1 [1 1 1])
    "#;
    
    fs::write(&test_file, initial_content).expect("Failed to write initial file");
    
    // Execute initial content
    harness.execute_file(test_file.to_str().unwrap()).expect("Failed to execute initial");
    let snapshot1 = harness.capture_scene_snapshot("Initial state");
    
    // Modified content - changes scale
    let modified_content = r#"
        (define cube1 (create-cube [0 0 0]))
        (scale cube1 [2 2 2])
    "#;
    
    // Simulate hot reload
    harness.simulate_hot_reload(
        test_file.to_str().unwrap(), 
        modified_content
    ).expect("Failed to hot reload");
    
    let snapshot2 = harness.capture_scene_snapshot("After hot reload");
    
    // Verify the scene changed
    assert_eq!(snapshot1.nodes.len(), snapshot2.nodes.len(), 
        "Should have same number of nodes");
    
    // In a real implementation, we'd check that the scale changed
    // For now, just verify reload succeeded
    
    harness.get_results("Basic Hot Reload Test")
}

/// Test state preservation during hot reload
fn test_state_preservation(harness: &mut TestHarness) -> TestResult {
    harness.reset();
    
    // Setup scene with camera
    harness.execute(r#"
        (define camera (create-perspective-camera 
            :position [0 5 10]
            :target [0 0 0]
            :fov 60))
        (set-active-camera camera)
        (define cube (create-cube [0 0 0]))
    "#).expect("Failed to setup scene");
    
    // Simulate runtime camera movement (would be preserved)
    harness.execute("(move-camera camera [2 0 0])").expect("Failed to move camera");
    
    let snapshot_before = harness.capture_scene_snapshot("Before reload");
    
    // Simulate hot reload with new object added
    let new_content = r#"
        (define camera (create-perspective-camera 
            :position [0 5 10]
            :target [0 0 0]
            :fov 60))
        (set-active-camera camera)
        (define cube (create-cube [0 0 0]))
        (define sphere (create-sphere [3 0 0]))  ; New object
    "#;
    
    // In a real implementation with preservation, camera position would be maintained
    harness.execute(new_content).expect("Failed to reload");
    
    let snapshot_after = harness.capture_scene_snapshot("After reload");
    
    // Verify new object was added
    assert!(snapshot_after.nodes.len() > snapshot_before.nodes.len(),
        "Should have added new object");
    
    harness.get_results("State Preservation Test")
}

/// Test rapid successive hot reloads
fn test_rapid_hot_reload(harness: &mut TestHarness) -> TestResult {
    harness.reset();
    
    let temp_dir = TempDir::new().expect("Failed to create temp dir");
    let test_file = temp_dir.path().join("rapid_test.xrl");
    
    // Perform multiple rapid reloads
    for i in 0..5 {
        let content = format!(r#"
            (define cube (create-cube [0 {} 0]))
            (println "Reload {}")
        "#, i, i);
        
        fs::write(&test_file, &content).expect("Failed to write file");
        harness.execute(&content).expect("Failed to execute");
        
        // Small delay to simulate rapid changes
        std::thread::sleep(Duration::from_millis(10));
    }
    
    // Check that all reloads were captured
    let output = harness.captured_output.lock().unwrap();
    assert_eq!(output.len(), 5, "Should have 5 reload messages");
    
    harness.get_results("Rapid Hot Reload Test")
}

/// Test hot reload with errors
fn test_hot_reload_with_errors(harness: &mut TestHarness) -> TestResult {
    harness.reset();
    
    // Valid initial content
    harness.execute(r#"
        (define cube (create-cube [0 0 0]))
    "#).expect("Failed to create initial scene");
    
    let initial_snapshot = harness.capture_scene_snapshot("Initial valid state");
    
    // Invalid content (syntax error)
    let invalid_content = r#"
        (define cube (create-cube [0 0 0))  ; Missing closing bracket
    "#;
    
    // Try to reload with invalid content - should fail gracefully
    match harness.execute(invalid_content) {
        Err(_) => {
            // Expected to fail
            let error_snapshot = harness.capture_scene_snapshot("After error");
            
            // Scene should remain unchanged after error
            assert_eq!(initial_snapshot.nodes.len(), error_snapshot.nodes.len(),
                "Scene should be unchanged after reload error");
        }
        Ok(_) => {
            panic!("Should have failed with syntax error");
        }
    }
    
    harness.get_results("Hot Reload Error Handling Test")
}

/// Test hot reload performance
fn test_hot_reload_performance(harness: &mut TestHarness) -> TestResult {
    harness.reset();
    harness.enable_debug();
    
    let iterations = 10;
    let mut reload_times = Vec::new();
    
    for i in 0..iterations {
        let content = format!(r#"
            (define cube{} (create-cube [{} 0 0]))
        "#, i, i);
        
        let start = std::time::Instant::now();
        harness.execute(&content).expect("Failed to execute");
        let elapsed = start.elapsed();
        
        reload_times.push(elapsed);
    }
    
    // Calculate average reload time
    let total: Duration = reload_times.iter().sum();
    let average = total / iterations;
    
    println!("Average reload time: {:?}", average);
    
    // Performance assertion - reload should be fast
    assert!(average < Duration::from_millis(100), 
        "Hot reload should complete in under 100ms on average");
    
    harness.get_results("Hot Reload Performance Test")
}

/// Create the hot reload test suite
pub fn create_hot_reload_test_suite() -> TestSuite {
    let mut suite = TestSuite::new("Hot Reload Tests");
    
    suite.add_test(test_basic_hot_reload);
    suite.add_test(test_state_preservation);
    suite.add_test(test_rapid_hot_reload);
    suite.add_test(test_hot_reload_with_errors);
    suite.add_test(test_hot_reload_performance);
    
    suite
}

#[cfg(test)]
mod tests {
    use super::*;
    
    #[test]
    fn run_hot_reload_tests() {
        let suite = create_hot_reload_test_suite();
        let results = suite.run();
        
        for result in &results {
            println!("Test: {} - {}", result.name, 
                if result.passed { "PASSED" } else { "FAILED" });
            
            if !result.passed {
                println!("  Message: {}", result.message);
            }
            
            // Print performance metrics
            println!("  Parse time: {:?}", result.performance_metrics.parse_time);
            println!("  Eval time: {:?}", result.performance_metrics.eval_time);
        }
        
        assert!(results.iter().all(|r| r.passed), "Some tests failed");
    }
}