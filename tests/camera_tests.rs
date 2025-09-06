//! Camera System Tests
//! Tests all camera functionality in a headless environment

use vm::test_framework::*;
use vm::intrinsics::Vec3;
use vm::value::ObjectId;
use std::time::Duration;

/// Test basic camera creation and properties
fn test_camera_creation(harness: &mut TestHarness) -> TestResult {
    harness.reset();
    
    // Create perspective camera
    let result = harness.execute(r#"
        (define cam1 (create-perspective-camera 
            :position [10 5 15]
            :target [0 0 0]
            :fov 75))
        cam1
    "#).expect("Failed to create camera");
    
    // Verify camera was created
    if let vm::value::Value::Object(id) = result {
        harness.assert_object_exists(id, "Perspective camera exists");
        harness.assert_position(id, Vec3::new(10.0, 5.0, 15.0), 0.01);
        harness.assert_camera_fov(id, 75.0, 0.01);
    } else {
        panic!("Expected object ID, got {:?}", result);
    }
    
    // Test orthographic camera
    harness.execute(r#"
        (define cam2 (create-orthographic-camera
            :position [0 20 0]
            :target [0 0 0]))
    "#).expect("Failed to create orthographic camera");
    
    // Test orbit camera
    harness.execute(r#"
        (define cam3 (orbit-camera
            :target [0 0 0]
            :radius 10
            :theta 45
            :phi 30))
    "#).expect("Failed to create orbit camera");
    
    harness.capture_scene_snapshot("Three cameras created");
    
    // Verify we have 3 cameras
    let scene_graph = Introspector::get_scene_graph();
    assert_eq!(scene_graph.stats.camera_count, 3, "Should have 3 cameras");
    
    harness.get_results("Camera Creation Test")
}

/// Test camera manipulation functions
fn test_camera_manipulation(harness: &mut TestHarness) -> TestResult {
    harness.reset();
    
    // Create and manipulate camera
    let result = harness.execute(r#"
        (define cam (create-perspective-camera 
            :position [0 0 10]
            :target [0 0 0]
            :fov 60))
        cam
    "#).expect("Failed to create camera");
    
    let camera_id = if let vm::value::Value::Object(id) = result {
        id
    } else {
        panic!("Expected object ID");
    };
    
    // Test move-camera
    harness.execute("(move-camera cam [5 0 0])").expect("Failed to move camera");
    harness.assert_position(camera_id, Vec3::new(5.0, 0.0, 10.0), 0.01);
    
    // Test look-at
    harness.execute("(look-at cam [0 5 0])").expect("Failed to look-at");
    
    // Test zoom-camera (changes FOV)
    harness.execute("(zoom-camera cam 2.0)").expect("Failed to zoom");
    harness.assert_camera_fov(camera_id, 30.0, 0.01); // 60 / 2 = 30
    
    // Test set-fov
    harness.execute("(set-fov cam 90)").expect("Failed to set FOV");
    harness.assert_camera_fov(camera_id, 90.0, 0.01);
    
    harness.capture_scene_snapshot("After manipulation");
    
    harness.get_results("Camera Manipulation Test")
}

/// Test camera utilities and queries
fn test_camera_utilities(harness: &mut TestHarness) -> TestResult {
    harness.reset();
    
    harness.execute(r#"
        (define cam (create-perspective-camera 
            :position [3 4 5]
            :target [1 2 3]
            :fov 45))
        
        (define pos (get-camera-position cam))
        (define target (get-camera-target cam))
        (define fov (get-camera-fov cam))
        
        (println "Position:" pos)
        (println "Target:" target)
        (println "FOV:" fov)
    "#).expect("Failed to query camera");
    
    // Check captured output
    let output = harness.captured_output.lock().unwrap();
    assert!(output.iter().any(|s| s.contains("[3 4 5]")), "Should output position");
    assert!(output.iter().any(|s| s.contains("[1 2 3]")), "Should output target");
    assert!(output.iter().any(|s| s.contains("45")), "Should output FOV");
    
    harness.get_results("Camera Utilities Test")
}

/// Test camera switching and cycling
fn test_camera_switching(harness: &mut TestHarness) -> TestResult {
    harness.reset();
    
    // Create multiple cameras
    harness.execute(r#"
        (define cam1 (create-perspective-camera :position [0 0 10]))
        (define cam2 (create-perspective-camera :position [10 0 0]))
        (define cam3 (create-perspective-camera :position [0 10 0]))
    "#).expect("Failed to create cameras");
    
    // Set active camera
    harness.execute("(set-active-camera cam2)").expect("Failed to set active camera");
    
    let snapshot1 = harness.capture_scene_snapshot("cam2 active");
    assert!(snapshot1.active_camera.is_some(), "Should have active camera");
    
    // Cycle cameras
    harness.execute("(cycle-cameras)").expect("Failed to cycle cameras");
    let snapshot2 = harness.capture_scene_snapshot("After cycle");
    
    // Active camera should have changed
    assert_ne!(snapshot1.active_camera, snapshot2.active_camera, 
        "Active camera should change after cycling");
    
    harness.get_results("Camera Switching Test")
}

/// Test interaction chaining for complex scenarios
fn test_interaction_chain(harness: &mut TestHarness) -> TestResult {
    harness.reset();
    
    let interactions = vec![
        Interaction::Execute(r#"
            (define cam (create-fps-camera 
                :position [0 1.8 10]
                :yaw 0
                :pitch 0))
            (define cube (create-cube [0 0 0]))
        "#.to_string()),
        
        Interaction::Snapshot("Initial setup".to_string()),
        
        Interaction::Execute("(move-camera cam [0 0 -5])".to_string()),
        Interaction::Wait(Duration::from_millis(10)),
        
        Interaction::Snapshot("After moving closer".to_string()),
        
        Interaction::Execute("(rotate-camera cam [0 45 0])".to_string()),
        
        Interaction::Assert(Box::new(|h| {
            let sg = Introspector::get_scene_graph();
            assert_eq!(sg.stats.camera_count, 1, "Should have 1 camera");
            assert_eq!(sg.stats.node_count, 1, "Should have 1 node");
        })),
        
        Interaction::Snapshot("Final state".to_string()),
    ];
    
    harness.chain_interactions(interactions).expect("Failed to chain interactions");
    
    // Verify we captured 3 snapshots
    assert_eq!(harness.scene_snapshots.len(), 3, "Should have 3 snapshots");
    
    harness.get_results("Interaction Chain Test")
}

/// Create the camera test suite
pub fn create_camera_test_suite() -> TestSuite {
    let mut suite = TestSuite::new("Camera Tests");
    
    suite.add_test(test_camera_creation);
    suite.add_test(test_camera_manipulation);
    suite.add_test(test_camera_utilities);
    suite.add_test(test_camera_switching);
    suite.add_test(test_interaction_chain);
    
    suite
}

#[cfg(test)]
mod tests {
    use super::*;
    
    #[test]
    fn run_camera_tests() {
        let suite = create_camera_test_suite();
        let results = suite.run();
        
        for result in &results {
            println!("Test: {} - {}", result.name, 
                if result.passed { "PASSED" } else { "FAILED" });
            
            if !result.passed {
                for assertion in &result.assertions {
                    if !assertion.passed {
                        println!("  Failed: {} - expected {}, got {}", 
                            assertion.description, assertion.expected, assertion.actual);
                    }
                }
            }
        }
        
        // All tests should pass
        assert!(results.iter().all(|r| r.passed), "Some tests failed");
    }
}