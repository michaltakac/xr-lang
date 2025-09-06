#!/usr/bin/env rust-script
//! Simple test runner for XR-Lang Test Framework
//! 
//! Run with: rustc run_tests.rs -L target/debug/deps --extern vm=target/debug/libvm.rlib && ./run_tests

use vm::test_framework::*;
use vm::intrinsics::Vec3;

fn main() {
    println!("\n=== XR-Lang Test Framework Demo ===\n");
    
    // Run a simple test
    let mut harness = TestHarness::new();
    
    println!("Testing camera creation...");
    test_camera_basics(&mut harness);
    
    println!("\nTesting scene introspection...");
    test_introspection(&mut harness);
    
    println!("\nTesting hot-reload simulation...");
    test_hot_reload(&mut harness);
    
    println!("\n✅ All manual tests completed!");
}

fn test_camera_basics(harness: &mut TestHarness) {
    harness.reset();
    
    // Test camera creation
    let result = harness.execute(r#"
        (define cam1 (create-perspective-camera 
            :position [10 5 15]
            :target [0 0 0]
            :fov 60))
        cam1
    "#).expect("Failed to create camera");
    
    if let vm::value::Value::Object(id) = result {
        println!("  Created camera with ID: {}", id.0);
        
        // Test assertions
        let exists = harness.assert_object_exists(id, "Camera exists");
        let pos_ok = harness.assert_position(id, Vec3::new(10.0, 5.0, 15.0), 0.01);
        let fov_ok = harness.assert_camera_fov(id, 60.0, 0.01);
        
        println!("  Assertions: exists={}, position={}, fov={}", exists, pos_ok, fov_ok);
        
        // Test camera manipulation
        harness.execute("(move-camera cam1 [0 2 0])").expect("Failed to move camera");
        let new_pos_ok = harness.assert_position(id, Vec3::new(10.0, 7.0, 15.0), 0.01);
        println!("  After move: position_correct={}", new_pos_ok);
        
        // Test utilities
        harness.execute(r#"
            (define pos (get-camera-position cam1))
            (define fov (get-camera-fov cam1))
            (println "Camera position:" pos "FOV:" fov)
        "#).expect("Failed to query camera");
        
        // Check captured output
        let output = harness.captured_output.lock().unwrap();
        if !output.is_empty() {
            println!("  Captured output: {}", output.last().unwrap());
        }
    }
    
    let results = harness.get_results("Camera Basics");
    println!("  Test result: {} ({})", 
        if results.passed { "PASSED" } else { "FAILED" },
        results.message);
}

fn test_introspection(harness: &mut TestHarness) {
    harness.reset();
    
    // Create a scene
    harness.execute(r#"
        (define cam1 (create-perspective-camera :position [0 5 10]))
        (define cam2 (create-orthographic-camera :position [0 20 0]))
        (define cube1 (create-cube [0 0 0]))
        (define cube2 (create-cube [3 0 0]))
        (define sphere (create-sphere [0 3 0]))
        (set-active-camera cam1)
    "#).expect("Failed to create scene");
    
    // Introspect
    let scene_graph = Introspector::get_scene_graph();
    println!("  Scene contains:");
    println!("    - {} cameras", scene_graph.stats.camera_count);
    println!("    - {} nodes", scene_graph.stats.node_count);
    println!("    - {} total objects", scene_graph.stats.total_objects);
    
    if let Some(active) = scene_graph.active_camera {
        println!("    - Active camera: Object({})", active.0);
    }
    
    // Test snapshots
    let snapshot = harness.capture_scene_snapshot("Test scene");
    println!("  Snapshot captured: {} at {:?}", snapshot.label, snapshot.timestamp);
    
    // Test execution trace
    let trace = Introspector::trace_execution("(+ 1 2 3)");
    println!("  Execution trace: {} steps in {:?}", 
        trace.evaluations.len(), trace.total_time);
    
    // Test memory estimation
    let memory = Introspector::estimate_memory_usage();
    println!("  Memory usage: {} bytes", memory.total);
}

fn test_hot_reload(harness: &mut TestHarness) {
    harness.reset();
    
    // Initial scene
    harness.execute(r#"
        (define cube (create-cube [0 0 0]))
        (scale cube [1 1 1])
    "#).expect("Failed to create initial scene");
    
    let snapshot1 = harness.capture_scene_snapshot("Before reload");
    println!("  Initial: {} objects", snapshot1.nodes.len());
    
    // Simulate hot reload with changes
    harness.execute(r#"
        (define cube (create-cube [0 0 0]))
        (scale cube [2 2 2])
        (define sphere (create-sphere [3 0 0]))
    "#).expect("Failed to reload");
    
    let snapshot2 = harness.capture_scene_snapshot("After reload");
    println!("  After reload: {} objects", snapshot2.nodes.len());
    
    let objects_added = snapshot2.nodes.len() - snapshot1.nodes.len();
    println!("  Objects added: {}", objects_added);
    
    if objects_added > 0 {
        println!("  ✅ Hot reload simulation successful!");
    }
}