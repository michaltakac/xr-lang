//! Example of using XR-Lang Test Framework
//! This shows how to write and run tests programmatically

use vm::test_framework::*;
use vm::intrinsics::Vec3;
use std::time::Duration;

fn main() {
    println!("XR-Lang Test Framework Example");
    println!("==============================\n");
    
    // Create a test harness
    let mut harness = TestHarness::new();
    harness.enable_debug(); // Enable debug output
    
    // Example 1: Basic execution and assertion
    println!("1. Testing basic camera creation:");
    test_basic_camera(&mut harness);
    
    // Example 2: Testing with snapshots
    println!("\n2. Testing with scene snapshots:");
    test_with_snapshots(&mut harness);
    
    // Example 3: Testing interaction chains
    println!("\n3. Testing interaction chains:");
    test_interaction_chain(&mut harness);
    
    // Example 4: Testing hot reload
    println!("\n4. Testing hot reload:");
    test_hot_reload_example(&mut harness);
    
    // Example 5: Introspection and debugging
    println!("\n5. Testing introspection:");
    test_introspection_example(&mut harness);
}

fn test_basic_camera(harness: &mut TestHarness) {
    harness.reset();
    
    // Execute XRL code
    let result = harness.execute(r#"
        (define cam (create-perspective-camera 
            :position [5 10 15]
            :target [0 0 0]
            :fov 60))
        cam
    "#).expect("Failed to create camera");
    
    println!("Created camera: {:?}", result);
    
    // Make assertions
    if let vm::value::Value::Object(id) = result {
        let exists = harness.assert_object_exists(id, "Camera exists");
        println!("Camera exists: {}", exists);
        
        let pos_correct = harness.assert_position(id, Vec3::new(5.0, 10.0, 15.0), 0.01);
        println!("Position correct: {}", pos_correct);
        
        let fov_correct = harness.assert_camera_fov(id, 60.0, 0.01);
        println!("FOV correct: {}", fov_correct);
    }
    
    // Get test results
    let results = harness.get_results("Basic Camera Test");
    print_test_result(&results);
}

fn test_with_snapshots(harness: &mut TestHarness) {
    harness.reset();
    
    // Create initial scene
    harness.execute(r#"
        (define cam (create-perspective-camera :position [0 5 10]))
        (define cube (create-cube [0 0 0]))
    "#).expect("Failed to create scene");
    
    // Capture snapshot
    let snapshot1 = harness.capture_scene_snapshot("Initial state");
    println!("Snapshot 1: {} nodes, {} cameras", 
        snapshot1.nodes.len(), snapshot1.cameras.len());
    
    // Modify scene
    harness.execute(r#"
        (define sphere (create-sphere [3 0 0]))
        (move-camera cam [2 0 0])
    "#).expect("Failed to modify scene");
    
    // Capture another snapshot
    let snapshot2 = harness.capture_scene_snapshot("After modifications");
    println!("Snapshot 2: {} nodes, {} cameras", 
        snapshot2.nodes.len(), snapshot2.cameras.len());
    
    // Compare snapshots
    println!("Objects added: {}", snapshot2.nodes.len() - snapshot1.nodes.len());
    
    let results = harness.get_results("Snapshot Test");
    print_test_result(&results);
}

fn test_interaction_chain(harness: &mut TestHarness) {
    harness.reset();
    
    let interactions = vec![
        Interaction::Execute("(define cam (create-fps-camera :position [0 1.8 10]))".to_string()),
        Interaction::Snapshot("Camera created".to_string()),
        
        Interaction::Execute("(define cube (create-cube [0 0 0]))".to_string()),
        Interaction::Wait(Duration::from_millis(10)),
        
        Interaction::Execute("(move-camera cam [0 0 -5])".to_string()),
        Interaction::Snapshot("Moved closer".to_string()),
        
        Interaction::Assert(Box::new(|h| {
            let graph = Introspector::get_scene_graph();
            println!("Scene has {} objects", graph.stats.total_objects);
            assert_eq!(graph.stats.camera_count, 1, "Should have 1 camera");
        })),
    ];
    
    harness.chain_interactions(interactions).expect("Failed to chain interactions");
    
    println!("Captured {} snapshots", harness.scene_snapshots.len());
    
    let results = harness.get_results("Interaction Chain Test");
    print_test_result(&results);
}

fn test_hot_reload_example(harness: &mut TestHarness) {
    harness.reset();
    
    // Initial content
    let initial = r#"
        (define cube (create-cube [0 0 0]))
        (scale cube [1 1 1])
    "#;
    
    harness.execute(initial).expect("Failed initial execution");
    let snapshot1 = harness.capture_scene_snapshot("Before reload");
    
    // Modified content
    let modified = r#"
        (define cube (create-cube [0 0 0]))
        (scale cube [2 2 2])
        (define sphere (create-sphere [3 0 0]))
    "#;
    
    // Simulate hot reload
    harness.execute(modified).expect("Failed hot reload");
    let snapshot2 = harness.capture_scene_snapshot("After reload");
    
    println!("Before reload: {} objects", snapshot1.nodes.len());
    println!("After reload: {} objects", snapshot2.nodes.len());
    
    let results = harness.get_results("Hot Reload Example");
    print_test_result(&results);
}

fn test_introspection_example(harness: &mut TestHarness) {
    harness.reset();
    
    // Create a scene
    harness.execute(r#"
        (define cam1 (create-perspective-camera :position [0 5 10]))
        (define cam2 (create-orthographic-camera :position [0 20 0]))
        (define cube (create-cube [0 0 0]))
        (define sphere (create-sphere [3 0 0]))
        (set-active-camera cam1)
    "#).expect("Failed to create scene");
    
    // Introspect the scene
    let scene_graph = Introspector::get_scene_graph();
    
    println!("\nScene Graph:");
    println!("  Cameras: {}", scene_graph.stats.camera_count);
    println!("  Nodes: {}", scene_graph.stats.node_count);
    println!("  Total objects: {}", scene_graph.stats.total_objects);
    
    if let Some(active) = scene_graph.active_camera {
        println!("  Active camera: Object({})", active.0);
    }
    
    // List all objects
    for camera in &scene_graph.cameras {
        println!("  Camera at [{:.1}, {:.1}, {:.1}] FOV: {:.1}°",
            camera.position.x, camera.position.y, camera.position.z, camera.fov);
    }
    
    for node in &scene_graph.nodes {
        println!("  {} at [{:.1}, {:.1}, {:.1}]",
            node.name,
            node.transform.position.x,
            node.transform.position.y,
            node.transform.position.z);
    }
    
    // Trace execution
    let code = "(+ 1 2 3)";
    println!("\nTracing execution of: {}", code);
    let trace = Introspector::trace_execution(code);
    
    println!("  Parse time: {:?}", trace.parse_time);
    println!("  Eval time: {:?}", trace.total_time);
    println!("  AST nodes: {}", trace.ast_nodes);
    
    for step in &trace.evaluations {
        println!("  Step: {} -> {} ({:?})",
            step.expr, step.result, step.duration);
    }
    
    // Estimate memory usage
    let memory = Introspector::estimate_memory_usage();
    println!("\nMemory Usage:");
    println!("  Nodes: {} bytes", memory.nodes);
    println!("  Cameras: {} bytes", memory.cameras);
    println!("  Total: {} bytes", memory.total);
}

fn print_test_result(result: &TestResult) {
    println!("\n  Result: {}", if result.passed { "✅ PASSED" } else { "❌ FAILED" });
    println!("  Message: {}", result.message);
    println!("  Duration: {:?}", result.duration);
    
    if !result.assertions.is_empty() {
        println!("  Assertions:");
        for assertion in &result.assertions {
            println!("    {} {}: {}",
                if assertion.passed { "✅" } else { "❌" },
                assertion.description,
                if assertion.passed {
                    "OK".to_string()
                } else {
                    format!("expected {}, got {}", assertion.expected, assertion.actual)
                });
        }
    }
    
    if !result.captured_output.is_empty() {
        println!("  Output:");
        for line in &result.captured_output {
            println!("    {}", line);
        }
    }
}