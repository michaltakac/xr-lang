//! XR-Lang Test Runner
//! Command-line test runner for all XR-Lang tests

use vm::test_framework::*;
use vm::intrinsics::Vec3;
use std::env;
use std::time::Duration;

fn main() {
    let args: Vec<String> = env::args().collect();
    let verbose = args.contains(&"--verbose".to_string()) || args.contains(&"-v".to_string());
    let filter = args.iter()
        .position(|a| a == "--filter" || a == "-f")
        .and_then(|i| args.get(i + 1))
        .cloned();
    
    if args.contains(&"--help".to_string()) || args.contains(&"-h".to_string()) {
        print_usage();
        return;
    }
    
    println!("\n");
    println!("╔════════════════════════════════════════════════════════════╗");
    println!("║           XR-Lang Test Runner (Headless Mode)             ║");
    println!("╚════════════════════════════════════════════════════════════╝");
    println!();
    
    let mut runner = TestRunner::new();
    runner.verbose = verbose;
    
    // Add all test suites
    let mut suites_to_run = vec![];
    
    if filter.as_ref().map_or(true, |f| f.contains("camera")) {
        suites_to_run.push(("Camera", create_camera_test_suite()));
    }
    
    if filter.as_ref().map_or(true, |f| f.contains("reload")) {
        suites_to_run.push(("Hot Reload", create_hot_reload_test_suite()));
    }
    
    if filter.as_ref().map_or(true, |f| f.contains("introspect")) {
        suites_to_run.push(("Introspection", create_introspection_test_suite()));
    }
    
    if suites_to_run.is_empty() {
        println!("No test suites match filter: {:?}", filter);
        return;
    }
    
    // Run all suites
    for (name, suite) in suites_to_run {
        println!("Adding suite: {}", name);
        runner.add_suite(suite);
    }
    
    // Run tests
    let report = runner.run_all();
    
    // Print detailed results if verbose
    if verbose {
        print_detailed_results(&report);
    }
    
    // Print summary
    runner.print_summary(&report);
    
    // Export results if requested
    if args.contains(&"--json".to_string()) {
        export_json_report(&report);
    }
    
    if args.contains(&"--debug-log".to_string()) {
        export_debug_log(&report);
    }
    
    // Exit with appropriate code
    std::process::exit(if report.failed_tests > 0 { 1 } else { 0 });
}

fn print_detailed_results(report: &TestReport) {
    println!("\n╔════════════════════════════════════════════════════════════╗");
    println!("║                    DETAILED RESULTS                       ║");
    println!("╚════════════════════════════════════════════════════════════╝");
    
    for result in &report.results {
        println!("\n┌─ Test: {} ─┐", result.name);
        println!("│ Status: {}", if result.passed { "✅ PASSED" } else { "❌ FAILED" });
        println!("│ Duration: {:?}", result.duration);
        println!("│ Assertions: {}", result.assertions.len());
        
        // Performance metrics
        println!("│ Performance:");
        println!("│   Parse time: {:?}", result.performance_metrics.parse_time);
        println!("│   Eval time: {:?}", result.performance_metrics.eval_time);
        println!("│   Objects: {}", result.performance_metrics.object_count);
        
        // Failed assertions
        if !result.passed {
            println!("│ Failed Assertions:");
            for assertion in &result.assertions {
                if !assertion.passed {
                    println!("│   ❌ {}", assertion.description);
                    println!("│      Expected: {}", assertion.expected);
                    println!("│      Actual: {}", assertion.actual);
                }
            }
        }
        
        // Captured output
        if !result.captured_output.is_empty() {
            println!("│ Output:");
            for line in &result.captured_output {
                println!("│   {}", line);
            }
        }
        
        // Scene snapshots
        if !result.scene_snapshots.is_empty() {
            println!("│ Snapshots:");
            for snapshot in &result.scene_snapshots {
                println!("│   {} - {} nodes, {} cameras", 
                    snapshot.label, 
                    snapshot.nodes.len(), 
                    snapshot.cameras.len());
            }
        }
        
        println!("└────────────────────────────────────────────────────────────┘");
    }
}

fn export_json_report(report: &TestReport) {
    use serde_json::json;
    
    let json_report = json!({
        "total_tests": report.total_tests,
        "passed_tests": report.passed_tests,
        "failed_tests": report.failed_tests,
        "duration_ms": report.total_duration.as_millis(),
        "failed_test_names": report.failed_test_names,
        "tests": report.results.iter().map(|r| {
            json!({
                "name": r.name,
                "passed": r.passed,
                "message": r.message,
                "duration_ms": r.duration.as_millis(),
                "assertions": r.assertions.len(),
                "failed_assertions": r.assertions.iter()
                    .filter(|a| !a.passed)
                    .map(|a| json!({
                        "description": a.description,
                        "expected": a.expected,
                        "actual": a.actual,
                    }))
                    .collect::<Vec<_>>(),
                "performance": {
                    "parse_time_us": r.performance_metrics.parse_time.as_micros(),
                    "eval_time_us": r.performance_metrics.eval_time.as_micros(),
                    "object_count": r.performance_metrics.object_count,
                }
            })
        }).collect::<Vec<_>>(),
    });
    
    let json_string = serde_json::to_string_pretty(&json_report).unwrap();
    std::fs::write("test_results.json", json_string).unwrap();
    println!("\n📊 Test results exported to test_results.json");
}

fn export_debug_log(report: &TestReport) {
    use std::fs::File;
    use std::io::Write;
    
    let mut file = File::create("test_debug.log").unwrap();
    
    writeln!(file, "XR-Lang Test Debug Log").unwrap();
    writeln!(file, "======================").unwrap();
    writeln!(file, "Generated: {:?}", std::time::SystemTime::now()).unwrap();
    writeln!(file).unwrap();
    
    for result in &report.results {
        writeln!(file, "\n### Test: {}", result.name).unwrap();
        writeln!(file, "Status: {}", if result.passed { "PASSED" } else { "FAILED" }).unwrap();
        writeln!(file, "Duration: {:?}", result.duration).unwrap();
        
        // All assertions
        writeln!(file, "\nAssertions:").unwrap();
        for (i, assertion) in result.assertions.iter().enumerate() {
            writeln!(file, "  {}. {} [{}]", 
                i + 1,
                assertion.description,
                if assertion.passed { "PASS" } else { "FAIL" }
            ).unwrap();
            
            if !assertion.passed {
                writeln!(file, "     Expected: {}", assertion.expected).unwrap();
                writeln!(file, "     Actual: {}", assertion.actual).unwrap();
                writeln!(file, "     Location: {}", assertion.location).unwrap();
            }
        }
        
        // Captured output
        if !result.captured_output.is_empty() {
            writeln!(file, "\nCaptured Output:").unwrap();
            for line in &result.captured_output {
                writeln!(file, "  > {}", line).unwrap();
            }
        }
        
        // Scene snapshots with full details
        if !result.scene_snapshots.is_empty() {
            writeln!(file, "\nScene Snapshots:").unwrap();
            for snapshot in &result.scene_snapshots {
                writeln!(file, "  Snapshot: {}", snapshot.label).unwrap();
                writeln!(file, "    Timestamp: {:?}", snapshot.timestamp).unwrap();
                writeln!(file, "    Nodes: {}", snapshot.nodes.len()).unwrap();
                writeln!(file, "    Cameras: {}", snapshot.cameras.len()).unwrap();
                
                if let Some(active) = snapshot.active_camera {
                    writeln!(file, "    Active Camera: Object({})", active.0).unwrap();
                }
                
                // List all objects
                for (id, node) in &snapshot.nodes {
                    writeln!(file, "      Node {}: {} at [{:.2}, {:.2}, {:.2}]",
                        id.0, node.name,
                        node.transform.position.x,
                        node.transform.position.y,
                        node.transform.position.z
                    ).unwrap();
                }
                
                for (id, camera) in &snapshot.cameras {
                    writeln!(file, "      Camera {}: pos [{:.2}, {:.2}, {:.2}], FOV {:.1}°",
                        id.0,
                        camera.position.x,
                        camera.position.y,
                        camera.position.z,
                        camera.fov
                    ).unwrap();
                }
            }
        }
        
        // Performance metrics
        writeln!(file, "\nPerformance Metrics:").unwrap();
        writeln!(file, "  Parse Time: {:?}", result.performance_metrics.parse_time).unwrap();
        writeln!(file, "  Eval Time: {:?}", result.performance_metrics.eval_time).unwrap();
        writeln!(file, "  Total Objects: {}", result.performance_metrics.object_count).unwrap();
        writeln!(file, "  Memory Estimate: {} bytes", result.performance_metrics.memory_usage).unwrap();
        
        writeln!(file, "\n{}", "─".repeat(60)).unwrap();
    }
    
    println!("📝 Debug log exported to test_debug.log");
}

/// Usage information
fn print_usage() {
    println!("Usage: test_runner [OPTIONS]");
    println!();
    println!("Options:");
    println!("  --verbose, -v       Show detailed test output");
    println!("  --filter, -f TEXT   Filter test suites by name");
    println!("  --json              Export results as JSON");
    println!("  --debug-log         Export detailed debug log");
    println!("  --help, -h          Show this help message");
    println!();
    println!("Examples:");
    println!("  test_runner                    # Run all tests");
    println!("  test_runner -v                 # Run with verbose output");
    println!("  test_runner -f camera          # Run only camera tests");
    println!("  test_runner --json             # Export JSON report");
    println!("  test_runner -v --debug-log     # Verbose with debug log");
}

// ============================================================================
// TEST SUITES
// ============================================================================

/// Create camera test suite
fn create_camera_test_suite() -> TestSuite {
    let mut suite = TestSuite::new("Camera Tests");
    
    suite.add_test(test_camera_creation);
    suite.add_test(test_camera_manipulation);
    suite.add_test(test_camera_utilities);
    suite.add_test(test_camera_switching);
    
    suite
}

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
    }
    
    // Test other camera types
    harness.execute(r#"
        (define cam2 (create-orthographic-camera
            :position [0 20 0]
            :target [0 0 0]))
        (define cam3 (orbit-camera
            :target [0 0 0]
            :radius 10
            :theta 45
            :phi 30))
    "#).expect("Failed to create other cameras");
    
    harness.capture_scene_snapshot("Three cameras created");
    
    harness.get_results("Camera Creation Test")
}

fn test_camera_manipulation(harness: &mut TestHarness) -> TestResult {
    harness.reset();
    
    let result = harness.execute(r#"
        (define cam (create-perspective-camera 
            :position [0 0 10]
            :target [0 0 0]
            :fov 60))
        cam
    "#).expect("Failed to create camera");
    
    if let vm::value::Value::Object(camera_id) = result {
        // Test move-camera
        harness.execute("(move-camera cam [5 0 0])").expect("Failed to move camera");
        harness.assert_position(camera_id, Vec3::new(5.0, 0.0, 10.0), 0.01);
        
        // Test zoom-camera
        harness.execute("(zoom-camera cam 2.0)").expect("Failed to zoom");
        harness.assert_camera_fov(camera_id, 30.0, 0.01);
    }
    
    harness.get_results("Camera Manipulation Test")
}

fn test_camera_utilities(harness: &mut TestHarness) -> TestResult {
    harness.reset();
    
    let result = harness.execute(r#"
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
        
        ; Return values for verification
        [pos target fov cam]
    "#).expect("Failed to query camera");
    
    // Verify returned values
    if let vm::value::Value::Vector(values) = result {
        assert_eq!(values.len(), 4, "Should return 4 values");
        
        // Check position
        if let vm::value::Value::Vector(pos) = &values[0] {
            assert_eq!(pos.len(), 3, "Position should have 3 components");
            // Position should be [3, 4, 5]
            if let vm::value::Value::Int(x) = pos[0] { assert_eq!(x, 3); }
            if let vm::value::Value::Int(y) = pos[1] { assert_eq!(y, 4); }
            if let vm::value::Value::Int(z) = pos[2] { assert_eq!(z, 5); }
        } else {
            panic!("Position should be a vector");
        }
        
        // Check target
        if let vm::value::Value::Vector(target) = &values[1] {
            assert_eq!(target.len(), 3, "Target should have 3 components");
            // Target should be [1, 2, 3]
            if let vm::value::Value::Int(x) = target[0] { assert_eq!(x, 1); }
            if let vm::value::Value::Int(y) = target[1] { assert_eq!(y, 2); }
            if let vm::value::Value::Int(z) = target[2] { assert_eq!(z, 3); }
        } else {
            panic!("Target should be a vector");
        }
        
        // Check FOV (can be Float or Int)
        match &values[2] {
            vm::value::Value::Int(fov) => assert_eq!(*fov, 45, "FOV should be 45"),
            vm::value::Value::Float(fov) => assert!((fov - 45.0).abs() < 0.01, "FOV should be 45.0"),
            _ => panic!("FOV should be a number")
        }
        
        // Check camera ID exists
        if let vm::value::Value::Object(id) = &values[3] {
            harness.assert_object_exists(*id, "Camera should exist");
            harness.assert_camera_fov(*id, 45.0, 0.01);
        } else {
            panic!("Camera should be an object");
        }
    } else {
        panic!("Should return a vector of values");
    }
    
    // Check captured output
    {
        let output = harness.captured_output.lock().unwrap();
        assert!(!output.is_empty(), "Should have captured output");
        assert_eq!(output.len(), 3, "Should have 3 output lines");
        assert!(output[0].contains("Position:"), "First line should contain Position");
        assert!(output[1].contains("Target:"), "Second line should contain Target");
        assert!(output[2].contains("FOV:"), "Third line should contain FOV");
    }
    
    harness.get_results("Camera Utilities Test")
}

fn test_camera_switching(harness: &mut TestHarness) -> TestResult {
    harness.reset();
    
    let result = harness.execute(r#"
        (define cam1 (create-perspective-camera :position [0 0 10]))
        (define cam2 (create-perspective-camera :position [10 0 0]))
        (define cam3 (create-perspective-camera :position [0 10 0]))
        (set-active-camera cam2)
        [cam1 cam2 cam3]
    "#).expect("Failed to create and switch cameras");
    
    // Verify all cameras were created
    if let vm::value::Value::Vector(cams) = result {
        assert_eq!(cams.len(), 3, "Should have created 3 cameras");
        
        // Verify each is a valid camera object
        for (i, cam) in cams.iter().enumerate() {
            if let vm::value::Value::Object(id) = cam {
                harness.assert_object_exists(*id, &format!("Camera {} exists", i + 1));
                
                // Check positions match what we set
                match i {
                    0 => harness.assert_position(*id, Vec3::new(0.0, 0.0, 10.0), 0.01),
                    1 => harness.assert_position(*id, Vec3::new(10.0, 0.0, 0.0), 0.01),
                    2 => harness.assert_position(*id, Vec3::new(0.0, 10.0, 0.0), 0.01),
                    _ => false
                };
            } else {
                panic!("Camera {} should be an object", i + 1);
            }
        }
        
        // Verify cam2 is the active camera
        if let vm::value::Value::Object(cam2_id) = &cams[1] {
            let snapshot = harness.capture_scene_snapshot("After switching");
            assert!(snapshot.active_camera.is_some(), "Should have active camera");
            assert_eq!(snapshot.active_camera.unwrap(), *cam2_id, "Active camera should be cam2");
        }
    } else {
        panic!("Should return vector of cameras");
    }
    
    // Test cycle-cameras function
    harness.execute("(cycle-cameras)").expect("Failed to cycle cameras");
    let snapshot2 = harness.capture_scene_snapshot("After cycling");
    assert!(snapshot2.active_camera.is_some(), "Should still have active camera after cycling");
    
    harness.get_results("Camera Switching Test")
}

/// Create hot reload test suite
fn create_hot_reload_test_suite() -> TestSuite {
    let mut suite = TestSuite::new("Hot Reload Tests");
    
    suite.add_test(test_basic_hot_reload);
    suite.add_test(test_state_preservation);
    
    suite
}

fn test_basic_hot_reload(harness: &mut TestHarness) -> TestResult {
    harness.reset();
    
    // Initial content
    let result1 = harness.execute(r#"
        (define cube1 (create-cube [0 0 0]))
        (scale cube1 [1 1 1])
        cube1
    "#).expect("Failed to execute initial");
    
    // Verify initial cube
    if let vm::value::Value::Object(cube_id) = result1 {
        harness.assert_object_exists(cube_id, "Initial cube exists");
        harness.assert_scale(cube_id, Vec3::new(1.0, 1.0, 1.0), 0.01);
        harness.assert_position(cube_id, Vec3::new(0.0, 0.0, 0.0), 0.01);
    } else {
        panic!("Initial cube should be an object");
    }
    
    let snapshot1 = harness.capture_scene_snapshot("Initial state");
    assert_eq!(snapshot1.nodes.len(), 1, "Should have 1 node initially");
    
    // Modified content (simulating hot reload)
    let result2 = harness.execute(r#"
        (define cube1 (create-cube [0 0 0]))
        (scale cube1 [2 2 2])
        (define sphere (create-sphere [3 0 0]))
        [cube1 sphere]
    "#).expect("Failed to reload");
    
    // Verify objects after reload
    if let vm::value::Value::Vector(objects) = result2 {
        assert_eq!(objects.len(), 2, "Should have 2 objects after reload");
        
        // Check cube with new scale
        if let vm::value::Value::Object(cube_id) = &objects[0] {
            harness.assert_object_exists(*cube_id, "Reloaded cube exists");
            harness.assert_scale(*cube_id, Vec3::new(2.0, 2.0, 2.0), 0.01);
            harness.assert_position(*cube_id, Vec3::new(0.0, 0.0, 0.0), 0.01);
        } else {
            panic!("First object should be cube");
        }
        
        // Check sphere
        if let vm::value::Value::Object(sphere_id) = &objects[1] {
            harness.assert_object_exists(*sphere_id, "Sphere exists");
            harness.assert_position(*sphere_id, Vec3::new(3.0, 0.0, 0.0), 0.01);
        } else {
            panic!("Second object should be sphere");
        }
    } else {
        panic!("Should return vector of objects");
    }
    
    let snapshot2 = harness.capture_scene_snapshot("After reload");
    assert_eq!(snapshot2.nodes.len(), 3, "Should have 3 nodes after reload (1 original + 2 new)");
    
    // Verify the scene changed
    assert!(snapshot2.nodes.len() > snapshot1.nodes.len(), 
        "Should have added objects (2 > 1)");
    
    harness.get_results("Basic Hot Reload Test")
}

fn test_state_preservation(harness: &mut TestHarness) -> TestResult {
    harness.reset();
    
    let result1 = harness.execute(r#"
        (define preserved-value 42)
        (define camera (create-perspective-camera 
            :position [0 5 10]
            :target [0 0 0]
            :fov 60))
        (set-active-camera camera)
        (define cube (create-cube [0 0 0]))
        [preserved-value camera cube]
    "#).expect("Failed to setup scene");
    
    // Verify initial setup
    if let vm::value::Value::Vector(initial) = result1 {
        assert_eq!(initial.len(), 3, "Should have 3 initial values");
        
        // Check preserved value
        if let vm::value::Value::Int(val) = initial[0] {
            assert_eq!(val, 42, "Preserved value should be 42");
        }
        
        // Verify camera and cube objects
        if let vm::value::Value::Object(cam_id) = &initial[1] {
            harness.assert_object_exists(*cam_id, "Initial camera exists");
            harness.assert_position(*cam_id, Vec3::new(0.0, 5.0, 10.0), 0.01);
            harness.assert_camera_fov(*cam_id, 60.0, 0.01);
        }
        if let vm::value::Value::Object(cube_id) = &initial[2] {
            harness.assert_object_exists(*cube_id, "Initial cube exists");
            harness.assert_position(*cube_id, Vec3::new(0.0, 0.0, 0.0), 0.01);
        }
    }
    
    // Simulate runtime camera movement
    let result2 = harness.execute(r#"
        (move-camera camera [2 0 0])
        (get-camera-position camera)
    "#).expect("Failed to move camera");
    
    // Verify camera moved
    if let vm::value::Value::Vector(new_pos) = result2 {
        assert_eq!(new_pos.len(), 3, "Position should have 3 components");
        // Camera should be at [2, 5, 10] after moving [2, 0, 0]
        if let vm::value::Value::Float(x) = new_pos[0] {
            assert!((x - 2.0).abs() < 0.01, "X should be 2.0 after move");
        }
    }
    
    let snapshot_before = harness.capture_scene_snapshot("Before reload");
    assert_eq!(snapshot_before.nodes.len(), 1, "Should have 1 node before reload");
    assert_eq!(snapshot_before.cameras.len(), 1, "Should have 1 camera before reload");
    
    // Simulate hot reload with new objects
    let result3 = harness.execute(r#"
        ; preserved-value should still be accessible
        (define camera (create-perspective-camera 
            :position [0 5 10]
            :target [0 0 0]
            :fov 60))
        (set-active-camera camera)
        (define cube (create-cube [0 0 0]))
        (define sphere (create-sphere [3 0 0]))
        [preserved-value sphere]
    "#).expect("Failed to reload");
    
    // Verify state after reload
    if let vm::value::Value::Vector(reloaded) = result3 {
        assert_eq!(reloaded.len(), 2, "Should return 2 values");
        
        // Check preserved value is still 42
        if let vm::value::Value::Int(val) = reloaded[0] {
            assert_eq!(val, 42, "Preserved value should still be 42 after reload");
        }
        
        // Check new sphere exists
        if let vm::value::Value::Object(sphere_id) = &reloaded[1] {
            harness.assert_object_exists(*sphere_id, "Sphere exists after reload");
            harness.assert_position(*sphere_id, Vec3::new(3.0, 0.0, 0.0), 0.01);
        }
    }
    
    let snapshot_after = harness.capture_scene_snapshot("After reload");
    assert_eq!(snapshot_after.nodes.len(), 3, "Should have 3 nodes after reload");
    assert_eq!(snapshot_after.cameras.len(), 2, "Should have 2 cameras after reload");
    assert!(snapshot_after.nodes.len() > snapshot_before.nodes.len(),
        "Should have added new objects (3 > 1)");
    
    harness.get_results("State Preservation Test")
}

/// Create introspection test suite
fn create_introspection_test_suite() -> TestSuite {
    let mut suite = TestSuite::new("Introspection Tests");
    
    suite.add_test(test_scene_graph_introspection);
    suite.add_test(test_execution_trace);
    
    suite
}

fn test_scene_graph_introspection(harness: &mut TestHarness) -> TestResult {
    harness.reset();
    
    let result = harness.execute(r#"
        (define cam1 (create-perspective-camera :position [0 5 10]))
        (define cam2 (create-orthographic-camera :position [0 20 0]))
        (define cube1 (create-cube [0 0 0]))
        (define cube2 (create-cube [3 0 0]))
        (define sphere1 (create-sphere [0 3 0]))
        (set-active-camera cam1)
        [cam1 cam2 cube1 cube2 sphere1]
    "#).expect("Failed to build scene");
    
    // Verify all objects were created
    if let vm::value::Value::Vector(objects) = result {
        assert_eq!(objects.len(), 5, "Should have created 5 objects");
        
        // Verify cameras
        if let vm::value::Value::Object(cam1_id) = &objects[0] {
            harness.assert_object_exists(*cam1_id, "Camera 1 exists");
            harness.assert_position(*cam1_id, Vec3::new(0.0, 5.0, 10.0), 0.01);
        }
        if let vm::value::Value::Object(cam2_id) = &objects[1] {
            harness.assert_object_exists(*cam2_id, "Camera 2 exists");
            harness.assert_position(*cam2_id, Vec3::new(0.0, 20.0, 0.0), 0.01);
        }
        
        // Verify scene objects
        if let vm::value::Value::Object(cube1_id) = &objects[2] {
            harness.assert_object_exists(*cube1_id, "Cube 1 exists");
            harness.assert_position(*cube1_id, Vec3::new(0.0, 0.0, 0.0), 0.01);
        }
        if let vm::value::Value::Object(cube2_id) = &objects[3] {
            harness.assert_object_exists(*cube2_id, "Cube 2 exists");
            harness.assert_position(*cube2_id, Vec3::new(3.0, 0.0, 0.0), 0.01);
        }
        if let vm::value::Value::Object(sphere_id) = &objects[4] {
            harness.assert_object_exists(*sphere_id, "Sphere exists");
            harness.assert_position(*sphere_id, Vec3::new(0.0, 3.0, 0.0), 0.01);
        }
        
        // Verify active camera is cam1
        if let vm::value::Value::Object(cam1_id) = &objects[0] {
            let snapshot = harness.capture_scene_snapshot("Scene snapshot");
            assert!(snapshot.active_camera.is_some(), "Should have active camera");
            assert_eq!(snapshot.active_camera.unwrap(), *cam1_id, "Active camera should be cam1");
        }
    }
    
    let scene_graph = Introspector::get_scene_graph();
    
    assert_eq!(scene_graph.stats.camera_count, 2, "Should have 2 cameras in scene graph");
    assert_eq!(scene_graph.stats.node_count, 3, "Should have 3 nodes in scene graph");
    assert_eq!(scene_graph.stats.camera_count + scene_graph.stats.node_count, 5, "Should have 5 total objects");
    assert!(scene_graph.active_camera.is_some(), "Should have active camera in scene graph");
    
    // Test scene graph hierarchy
    assert!(!scene_graph.nodes.is_empty(), "Scene graph should have nodes");
    assert!(!scene_graph.cameras.is_empty(), "Scene graph should have cameras");
    
    // Verify node details in scene graph
    for node in &scene_graph.nodes {
        assert!(!node.name.is_empty(), "Node should have a name");
    }
    
    harness.get_results("Scene Graph Introspection Test")
}

fn test_execution_trace(harness: &mut TestHarness) -> TestResult {
    harness.reset();
    
    // Test simple arithmetic execution
    let code = r#"
        (define x 10)
        (define y 20)
        (define sum (+ x y))
        sum
    "#;
    
    let trace = Introspector::trace_execution(code);
    
    assert!(trace.ast_nodes > 0, "Should have parsed AST nodes");
    assert_eq!(trace.ast_nodes, 4, "Should have 4 AST nodes (3 defines + 1 sum)");
    assert!(!trace.evaluations.is_empty(), "Should have evaluation steps");
    assert!(trace.evaluations.len() >= 4, "Should have at least 4 evaluation steps");
    assert!(trace.errors.is_empty(), "Should have no errors");
    assert!(trace.total_time > Duration::from_secs(0), "Should have execution time");
    
    // Verify evaluation steps exist (specific content may vary)
    for eval_step in &trace.evaluations {
        assert!(!eval_step.expr.is_empty(), "Evaluation step should have expression");
    }
    
    // Test execution with error
    let error_code = r#"
        (define x 10)
        (undefined-function x)
    "#;
    
    let error_trace = Introspector::trace_execution(error_code);
    assert!(!error_trace.errors.is_empty(), "Should have errors for undefined function");
    
    // Test more complex execution
    let complex_code = r#"
        (define factorial
          (lambda (n)
            (if (<= n 1)
                1
                (* n (factorial (- n 1))))))
        (factorial 5)
    "#;
    
    let complex_trace = Introspector::trace_execution(complex_code);
    assert!(complex_trace.ast_nodes >= 2, "Should have at least 2 AST nodes for complex code");
    assert!(!complex_trace.evaluations.is_empty(), "Complex code should have evaluation steps");
    
    // Verify memory estimation
    let memory = Introspector::estimate_memory_usage();
    assert!(memory.total > 0, "Should have memory usage");
    assert!(memory.nodes >= 0, "Should have node memory");
    assert!(memory.cameras >= 0, "Should have camera memory");
    // Total memory includes overhead beyond just nodes and cameras
    assert!(memory.total >= memory.nodes + memory.cameras, "Total should be at least nodes + cameras");
    
    harness.get_results("Execution Trace Test")
}