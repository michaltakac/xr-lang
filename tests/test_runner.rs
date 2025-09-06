//! XR-Lang Test Runner
//! Command-line test runner for all XR-Lang tests

use vm::test_framework::*;
use std::env;

mod camera_tests;
mod hot_reload_tests;
mod introspection_tests;

fn main() {
    let args: Vec<String> = env::args().collect();
    let verbose = args.contains(&"--verbose".to_string()) || args.contains(&"-v".to_string());
    let filter = args.iter()
        .position(|a| a == "--filter" || a == "-f")
        .and_then(|i| args.get(i + 1))
        .cloned();
    
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
        suites_to_run.push(("Camera", camera_tests::create_camera_test_suite()));
    }
    
    if filter.as_ref().map_or(true, |f| f.contains("reload")) {
        suites_to_run.push(("Hot Reload", hot_reload_tests::create_hot_reload_test_suite()));
    }
    
    if filter.as_ref().map_or(true, |f| f.contains("introspect")) {
        suites_to_run.push(("Introspection", introspection_tests::create_introspection_test_suite()));
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