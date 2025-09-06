//! XR-Lang Test Framework
//! 
//! A headless testing framework for XR-Lang that provides:
//! - Programmatic testing without UI
//! - Hot-reload validation
//! - Scene state introspection
//! - Interaction chaining
//! - Debug output capture
//! - Performance profiling

use crate::value::{Value, Symbol, Environment, ObjectId};
use crate::evaluator::Evaluator;
use crate::parser::Parser;
use crate::intrinsics::{SceneState, SCENE, Vec3, Camera, Transform, SceneNode};
use crate::hotreload::HotReloadManager;
use std::rc::Rc;
use std::collections::HashMap;
use std::time::{Instant, Duration};
use std::fs;
use std::path::Path;
use std::sync::Arc;
use std::sync::Mutex;

/// Test result with detailed information
#[derive(Debug, Clone)]
pub struct TestResult {
    pub name: String,
    pub passed: bool,
    pub message: String,
    pub duration: Duration,
    pub assertions: Vec<Assertion>,
    pub captured_output: Vec<String>,
    pub scene_snapshots: Vec<SceneSnapshot>,
    pub performance_metrics: PerformanceMetrics,
}

/// Individual assertion result
#[derive(Debug, Clone)]
pub struct Assertion {
    pub description: String,
    pub passed: bool,
    pub expected: String,
    pub actual: String,
    pub location: String,
}

/// Snapshot of scene state at a point in time
#[derive(Debug, Clone)]
pub struct SceneSnapshot {
    pub timestamp: Instant,
    pub label: String,
    pub cameras: HashMap<ObjectId, Camera>,
    pub nodes: HashMap<ObjectId, SceneNode>,
    pub active_camera: Option<ObjectId>,
}

/// Performance metrics
#[derive(Debug, Clone, Default)]
pub struct PerformanceMetrics {
    pub eval_time: Duration,
    pub parse_time: Duration,
    pub render_time: Duration,
    pub memory_usage: usize,
    pub object_count: usize,
    pub frame_times: Vec<Duration>,
}

/// Test harness for running XR-Lang tests
pub struct TestHarness {
    pub evaluator: Evaluator,
    pub captured_output: Arc<Mutex<Vec<String>>>,
    pub scene_snapshots: Vec<SceneSnapshot>,
    pub assertions: Vec<Assertion>,
    pub performance_metrics: PerformanceMetrics,
    pub hot_reload_manager: Option<HotReloadManager>,
    pub debug_mode: bool,
}

impl TestHarness {
    pub fn new() -> Self {
        let captured_output = Arc::new(Mutex::new(Vec::new()));
        let output_clone = Arc::clone(&captured_output);
        
        let mut evaluator = Evaluator::new();
        
        // Register camera intrinsics
        let camera_intrinsics = crate::intrinsics_camera::register_camera_intrinsics();
        let env = Rc::get_mut(&mut evaluator.global_env).unwrap();
        for (name, func) in camera_intrinsics {
            env.bind(Symbol(name), Value::NativeFunction(func));
        }
        
        // Override println to capture output
        env.bind(Symbol("println".to_string()), Value::NativeFunction(
            Rc::new(move |args| {
                let output = args.iter()
                    .map(|v| format!("{}", v))
                    .collect::<Vec<_>>()
                    .join(" ");
                output_clone.lock().unwrap().push(output.clone());
                println!("[TEST] {}", output);
                Ok(Value::Nil)
            })
        ));
        
        TestHarness {
            evaluator,
            captured_output,
            scene_snapshots: Vec::new(),
            assertions: Vec::new(),
            performance_metrics: PerformanceMetrics::default(),
            hot_reload_manager: None,
            debug_mode: false,
        }
    }
    
    /// Enable debug mode for verbose output
    pub fn enable_debug(&mut self) {
        self.debug_mode = true;
    }
    
    /// Execute XRL code and measure performance
    pub fn execute(&mut self, source: &str) -> Result<Value, String> {
        let parse_start = Instant::now();
        let mut parser = Parser::new(source);
        let exprs = parser.parse().map_err(|e| format!("Parse error: {}", e))?;
        self.performance_metrics.parse_time = parse_start.elapsed();
        
        let eval_start = Instant::now();
        let mut result = Value::Nil;
        for expr in exprs {
            result = self.evaluator.eval(&expr, Rc::clone(&self.evaluator.global_env))
                .map_err(|e| format!("Eval error: {}", e))?;
        }
        self.performance_metrics.eval_time = eval_start.elapsed();
        
        // Update object count
        SCENE.with(|scene| {
            let scene = scene.borrow();
            self.performance_metrics.object_count = scene.nodes.len() + scene.cameras.len();
        });
        
        Ok(result)
    }
    
    /// Execute XRL file
    pub fn execute_file(&mut self, path: &str) -> Result<Value, String> {
        let source = fs::read_to_string(path)
            .map_err(|e| format!("Failed to read file {}: {}", path, e))?;
        self.execute(&source)
    }
    
    /// Capture current scene state
    pub fn capture_scene_snapshot(&mut self, label: &str) -> SceneSnapshot {
        let snapshot = SCENE.with(|scene| {
            let scene = scene.borrow();
            SceneSnapshot {
                timestamp: Instant::now(),
                label: label.to_string(),
                cameras: scene.cameras.clone(),
                nodes: scene.nodes.clone(),
                active_camera: scene.active_camera,
            }
        });
        
        self.scene_snapshots.push(snapshot.clone());
        
        if self.debug_mode {
            println!("[SNAPSHOT] {} - {} nodes, {} cameras", 
                label, snapshot.nodes.len(), snapshot.cameras.len());
        }
        
        snapshot
    }
    
    /// Assert scene contains expected object
    pub fn assert_object_exists(&mut self, id: ObjectId, description: &str) -> bool {
        let exists = SCENE.with(|scene| {
            let scene = scene.borrow();
            scene.nodes.contains_key(&id) || scene.cameras.contains_key(&id)
        });
        
        self.assertions.push(Assertion {
            description: description.to_string(),
            passed: exists,
            expected: format!("Object {} exists", id.0),
            actual: if exists { "Object found".to_string() } else { "Object not found".to_string() },
            location: "scene".to_string(),
        });
        
        exists
    }
    
    /// Assert object has expected position
    pub fn assert_position(&mut self, id: ObjectId, expected: Vec3, tolerance: f32) -> bool {
        let actual_opt = SCENE.with(|scene| {
            let scene = scene.borrow();
            scene.nodes.get(&id).map(|n| n.transform.position)
                .or_else(|| scene.cameras.get(&id).map(|c| c.position))
        });
        
        let passed = if let Some(actual) = actual_opt {
            (actual.x - expected.x).abs() < tolerance &&
            (actual.y - expected.y).abs() < tolerance &&
            (actual.z - expected.z).abs() < tolerance
        } else {
            false
        };
        
        self.assertions.push(Assertion {
            description: format!("Object {} position", id.0),
            passed,
            expected: format!("[{:.2}, {:.2}, {:.2}]", expected.x, expected.y, expected.z),
            actual: actual_opt.map(|a| format!("[{:.2}, {:.2}, {:.2}]", a.x, a.y, a.z))
                .unwrap_or_else(|| "Object not found".to_string()),
            location: "transform".to_string(),
        });
        
        passed
    }
    
    /// Assert object scale
    pub fn assert_scale(&mut self, id: ObjectId, expected: Vec3, tolerance: f32) -> bool {
        let actual_opt = SCENE.with(|scene| {
            let scene = scene.borrow();
            scene.nodes.get(&id).map(|n| n.transform.scale)
        });
        
        let passed = if let Some(actual) = actual_opt {
            (actual.x - expected.x).abs() < tolerance &&
            (actual.y - expected.y).abs() < tolerance &&
            (actual.z - expected.z).abs() < tolerance
        } else {
            false
        };
        
        self.assertions.push(Assertion {
            description: format!("Object {} has scale {:?}", id.0, expected),
            passed,
            expected: format!("{:?}", expected),
            actual: actual_opt.map_or("Object not found".to_string(), |a| format!("{:?}", a)),
            location: "assert_scale".to_string(),
        });
        
        passed
    }
    
    /// Assert camera properties
    pub fn assert_camera_fov(&mut self, id: ObjectId, expected_fov: f32, tolerance: f32) -> bool {
        let actual_opt = SCENE.with(|scene| {
            scene.borrow().cameras.get(&id).map(|c| c.fov)
        });
        
        let passed = if let Some(actual) = actual_opt {
            (actual - expected_fov).abs() < tolerance
        } else {
            false
        };
        
        self.assertions.push(Assertion {
            description: format!("Camera {} FOV", id.0),
            passed,
            expected: format!("{:.1}°", expected_fov),
            actual: actual_opt.map(|a| format!("{:.1}°", a))
                .unwrap_or_else(|| "Camera not found".to_string()),
            location: "camera".to_string(),
        });
        
        passed
    }
    
    /// Simulate hot-reload by modifying file and triggering reload
    pub fn simulate_hot_reload(&mut self, file_path: &str, new_content: &str) -> Result<(), String> {
        // Save original content
        let original = fs::read_to_string(file_path).ok();
        
        // Write new content
        fs::write(file_path, new_content)
            .map_err(|e| format!("Failed to write file: {}", e))?;
        
        // Trigger reload (would normally be detected by file watcher)
        self.execute(new_content)?;
        
        // Restore original if it existed
        if let Some(orig) = original {
            fs::write(file_path, orig).ok();
        }
        
        Ok(())
    }
    
    /// Chain multiple interactions
    pub fn chain_interactions(&mut self, interactions: Vec<Interaction>) -> Result<(), String> {
        for interaction in interactions {
            match interaction {
                Interaction::Execute(code) => {
                    self.execute(&code)?;
                }
                Interaction::Wait(duration) => {
                    std::thread::sleep(duration);
                }
                Interaction::Snapshot(label) => {
                    self.capture_scene_snapshot(&label);
                }
                Interaction::Assert(assertion) => {
                    (assertion)(self);
                }
                Interaction::HotReload(path, content) => {
                    self.simulate_hot_reload(&path, &content)?;
                }
            }
        }
        Ok(())
    }
    
    /// Get test results
    pub fn get_results(&self, test_name: &str) -> TestResult {
        let all_passed = self.assertions.iter().all(|a| a.passed);
        let passed_count = self.assertions.iter().filter(|a| a.passed).count();
        let total_count = self.assertions.len();
        
        TestResult {
            name: test_name.to_string(),
            passed: all_passed,
            message: format!("{}/{} assertions passed", passed_count, total_count),
            duration: self.performance_metrics.eval_time + self.performance_metrics.parse_time,
            assertions: self.assertions.clone(),
            captured_output: self.captured_output.lock().unwrap().clone(),
            scene_snapshots: self.scene_snapshots.clone(),
            performance_metrics: self.performance_metrics.clone(),
        }
    }
    
    /// Clear test state
    pub fn reset(&mut self) {
        self.assertions.clear();
        self.scene_snapshots.clear();
        self.captured_output.lock().unwrap().clear();
        self.performance_metrics = PerformanceMetrics::default();
        
        // Reset scene
        SCENE.with(|scene| {
            let mut scene = scene.borrow_mut();
            scene.nodes.clear();
            scene.cameras.clear();
            scene.active_camera = None;
            scene.next_id = 1;
        });
    }
}

/// Interaction types for chaining
pub enum Interaction {
    Execute(String),
    Wait(Duration),
    Snapshot(String),
    Assert(Box<dyn Fn(&mut TestHarness)>),
    HotReload(String, String),
}

/// Test suite for running multiple tests
pub struct TestSuite {
    pub name: String,
    pub tests: Vec<Box<dyn Fn(&mut TestHarness) -> TestResult>>,
    pub setup: Option<Box<dyn Fn(&mut TestHarness)>>,
    pub teardown: Option<Box<dyn Fn(&mut TestHarness)>>,
}

impl TestSuite {
    pub fn new(name: &str) -> Self {
        TestSuite {
            name: name.to_string(),
            tests: Vec::new(),
            setup: None,
            teardown: None,
        }
    }
    
    pub fn add_test<F>(&mut self, test: F) 
    where
        F: Fn(&mut TestHarness) -> TestResult + 'static
    {
        self.tests.push(Box::new(test));
    }
    
    pub fn set_setup<F>(&mut self, setup: F)
    where
        F: Fn(&mut TestHarness) + 'static
    {
        self.setup = Some(Box::new(setup));
    }
    
    pub fn run(&self) -> Vec<TestResult> {
        let mut results = Vec::new();
        let mut harness = TestHarness::new();
        
        for test in &self.tests {
            // Run setup
            if let Some(ref setup) = self.setup {
                setup(&mut harness);
            }
            
            // Run test
            let result = test(&mut harness);
            results.push(result);
            
            // Run teardown
            if let Some(ref teardown) = self.teardown {
                teardown(&mut harness);
            }
            
            // Reset for next test
            harness.reset();
        }
        
        results
    }
}

/// Introspection utilities
pub struct Introspector;

impl Introspector {
    /// Get detailed scene graph
    pub fn get_scene_graph() -> SceneGraph {
        SCENE.with(|scene| {
            let scene = scene.borrow();
            SceneGraph {
                nodes: scene.nodes.values().cloned().collect(),
                cameras: scene.cameras.values().cloned().collect(),
                active_camera: scene.active_camera,
                stats: SceneStats {
                    node_count: scene.nodes.len(),
                    camera_count: scene.cameras.len(),
                    total_objects: scene.nodes.len() + scene.cameras.len(),
                },
            }
        })
    }
    
    /// Trace execution path
    pub fn trace_execution(source: &str) -> ExecutionTrace {
        let mut trace = ExecutionTrace::default();
        let start = Instant::now();
        
        // Parse
        let parse_start = Instant::now();
        let mut parser = Parser::new(source);
        match parser.parse() {
            Ok(exprs) => {
                trace.parse_time = parse_start.elapsed();
                trace.ast_nodes = exprs.len();
                
                // Evaluate
                let mut evaluator = Evaluator::new();
                for expr in exprs {
                    let eval_start = Instant::now();
                    match evaluator.eval(&expr, Rc::clone(&evaluator.global_env)) {
                        Ok(val) => {
                            trace.evaluations.push(EvalStep {
                                expr: format!("{:?}", expr),
                                result: format!("{}", val),
                                duration: eval_start.elapsed(),
                                success: true,
                            });
                        }
                        Err(e) => {
                            trace.evaluations.push(EvalStep {
                                expr: format!("{:?}", expr),
                                result: e.clone(),
                                duration: eval_start.elapsed(),
                                success: false,
                            });
                            trace.errors.push(e);
                        }
                    }
                }
            }
            Err(e) => {
                trace.errors.push(format!("Parse error: {}", e));
            }
        }
        
        trace.total_time = start.elapsed();
        trace
    }
    
    /// Get memory usage estimate
    pub fn estimate_memory_usage() -> MemoryUsage {
        SCENE.with(|scene| {
            let scene = scene.borrow();
            
            // Rough estimates
            let node_size = std::mem::size_of::<SceneNode>() * scene.nodes.len();
            let camera_size = std::mem::size_of::<Camera>() * scene.cameras.len();
            let overhead = 1024; // HashMap overhead estimate
            
            MemoryUsage {
                nodes: node_size,
                cameras: camera_size,
                total: node_size + camera_size + overhead,
            }
        })
    }
}

/// Scene graph representation
#[derive(Debug, Clone)]
pub struct SceneGraph {
    pub nodes: Vec<SceneNode>,
    pub cameras: Vec<Camera>,
    pub active_camera: Option<ObjectId>,
    pub stats: SceneStats,
}

#[derive(Debug, Clone)]
pub struct SceneStats {
    pub node_count: usize,
    pub camera_count: usize,
    pub total_objects: usize,
}

/// Execution trace for debugging
#[derive(Debug, Clone, Default)]
pub struct ExecutionTrace {
    pub parse_time: Duration,
    pub total_time: Duration,
    pub ast_nodes: usize,
    pub evaluations: Vec<EvalStep>,
    pub errors: Vec<String>,
}

#[derive(Debug, Clone)]
pub struct EvalStep {
    pub expr: String,
    pub result: String,
    pub duration: Duration,
    pub success: bool,
}

#[derive(Debug, Clone)]
pub struct MemoryUsage {
    pub nodes: usize,
    pub cameras: usize,
    pub total: usize,
}

/// Test runner for command-line execution
pub struct TestRunner {
    pub suites: Vec<TestSuite>,
    pub verbose: bool,
}

impl TestRunner {
    pub fn new() -> Self {
        TestRunner {
            suites: Vec::new(),
            verbose: false,
        }
    }
    
    pub fn add_suite(&mut self, suite: TestSuite) {
        self.suites.push(suite);
    }
    
    pub fn run_all(&self) -> TestReport {
        let mut report = TestReport::default();
        let start = Instant::now();
        
        for suite in &self.suites {
            println!("\n=== Running Test Suite: {} ===", suite.name);
            let results = suite.run();
            
            for result in &results {
                if self.verbose || !result.passed {
                    println!("\n  Test: {}", result.name);
                    println!("  Status: {}", if result.passed { "✅ PASSED" } else { "❌ FAILED" });
                    println!("  Message: {}", result.message);
                    
                    if !result.passed {
                        for assertion in &result.assertions {
                            if !assertion.passed {
                                println!("    ❌ {}: expected {}, got {}", 
                                    assertion.description, assertion.expected, assertion.actual);
                            }
                        }
                    }
                    
                    if self.verbose {
                        println!("  Duration: {:?}", result.duration);
                        if !result.captured_output.is_empty() {
                            println!("  Output:");
                            for line in &result.captured_output {
                                println!("    {}", line);
                            }
                        }
                    }
                }
                
                report.total_tests += 1;
                if result.passed {
                    report.passed_tests += 1;
                } else {
                    report.failed_tests += 1;
                    report.failed_test_names.push(result.name.clone());
                }
                report.results.push(result.clone());
            }
        }
        
        report.total_duration = start.elapsed();
        report
    }
    
    pub fn print_summary(&self, report: &TestReport) {
        println!("\n{}", "=".repeat(60));
        println!("TEST SUMMARY");
        println!("{}", "=".repeat(60));
        println!("Total Tests: {}", report.total_tests);
        println!("Passed: {} ✅", report.passed_tests);
        println!("Failed: {} ❌", report.failed_tests);
        println!("Duration: {:?}", report.total_duration);
        
        if !report.failed_test_names.is_empty() {
            println!("\nFailed Tests:");
            for name in &report.failed_test_names {
                println!("  - {}", name);
            }
        }
        
        let pass_rate = if report.total_tests > 0 {
            (report.passed_tests as f64 / report.total_tests as f64) * 100.0
        } else {
            0.0
        };
        
        println!("\nPass Rate: {:.1}%", pass_rate);
        
        if report.failed_tests == 0 {
            println!("\n🎉 All tests passed!");
        }
    }
}

#[derive(Debug, Clone, Default)]
pub struct TestReport {
    pub total_tests: usize,
    pub passed_tests: usize,
    pub failed_tests: usize,
    pub failed_test_names: Vec<String>,
    pub total_duration: Duration,
    pub results: Vec<TestResult>,
}