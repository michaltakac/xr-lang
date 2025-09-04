//! Test runner for XR-Lang tests

use crate::ast::*;
use anyhow::Result;
use colored::*;
use std::time::{Duration, Instant};
use std::collections::HashMap;

pub struct TestRunner {
    pub results: Vec<TestResult>,
    pub config: TestConfig,
    pub state: TestState,
}

#[derive(Debug, Clone)]
pub struct TestConfig {
    pub parallel: bool,
    pub verbose: bool,
    pub fail_fast: bool,
    pub timeout: Duration,
    pub device_profile: Option<String>,
}

#[derive(Debug, Clone)]
pub struct TestState {
    pub scene_data: Option<gpu::SceneData>,
    pub snapshots: HashMap<String, Snapshot>,
    pub timeline: Timeline,
    pub preserved_state: HashMap<String, PreservedState>,
}

#[derive(Debug, Clone)]
pub struct TestResult {
    pub name: String,
    pub status: TestStatus,
    pub duration: Duration,
    pub assertions: Vec<AssertionResult>,
    pub error: Option<String>,
    pub artifacts: Vec<TestArtifact>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum TestStatus {
    Pending,
    Running,
    Passed,
    Failed,
    Skipped,
    Timeout,
    Error,
}

#[derive(Debug, Clone)]
pub struct AssertionResult {
    pub assertion: String,
    pub passed: bool,
    pub message: Option<String>,
    pub actual: Option<String>,
    pub expected: Option<String>,
}

#[derive(Debug, Clone)]
pub struct TestArtifact {
    pub artifact_type: String,
    pub name: String,
    pub data: Vec<u8>,
}

#[derive(Debug, Clone)]
pub struct Snapshot {
    pub name: String,
    pub timestamp: Instant,
    pub data: SnapshotData,
}

#[derive(Debug, Clone)]
pub enum SnapshotData {
    Scene(gpu::SceneData),
    Image(Vec<u8>),
    State(HashMap<String, TestValue>),
}

#[derive(Debug, Clone)]
pub struct Timeline {
    pub events: Vec<TimelineEntry>,
    pub current_time: u32,
    pub branches: HashMap<String, Vec<TimelineEntry>>,
}

#[derive(Debug, Clone)]
pub struct TimelineEntry {
    pub time_ms: u32,
    pub event: TimelineEventType,
}

#[derive(Debug, Clone)]
pub enum TimelineEventType {
    StateChange(String, TestValue),
    Action(TestAction),
    Assertion(TestAssertion, bool),
}

#[derive(Debug, Clone)]
pub struct PreservedState {
    pub object_id: String,
    pub properties: HashMap<String, TestValue>,
}

impl TestRunner {
    pub fn new(config: TestConfig) -> Self {
        Self {
            results: Vec::new(),
            config,
            state: TestState {
                scene_data: None,
                snapshots: HashMap::new(),
                timeline: Timeline {
                    events: Vec::new(),
                    current_time: 0,
                    branches: HashMap::new(),
                },
                preserved_state: HashMap::new(),
            },
        }
    }
    
    pub async fn run_test(&mut self, test: &TestDef) -> Result<TestResult> {
        let start = Instant::now();
        let mut assertions = Vec::new();
        let mut error = None;
        let mut status = TestStatus::Passed;
        
        println!("{} Running test: {}", "▶".cyan(), test.name.bold());
        
        // Setup
        if let Some(setup) = &test.setup {
            if let Err(e) = self.run_setup(setup).await {
                error = Some(format!("Setup failed: {}", e));
                status = TestStatus::Error;
                return Ok(TestResult {
                    name: test.name.clone(),
                    status,
                    duration: start.elapsed(),
                    assertions,
                    error,
                    artifacts: Vec::new(),
                });
            }
        }
        
        // Run actions
        for action in &test.actions {
            if let Err(e) = self.execute_action(action).await {
                error = Some(format!("Action failed: {}", e));
                status = TestStatus::Error;
                break;
            }
        }
        
        // Run assertions
        if status != TestStatus::Error {
            for assertion in &test.assertions {
                let result = self.execute_assertion(assertion).await;
                let passed = result.passed;
                assertions.push(result);
                
                if !passed {
                    status = TestStatus::Failed;
                    if self.config.fail_fast {
                        break;
                    }
                }
            }
        }
        
        // Teardown
        if let Some(teardown) = &test.teardown {
            let _ = self.run_teardown(teardown).await;
        }
        
        let duration = start.elapsed();
        
        // Print result
        let status_str = match status {
            TestStatus::Pending => "○".white(),
            TestStatus::Running => "●".cyan(),
            TestStatus::Passed => "✓".green(),
            TestStatus::Failed => "✗".red(),
            TestStatus::Skipped => "⊘".yellow(),
            TestStatus::Timeout => "⏱".red(),
            TestStatus::Error => "⚠".red(),
        };
        
        println!("{} {} ({}ms)", 
            status_str, 
            test.name, 
            duration.as_millis());
        
        Ok(TestResult {
            name: test.name.clone(),
            status,
            duration,
            assertions,
            error,
            artifacts: Vec::new(),
        })
    }
    
    async fn run_setup(&mut self, setup: &TestSetup) -> Result<()> {
        for step in &setup.steps {
            self.execute_step(step).await?;
        }
        Ok(())
    }
    
    async fn run_teardown(&mut self, teardown: &TestTeardown) -> Result<()> {
        for step in &teardown.steps {
            self.execute_step(step).await?;
        }
        Ok(())
    }
    
    async fn execute_step(&mut self, step: &TestStep) -> Result<()> {
        // Execute test step
        match step.action.as_str() {
            "create-scene" => {
                // Create a test scene
                self.state.scene_data = Some(gpu::SceneData::default());
            }
            "load-scene" => {
                if let Some(TestValue::String(_file)) = step.params.get("file") {
                    // Load scene from file
                    // TODO: Implement scene loading
                }
            }
            _ => {}
        }
        Ok(())
    }
    
    async fn execute_action(&mut self, action: &TestAction) -> Result<()> {
        match action {
            TestAction::CreateObject { name: _name, object_type: _object_type, properties: _properties } => {
                // Create object in scene
                if let Some(ref mut _scene) = self.state.scene_data {
                    // TODO: Add object to scene
                }
            }
            TestAction::MoveCamera { position, target } => {
                if let Some(ref mut scene) = self.state.scene_data {
                    scene.camera = Some(gpu::CameraData {
                        position: gpu::Vec3::from(*position),
                        target: gpu::Vec3::from(*target),
                        fov: 60.0_f32.to_radians(),
                        meta: None,
                    });
                }
            }
            TestAction::TriggerHotReload => {
                // Simulate hot reload
                // TODO: Implement hot reload simulation
            }
            TestAction::Wait { duration_ms } => {
                tokio::time::sleep(Duration::from_millis(*duration_ms as u64)).await;
            }
            TestAction::RecordSnapshot { name } => {
                if let Some(ref scene) = self.state.scene_data {
                    self.state.snapshots.insert(
                        name.clone(),
                        Snapshot {
                            name: name.clone(),
                            timestamp: Instant::now(),
                            data: SnapshotData::Scene(scene.clone()),
                        },
                    );
                }
            }
            _ => {}
        }
        Ok(())
    }
    
    async fn execute_assertion(&mut self, assertion: &TestAssertion) -> AssertionResult {
        match assertion {
            TestAssertion::AssertEq { actual, expected } => {
                // Compare actual vs expected
                AssertionResult {
                    assertion: format!("assert_eq({}, {:?})", actual, expected),
                    passed: true, // TODO: Implement actual comparison
                    message: None,
                    actual: Some(actual.clone()),
                    expected: Some(format!("{:?}", expected)),
                }
            }
            TestAssertion::AssertPreserved { object } => {
                // Check if object state was preserved
                let preserved = self.state.preserved_state.contains_key(object);
                AssertionResult {
                    assertion: format!("assert_preserved({})", object),
                    passed: preserved,
                    message: if preserved {
                        Some(format!("{} was preserved", object))
                    } else {
                        Some(format!("{} was not preserved", object))
                    },
                    actual: None,
                    expected: None,
                }
            }
            TestAssertion::AssertNoErrors => {
                AssertionResult {
                    assertion: "assert_no_errors".to_string(),
                    passed: true, // TODO: Check error state
                    message: None,
                    actual: None,
                    expected: None,
                }
            }
            _ => AssertionResult {
                assertion: format!("{:?}", assertion),
                passed: false,
                message: Some("Not implemented".to_string()),
                actual: None,
                expected: None,
            },
        }
    }
    
    pub fn print_summary(&self) {
        let total = self.results.len();
        let passed = self.results.iter().filter(|r| r.status == TestStatus::Passed).count();
        let failed = self.results.iter().filter(|r| r.status == TestStatus::Failed).count();
        let skipped = self.results.iter().filter(|r| r.status == TestStatus::Skipped).count();
        let errors = self.results.iter().filter(|r| r.status == TestStatus::Error).count();
        
        println!("\n{}", "═".repeat(60));
        println!("{}", "Test Summary".bold());
        println!("{}", "═".repeat(60));
        
        println!("Total:   {}", total);
        if passed > 0 {
            println!("Passed:  {} {}", passed, "✓".green());
        }
        if failed > 0 {
            println!("Failed:  {} {}", failed, "✗".red());
        }
        if skipped > 0 {
            println!("Skipped: {} {}", skipped, "⊘".yellow());
        }
        if errors > 0 {
            println!("Errors:  {} {}", errors, "⚠".red());
        }
        
        let total_duration: Duration = self.results.iter().map(|r| r.duration).sum();
        println!("\nTotal time: {:.2}s", total_duration.as_secs_f32());
        
        // Print failures
        if failed > 0 || errors > 0 {
            println!("\n{}", "Failures:".red().bold());
            for result in &self.results {
                if result.status == TestStatus::Failed || result.status == TestStatus::Error {
                    println!("  {} {}", "✗".red(), result.name);
                    if let Some(ref error) = result.error {
                        println!("    {}", error.red());
                    }
                    for assertion in &result.assertions {
                        if !assertion.passed {
                            println!("    {} {}", "✗".red(), assertion.assertion);
                            if let Some(ref msg) = assertion.message {
                                println!("      {}", msg);
                            }
                        }
                    }
                }
            }
        }
    }
}