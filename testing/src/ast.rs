//! Testing DSL AST extensions

use serde::{Deserialize, Serialize};
use std::collections::HashMap;

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct TestDef {
    pub name: String,
    pub description: Option<String>,
    pub setup: Option<TestSetup>,
    pub actions: Vec<TestAction>,
    pub assertions: Vec<TestAssertion>,
    pub teardown: Option<TestTeardown>,
    pub meta: Option<TestMeta>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct TestSetup {
    pub steps: Vec<TestStep>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct TestTeardown {
    pub steps: Vec<TestStep>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct TestStep {
    pub action: String,
    pub params: HashMap<String, TestValue>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum TestAction {
    CreateObject { name: String, object_type: String, properties: HashMap<String, TestValue> },
    MoveCamera { position: [f32; 3], target: [f32; 3] },
    TriggerHotReload,
    Wait { duration_ms: u32 },
    SendInput { input_type: String, data: TestValue },
    RecordSnapshot { name: String },
    ExecuteCode { code: String },
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum TestAssertion {
    AssertEq { actual: String, expected: TestValue },
    AssertNe { actual: String, expected: TestValue },
    AssertTrue { condition: String },
    AssertFalse { condition: String },
    AssertPreserved { object: String },
    AssertNoErrors,
    AssertSnapshot { name: String, tolerance: f32 },
    AssertPerformance { metric: String, threshold: f32 },
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum TestValue {
    F32(f32),
    I32(i32),
    Bool(bool),
    String(String),
    Vec3([f32; 3]),
    List(Vec<TestValue>),
    Map(HashMap<String, TestValue>),
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct TestMeta {
    pub tags: Vec<String>,
    pub skip: bool,
    pub timeout_ms: Option<u32>,
    pub flaky: bool,
    pub device_profiles: Vec<String>,
}

// Recording-based test
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct RecordingTest {
    pub name: String,
    pub recording_file: String,
    pub playback_speed: f32,
    pub assertions: Vec<TestAssertion>,
}

// Property-based test
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct PropertyTest {
    pub name: String,
    pub generators: HashMap<String, GeneratorDef>,
    pub invariant: InvariantDef,
    pub shrink_strategy: Option<ShrinkStrategy>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct GeneratorDef {
    pub generator_type: String,
    pub params: HashMap<String, TestValue>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct InvariantDef {
    pub condition: String,
    pub description: Option<String>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum ShrinkStrategy {
    Automatic,
    Custom { function: String },
    None,
}

// Time-travel testing
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct TemporalTest {
    pub name: String,
    pub timeline: TimelineDef,
    pub assertions: Vec<TemporalAssertion>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct TimelineDef {
    pub branches: Vec<TimelineBranch>,
    pub duration_ms: u32,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct TimelineBranch {
    pub name: String,
    pub parent: Option<String>,
    pub branch_point_ms: u32,
    pub events: Vec<TimelineEvent>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct TimelineEvent {
    pub time_ms: u32,
    pub action: TestAction,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum TemporalAssertion {
    AtTime { time_ms: u32, assertion: Box<TestAssertion> },
    AtAllTimes { assertion: Box<TestAssertion> },
    BetweenTimes { start_ms: u32, end_ms: u32, assertion: Box<TestAssertion> },
    AtBranchPoint { branch: String, assertion: Box<TestAssertion> },
}

// Device-specific testing
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct DeviceTest {
    pub name: String,
    pub devices: Vec<String>,
    pub setup: Option<TestSetup>,
    pub scenarios: Vec<DeviceScenario>,
    pub assertions: Vec<DeviceAssertion>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct DeviceScenario {
    pub name: String,
    pub device_constraints: DeviceConstraints,
    pub actions: Vec<TestAction>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct DeviceConstraints {
    pub gpu_memory_mb: Option<u32>,
    pub thermal_state: Option<String>,
    pub battery_level: Option<u8>,
    pub network_conditions: Option<NetworkConditions>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct NetworkConditions {
    pub latency_ms: u32,
    pub bandwidth_mbps: u32,
    pub packet_loss_percent: f32,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum DeviceAssertion {
    AssertFramerate { min_fps: f32 },
    AssertNoFrameDrops,
    AssertThermalSustainable,
    AssertMemoryWithinBudget { max_mb: u32 },
    AssertCompatible { feature: String },
}