//! 3D visualization for test execution and results

use crate::runner::*;
use gpu::{Vec3, Vec2, Vec4, SceneData, CubeData, UIElementData};

pub struct TestVisualization {
    pub test_cubes: Vec<TestCube>,
    pub assertion_markers: Vec<AssertionMarker>,
    pub timeline_scrubber: TimelineScrubber,
    pub status_panel: StatusPanel,
}

#[derive(Debug, Clone)]
pub struct TestCube {
    pub name: String,
    pub position: Vec3,
    pub color: Vec3,
    pub status: TestStatus,
    pub scale: Vec3,
}

#[derive(Debug, Clone)]
pub struct AssertionMarker {
    pub position: Vec3,
    pub assertion: String,
    pub passed: bool,
    pub message: String,
}

#[derive(Debug, Clone)]
pub struct TimelineScrubber {
    pub position: Vec3,
    pub width: f32,
    pub current_time: f32,
    pub total_time: f32,
}

#[derive(Debug, Clone)]
pub struct StatusPanel {
    pub position: Vec3,
    pub size: Vec2,
    pub tests_run: usize,
    pub tests_passed: usize,
    pub tests_failed: usize,
}

impl TestVisualization {
    pub fn new() -> Self {
        Self {
            test_cubes: Vec::new(),
            assertion_markers: Vec::new(),
            timeline_scrubber: TimelineScrubber {
                position: Vec3::new(0.0, -2.0, 0.0),
                width: 10.0,
                current_time: 0.0,
                total_time: 100.0,
            },
            status_panel: StatusPanel {
                position: Vec3::new(5.0, 3.0, 0.0),
                size: Vec2::new(3.0, 2.0),
                tests_run: 0,
                tests_passed: 0,
                tests_failed: 0,
            },
        }
    }
    
    pub fn update_from_results(&mut self, results: &[TestResult]) {
        self.test_cubes.clear();
        self.assertion_markers.clear();
        
        // Create a cube for each test
        let spacing = 2.0;
        let row_size = 5;
        
        for (i, result) in results.iter().enumerate() {
            let row = i / row_size;
            let col = i % row_size;
            
            let x = col as f32 * spacing - (row_size as f32 * spacing / 2.0);
            let z = row as f32 * spacing;
            
            let color = match result.status {
                TestStatus::Passed => Vec3::new(0.0, 1.0, 0.0),   // Green
                TestStatus::Failed => Vec3::new(1.0, 0.0, 0.0),   // Red
                TestStatus::Skipped => Vec3::new(1.0, 1.0, 0.0),  // Yellow
                TestStatus::Timeout => Vec3::new(1.0, 0.5, 0.0),  // Orange
                TestStatus::Error => Vec3::new(0.5, 0.0, 0.5),    // Purple
            };
            
            self.test_cubes.push(TestCube {
                name: result.name.clone(),
                position: Vec3::new(x, 0.0, z),
                color,
                status: result.status.clone(),
                scale: Vec3::ONE,
            });
            
            // Add assertion markers
            for (j, assertion) in result.assertions.iter().enumerate() {
                let marker_y = 2.0 + j as f32 * 0.5;
                
                self.assertion_markers.push(AssertionMarker {
                    position: Vec3::new(x, marker_y, z),
                    assertion: assertion.assertion.clone(),
                    passed: assertion.passed,
                    message: assertion.message.clone().unwrap_or_default(),
                });
            }
        }
        
        // Update status panel
        self.status_panel.tests_run = results.len();
        self.status_panel.tests_passed = results.iter()
            .filter(|r| r.status == TestStatus::Passed).count();
        self.status_panel.tests_failed = results.iter()
            .filter(|r| r.status == TestStatus::Failed).count();
    }
    
    pub fn to_scene_data(&self) -> SceneData {
        let mut cubes = Vec::new();
        let mut ui_elements = Vec::new();
        
        // Add test cubes
        for test_cube in &self.test_cubes {
            cubes.push(CubeData {
                name: test_cube.name.clone(),
                position: test_cube.position,
                scale: test_cube.scale,
                rotation: gpu::Quat::IDENTITY,
                color: test_cube.color,
                behavior: None,
                interactive: true,
                id: Some(format!("test_{}", test_cube.name)),
                meta: None,
            });
        }
        
        // Add assertion markers as small cubes
        for marker in &self.assertion_markers {
            let color = if marker.passed {
                Vec3::new(0.0, 1.0, 0.0)  // Green
            } else {
                Vec3::new(1.0, 0.0, 0.0)  // Red
            };
            
            cubes.push(CubeData {
                name: format!("assertion_{}", marker.assertion),
                position: marker.position,
                scale: Vec3::new(0.2, 0.2, 0.2),
                rotation: gpu::Quat::IDENTITY,
                color,
                behavior: None,
                interactive: false,
                id: Some(format!("assertion_{}", marker.assertion)),
                meta: None,
            });
        }
        
        // Add timeline scrubber as a plane
        cubes.push(CubeData {
            name: "timeline_scrubber".to_string(),
            position: self.timeline_scrubber.position,
            scale: Vec3::new(self.timeline_scrubber.width, 0.1, 1.0),
            rotation: gpu::Quat::IDENTITY,
            color: Vec3::new(0.5, 0.5, 0.5),
            behavior: None,
            interactive: true,
            id: Some("timeline_scrubber".to_string()),
            meta: None,
        });
        
        // Add timeline progress
        let progress = self.timeline_scrubber.current_time / self.timeline_scrubber.total_time;
        cubes.push(CubeData {
            name: "timeline_progress".to_string(),
            position: self.timeline_scrubber.position + Vec3::new(
                -self.timeline_scrubber.width / 2.0 + progress * self.timeline_scrubber.width / 2.0,
                0.1,
                0.0
            ),
            scale: Vec3::new(progress * self.timeline_scrubber.width, 0.15, 0.8),
            rotation: gpu::Quat::IDENTITY,
            color: Vec3::new(0.0, 0.5, 1.0),
            behavior: None,
            interactive: false,
            id: Some("timeline_progress".to_string()),
            meta: None,
        });
        
        // Add status panel UI
        ui_elements.push(UIElementData {
            name: "status_panel".to_string(),
            ui_type: "panel".to_string(),
            position: self.status_panel.position,
            size: [self.status_panel.size.x, self.status_panel.size.y],
            text: Some(format!(
                "Tests: {}\nPassed: {} ✓\nFailed: {} ✗",
                self.status_panel.tests_run,
                self.status_panel.tests_passed,
                self.status_panel.tests_failed
            )),
            color: [0.2, 0.2, 0.2, 0.8],
            behavior: None,
        });
        
        SceneData {
            cubes,
            ui_elements,
            behaviors: std::collections::HashMap::new(),
            camera: Some(gpu::CameraData {
                position: Vec3::new(0.0, 10.0, 15.0),
                target: Vec3::new(0.0, 0.0, 0.0),
                fov: 60.0_f32.to_radians(),
                meta: None,
            }),
            lighting: None,
            input: None,
        }
    }
    
    pub fn handle_interaction(&mut self, object_name: &str, interaction_type: &str) {
        if object_name.starts_with("test_") {
            // Handle test cube interaction
            if interaction_type == "click" {
                println!("Test cube clicked: {}", object_name);
                // Could show test details, re-run test, etc.
            }
        } else if object_name == "timeline_scrubber" && interaction_type == "drag" {
            // Handle timeline scrubbing
            println!("Timeline scrubbing...");
            // Could seek to specific point in test execution
        }
    }
    
    pub fn animate(&mut self, dt: f32) {
        // Animate test cubes
        for cube in &mut self.test_cubes {
            match cube.status {
                TestStatus::Passed => {
                    // Gentle pulse for passed tests
                    let pulse = (self.timeline_scrubber.current_time * 2.0).sin() * 0.1 + 1.0;
                    cube.scale = Vec3::new(pulse, pulse, pulse);
                }
                TestStatus::Failed => {
                    // Shake failed tests
                    let shake = (self.timeline_scrubber.current_time * 10.0).sin() * 0.05;
                    cube.position.x += shake;
                }
                _ => {}
            }
        }
        
        // Update timeline
        self.timeline_scrubber.current_time += dt;
        if self.timeline_scrubber.current_time > self.timeline_scrubber.total_time {
            self.timeline_scrubber.current_time = 0.0;
        }
    }
}