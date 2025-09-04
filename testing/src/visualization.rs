//! 3D visualization for test execution and results

use crate::runner::*;
use gpu::{Vec3, SceneData, UIElementData};
use gpu::entity::{Entity, MeshSource, PrimitiveType, Transform};
use gpu::scene::CameraData;
use gpu::math::Quat;

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
    pub size: [f32; 2],
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
                position: Vec3::new(0.0, -5.0, 0.0),
                width: 10.0,
                current_time: 0.0,
                total_time: 100.0,
            },
            status_panel: StatusPanel {
                position: Vec3::new(5.0, 3.0, 0.0),
                size: [3.0, 2.0],
                tests_run: 0,
                tests_passed: 0,
                tests_failed: 0,
            },
        }
    }
    
    pub fn update_from_results(&mut self, results: &[TestResult]) {
        // Create test cubes in a grid
        let grid_size = (results.len() as f32).sqrt().ceil() as usize;
        
        self.test_cubes.clear();
        self.assertion_markers.clear();
        
        for (i, result) in results.iter().enumerate() {
            let x = (i % grid_size) as f32 * 2.0 - grid_size as f32;
            let z = (i / grid_size) as f32 * 2.0 - grid_size as f32;
            
            let color = match result.status {
                TestStatus::Pending => Vec3::new(0.5, 0.5, 0.5),
                TestStatus::Running => Vec3::new(1.0, 1.0, 0.0),
                TestStatus::Passed => Vec3::new(0.0, 1.0, 0.0),
                TestStatus::Failed => Vec3::new(1.0, 0.0, 0.0),
                TestStatus::Skipped => Vec3::new(0.3, 0.3, 0.3),
                TestStatus::Timeout => Vec3::new(1.0, 0.5, 0.0),
                TestStatus::Error => Vec3::new(0.8, 0.0, 0.8),
            };
            
            self.test_cubes.push(TestCube {
                name: result.name.clone(),
                position: Vec3::new(x, 0.0, z),
                color,
                status: result.status.clone(),
                scale: Vec3::new(1.0, 1.0, 1.0),
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
        let mut entities = Vec::new();
        
        // Convert test cubes to entities
        for test_cube in &self.test_cubes {
            let color = match test_cube.status {
                TestStatus::Pending => [0.5, 0.5, 0.5, 1.0],
                TestStatus::Running => [1.0, 1.0, 0.0, 1.0],
                TestStatus::Passed => [0.0, 1.0, 0.0, 1.0],
                TestStatus::Failed => [1.0, 0.0, 0.0, 1.0],
                TestStatus::Skipped => [0.3, 0.3, 0.3, 1.0],
                TestStatus::Timeout => [1.0, 0.5, 0.0, 1.0],
                TestStatus::Error => [0.8, 0.0, 0.8, 1.0],
            };
            
            entities.push(Entity {
                id: format!("test_{}", test_cube.name),
                name: test_cube.name.clone(),
                mesh: MeshSource::Primitive(PrimitiveType::cube()),
                transform: Transform {
                    position: test_cube.position,
                    rotation: Quat::IDENTITY,
                    scale: test_cube.scale,
                },
                material: Some(dsl::ast::MaterialDef::MeshBasic {
                    color,
                    opacity: 1.0,
                    transparent: false,
                    side: "front".to_string(),
                    wireframe: false,
                }),
                behavior: None,
                children: vec![],
                parent: None,
                components: vec![],
                meta: None,
            });
        }
        
        // Add assertion markers as small spheres
        for marker in &self.assertion_markers {
            let color = if marker.passed {
                [0.0, 1.0, 0.0, 1.0]  // Green
            } else {
                [1.0, 0.0, 0.0, 1.0]  // Red
            };
            
            entities.push(Entity {
                id: format!("assertion_{}", marker.assertion),
                name: format!("assertion_{}", marker.assertion),
                mesh: MeshSource::Primitive(PrimitiveType::sphere()),
                transform: Transform {
                    position: marker.position,
                    rotation: Quat::IDENTITY,
                    scale: Vec3::new(0.2, 0.2, 0.2),
                },
                material: Some(dsl::ast::MaterialDef::MeshBasic {
                    color,
                    opacity: 1.0,
                    transparent: false,
                    side: "front".to_string(),
                    wireframe: false,
                }),
                behavior: None,
                children: vec![],
                parent: None,
                components: vec![],
                meta: None,
            });
        }
        
        // Add timeline scrubber as a plane
        entities.push(Entity {
            id: "timeline_scrubber".to_string(),
            name: "timeline_scrubber".to_string(),
            mesh: MeshSource::Primitive(PrimitiveType::Plane { width: 10.0, height: 10.0, subdivisions: 1 }),
            transform: Transform {
                position: self.timeline_scrubber.position,
                rotation: Quat::IDENTITY,
                scale: Vec3::new(self.timeline_scrubber.width, 0.1, 1.0),
            },
            material: Some(dsl::ast::MaterialDef::MeshBasic {
                color: [0.5, 0.5, 0.5, 1.0],
                opacity: 1.0,
                transparent: false,
                side: "double".to_string(),
                wireframe: false,
            }),
            behavior: None,
            children: vec![],
            parent: None,
            components: vec![],
            meta: None,
        });
        
        // Add timeline progress
        let progress = self.timeline_scrubber.current_time / self.timeline_scrubber.total_time;
        entities.push(Entity {
            id: "timeline_progress".to_string(),
            name: "timeline_progress".to_string(),
            mesh: MeshSource::Primitive(PrimitiveType::Plane { width: 10.0, height: 10.0, subdivisions: 1 }),
            transform: Transform {
                position: self.timeline_scrubber.position + Vec3::new(
                    -self.timeline_scrubber.width / 2.0 + progress * self.timeline_scrubber.width / 2.0,
                    0.1,
                    0.0
                ),
                rotation: Quat::IDENTITY,
                scale: Vec3::new(progress * self.timeline_scrubber.width, 0.15, 0.8),
            },
            material: Some(dsl::ast::MaterialDef::MeshBasic {
                color: [0.0, 0.5, 1.0, 1.0],
                opacity: 1.0,
                transparent: false,
                side: "double".to_string(),
                wireframe: false,
            }),
            behavior: None,
            children: vec![],
            parent: None,
            components: vec![],
            meta: None,
        });
        
        // Create UI elements for status panel
        let ui_elements = vec![
            UIElementData {
                name: "status_panel".to_string(),
                ui_type: "panel".to_string(),
                position: self.status_panel.position,
                size: self.status_panel.size,
                text: Some(format!(
                    "Tests: {}\nPassed: {} ✓\nFailed: {} ✗",
                    self.status_panel.tests_run,
                    self.status_panel.tests_passed,
                    self.status_panel.tests_failed
                )),
                color: [0.2, 0.2, 0.2, 0.8],
                behavior: None,
            }
        ];
        
        SceneData {
            entities,
            ui_elements,
            behaviors: std::collections::HashMap::new(),
            camera: Some(CameraData {
                position: Vec3::new(0.0, 10.0, 15.0),
                target: Vec3::new(0.0, 0.0, 0.0),
                fov: 60.0_f32.to_radians(),
                meta: None,
            }),
            lighting: None,
            input: None,
            ast: vec![],
        }
    }
    
    pub fn update_timeline(&mut self, elapsed: f32) {
        self.timeline_scrubber.current_time = 
            (self.timeline_scrubber.current_time + elapsed) % self.timeline_scrubber.total_time;
    }
}