//! XRL file runner - evaluates XR-Lang code and renders 3D scenes

use anyhow::{Result, Context};
use std::path::Path;
use vm::{
    parser::Parser,
    evaluator::Evaluator,
    value::{Environment, ObjectId, Value, Symbol},
    intrinsics::{SCENE, register_scene_intrinsics},
};
use gpu::scene::{SceneData, CameraData};
use gpu::entity::{Entity, PrimitiveType, MeshSource, Transform};
use gpu::{Vec3, Quat};
use std::rc::Rc;

pub struct XrlRunner {
    evaluator: Evaluator,
    environment: Rc<Environment>,
}

impl XrlRunner {
    pub fn new() -> Self {
        // Create environment with intrinsic functions
        let mut env = Environment::new();
        let intrinsics = register_scene_intrinsics();
        
        // Register all intrinsic functions in the environment
        for (name, func) in intrinsics {
            env.bind(Symbol(name), Value::NativeFunction(func));
        }
        
        // Add basic utility functions
        env.bind(Symbol("list".to_string()), Value::NativeFunction(
            Rc::new(|args| Ok(Value::List(args.to_vec())))
        ));
        
        Self {
            evaluator: Evaluator::new(),
            environment: Rc::new(env),
        }
    }
    
    /// Load and evaluate an XRL file, returning GPU-ready scene data
    pub fn load_xrl_file(&mut self, path: &Path) -> Result<SceneData> {
        let source = std::fs::read_to_string(path)
            .with_context(|| format!("Failed to read XRL file: {}", path.display()))?;
        
        self.evaluate_xrl(&source)
    }
    
    /// Evaluate XRL source code and extract scene data
    pub fn evaluate_xrl(&mut self, source: &str) -> Result<SceneData> {
        // Clear previous scene data
        SCENE.with(|scene| {
            let mut scene = scene.borrow_mut();
            scene.nodes.clear();
            scene.cameras.clear();
            scene.next_id = 1;
        });
        
        // Parse the source
        let mut parser = Parser::new(source);
        let expressions = parser.parse()
            .map_err(|e| anyhow::anyhow!("Failed to parse XRL source: {}", e))?;
        
        // Evaluate all expressions
        for expr in expressions {
            self.evaluator.eval(&expr, self.environment.clone())
                .map_err(|e| anyhow::anyhow!("Failed to evaluate expression: {:?} - {}", expr, e))?;
        }
        
        // Extract scene data from VM and convert to GPU format
        let mut scene_data = SceneData::default();
        
        // Add default camera controls for XRL scenes
        scene_data.input = Some(gpu::scene::InputData {
            camera_controls: Some(gpu::scene::CameraControlsData {
                move_speed: 5.0,
                rotate_speed: 2.0,
                movement_keys: gpu::scene::MovementKeysData {
                    forward: "W".to_string(),
                    backward: "S".to_string(),
                    left: "A".to_string(),
                    right: "D".to_string(),
                    up: "Space".to_string(),
                    down: "Shift".to_string(),
                },
                rotation_keys: gpu::scene::RotationKeysData {
                    pitch_up: "Up".to_string(),
                    pitch_down: "Down".to_string(),
                    yaw_left: "Left".to_string(),
                    yaw_right: "Right".to_string(),
                },
                orbit_controls: Some(gpu::scene::OrbitControlsData {
                    enabled: true,
                    sensitivity: 1.0,
                    damping: 0.05,
                    min_distance: 1.0,
                    max_distance: 50.0,
                    min_polar_angle: 0.1,
                    max_polar_angle: 3.0,
                    enable_zoom: true,
                    zoom_speed: 1.0,
                }),
            }),
        });
        
        SCENE.with(|scene| {
            let scene = scene.borrow();
            
            // Convert cameras
            if let Some((_, camera)) = scene.cameras.iter().next() {
                scene_data.camera = Some(CameraData {
                    position: Vec3::new(camera.position.x, camera.position.y, camera.position.z),
                    target: Vec3::new(camera.target.x, camera.target.y, camera.target.z),
                    fov: camera.fov,
                    meta: None,
                });
            }
            
            // Convert scene nodes to entities
            for (ObjectId(id), node) in scene.nodes.iter() {
                if let Some(mesh_type) = &node.mesh_type {
                    // Use the from_type_string method that handles parameterized formats
                    // like "sphere:1.5:32" or "cylinder:0.5:2:24"
                    let mesh_source = if let Some(primitive) = PrimitiveType::from_type_string(mesh_type) {
                        MeshSource::Primitive(primitive)
                    } else {
                        // Default to a cube if unknown
                        println!("Warning: Unknown mesh type '{}', using default cube", mesh_type);
                        MeshSource::Primitive(PrimitiveType::Box {
                            width: 1.0,
                            height: 1.0,
                            depth: 1.0,
                        })
                    };
                    
                    let mut transform = Transform::default();
                    transform.position = Vec3::new(
                        node.transform.position.x,
                        node.transform.position.y,
                        node.transform.position.z,
                    );
                    transform.rotation = Quat::from_euler(
                        node.transform.rotation.x.to_radians(),
                        node.transform.rotation.y.to_radians(),
                        node.transform.rotation.z.to_radians()
                    );
                    transform.scale = Vec3::new(
                        node.transform.scale.x,
                        node.transform.scale.y,
                        node.transform.scale.z,
                    );
                    
                    scene_data.entities.push(Entity {
                        id: format!("object_{}", id),
                        name: node.name.clone(),
                        mesh: mesh_source,
                        transform,
                        material: None,
                        behavior: None,
                        children: Vec::new(),
                        parent: None,
                        components: Vec::new(),
                        meta: None,
                    });
                }
            }
        });
        
        Ok(scene_data)
    }
    
    /// Hot-reload support: re-evaluate file and return new scene
    pub fn reload_xrl_file(&mut self, path: &Path) -> Result<SceneData> {
        // Create fresh environment for clean reload
        self.environment = Rc::new(Environment::new());
        self.evaluator = Evaluator::new();
        
        self.load_xrl_file(path)
    }
}