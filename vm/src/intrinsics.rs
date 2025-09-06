//! Scene Primitives as Intrinsic Functions
//! 
//! Performance-critical 3D operations implemented in Rust and exposed to XR-Lang.
//! Following the Lisp tradition of using native functions for heavy computation.

use crate::value::{Value, NativeFn, ObjectId};
use std::rc::Rc;
use std::collections::HashMap;

/// 3D Vector type
#[derive(Debug, Clone, Copy, PartialEq)]
pub struct Vec3 {
    pub x: f32,
    pub y: f32,
    pub z: f32,
}

impl Vec3 {
    pub fn new(x: f32, y: f32, z: f32) -> Self {
        Vec3 { x, y, z }
    }
    
    pub fn from_value(val: &Value) -> Result<Self, String> {
        match val {
            Value::Vector(items) if items.len() == 3 => {
                let x = match &items[0] {
                    Value::Float(f) => *f as f32,
                    Value::Int(i) => *i as f32,
                    _ => return Err("Invalid x coordinate".to_string()),
                };
                let y = match &items[1] {
                    Value::Float(f) => *f as f32,
                    Value::Int(i) => *i as f32,
                    _ => return Err("Invalid y coordinate".to_string()),
                };
                let z = match &items[2] {
                    Value::Float(f) => *f as f32,
                    Value::Int(i) => *i as f32,
                    _ => return Err("Invalid z coordinate".to_string()),
                };
                Ok(Vec3::new(x, y, z))
            }
            _ => Err("Expected vector of 3 numbers".to_string()),
        }
    }
    
    pub fn to_value(&self) -> Value {
        Value::Vector(vec![
            Value::Float(self.x as f64),
            Value::Float(self.y as f64),
            Value::Float(self.z as f64),
        ])
    }
}

/// Transform component
#[derive(Debug, Clone)]
pub struct Transform {
    pub position: Vec3,
    pub rotation: Vec3,
    pub scale: Vec3,
}

impl Transform {
    pub fn identity() -> Self {
        Transform {
            position: Vec3::new(0.0, 0.0, 0.0),
            rotation: Vec3::new(0.0, 0.0, 0.0),
            scale: Vec3::new(1.0, 1.0, 1.0),
        }
    }
}

/// Camera component
#[derive(Debug, Clone)]
pub struct Camera {
    pub position: Vec3,
    pub target: Vec3,
    pub fov: f32,
}

/// Simple scene graph node
#[derive(Debug, Clone)]
pub struct SceneNode {
    pub id: ObjectId,
    pub name: String,
    pub transform: Transform,
    pub mesh_type: Option<String>,
    pub children: Vec<ObjectId>,
}

/// Global scene state (would be managed by the runtime)
pub struct SceneState {
    pub next_id: u64,
    pub nodes: HashMap<ObjectId, SceneNode>,
    pub cameras: HashMap<ObjectId, Camera>,
}

impl SceneState {
    pub fn new() -> Self {
        SceneState {
            next_id: 1,
            nodes: HashMap::new(),
            cameras: HashMap::new(),
        }
    }
    
    fn generate_id(&mut self) -> ObjectId {
        let id = ObjectId(self.next_id);
        self.next_id += 1;
        id
    }
}

// Thread-local scene state (in a real implementation, this would be managed differently)
thread_local! {
    pub static SCENE: std::cell::RefCell<SceneState> = std::cell::RefCell::new(SceneState::new());
}

/// Create a camera in the scene
/// Can be called with 1, 2, or 3 arguments:
/// (create-camera position)
/// (create-camera position target)
/// (create-camera position target fov-degrees)
pub fn intrinsic_create_camera() -> NativeFn {
    Rc::new(|args| {
        if args.is_empty() || args.len() > 3 {
            return Err("create-camera expects 1-3 arguments (position, [target], [fov-degrees])".to_string());
        }
        
        let position = Vec3::from_value(&args[0])?;
        
        let target = if args.len() > 1 {
            Vec3::from_value(&args[1])?
        } else {
            Vec3::new(0.0, 0.0, 0.0)
        };
        
        let fov = if args.len() > 2 {
            match &args[2] {
                Value::Float(f) => *f as f32,
                Value::Int(i) => *i as f32,
                _ => return Err("FOV must be a number (in degrees)".to_string()),
            }
        } else {
            60.0
        };
        
        SCENE.with(|scene| {
            let mut scene = scene.borrow_mut();
            let id = scene.generate_id();
            
            let camera = Camera {
                position,
                target,
                fov: fov.to_radians(),  // Convert to radians for renderer
            };
            
            scene.cameras.insert(id, camera);
            Ok(Value::Object(id))
        })
    })
}

/// Create a cube mesh
pub fn intrinsic_create_cube() -> NativeFn {
    Rc::new(|args| {
        if args.len() != 1 {
            return Err("create-cube expects 1 argument (position)".to_string());
        }
        
        let position = Vec3::from_value(&args[0])?;
        
        SCENE.with(|scene| {
            let mut scene = scene.borrow_mut();
            let id = scene.generate_id();
            
            let mut transform = Transform::identity();
            transform.position = position;
            
            let node = SceneNode {
                id,
                name: format!("cube_{}", id.0),
                transform,
                mesh_type: Some("cube".to_string()),
                children: Vec::new(),
            };
            
            scene.nodes.insert(id, node);
            Ok(Value::Object(id))
        })
    })
}

/// Create a sphere mesh
pub fn intrinsic_create_sphere() -> NativeFn {
    Rc::new(|args| {
        if args.len() != 1 {
            return Err("create-sphere expects 1 argument (position)".to_string());
        }
        
        let position = Vec3::from_value(&args[0])?;
        
        SCENE.with(|scene| {
            let mut scene = scene.borrow_mut();
            let id = scene.generate_id();
            
            let mut transform = Transform::identity();
            transform.position = position;
            
            let node = SceneNode {
                id,
                name: format!("sphere_{}", id.0),
                transform,
                mesh_type: Some("sphere".to_string()),
                children: Vec::new(),
            };
            
            scene.nodes.insert(id, node);
            Ok(Value::Object(id))
        })
    })
}

/// Update transform of an object
pub fn intrinsic_update_transform() -> NativeFn {
    Rc::new(|args| {
        if args.len() != 2 {
            return Err("update-transform expects 2 arguments (object-id, position)".to_string());
        }
        
        let id = match &args[0] {
            Value::Object(id) => *id,
            _ => return Err("First argument must be an object ID".to_string()),
        };
        
        let position = Vec3::from_value(&args[1])?;
        
        SCENE.with(|scene| {
            let mut scene = scene.borrow_mut();
            
            if let Some(node) = scene.nodes.get_mut(&id) {
                node.transform.position = position;
                Ok(Value::Nil)
            } else if let Some(camera) = scene.cameras.get_mut(&id) {
                camera.position = position;
                Ok(Value::Nil)
            } else {
                Err(format!("Object {:?} not found", id))
            }
        })
    })
}

/// Rotate an object
pub fn intrinsic_rotate() -> NativeFn {
    Rc::new(|args| {
        if args.len() != 2 {
            return Err("rotate expects 2 arguments (object-id, rotation)".to_string());
        }
        
        let id = match &args[0] {
            Value::Object(id) => *id,
            _ => return Err("First argument must be an object ID".to_string()),
        };
        
        let rotation = Vec3::from_value(&args[1])?;
        
        SCENE.with(|scene| {
            let mut scene = scene.borrow_mut();
            
            if let Some(node) = scene.nodes.get_mut(&id) {
                node.transform.rotation = rotation;
                Ok(Value::Nil)
            } else {
                Err(format!("Object {:?} not found", id))
            }
        })
    })
}

/// Scale an object
pub fn intrinsic_scale() -> NativeFn {
    Rc::new(|args| {
        if args.len() != 2 {
            return Err("scale expects 2 arguments (object-id, scale)".to_string());
        }
        
        let id = match &args[0] {
            Value::Object(id) => *id,
            _ => return Err("First argument must be an object ID".to_string()),
        };
        
        let scale = Vec3::from_value(&args[1])?;
        
        SCENE.with(|scene| {
            let mut scene = scene.borrow_mut();
            
            if let Some(node) = scene.nodes.get_mut(&id) {
                node.transform.scale = scale;
                Ok(Value::Nil)
            } else {
                Err(format!("Object {:?} not found", id))
            }
        })
    })
}

/// Get the position of an object
pub fn intrinsic_get_position() -> NativeFn {
    Rc::new(|args| {
        if args.len() != 1 {
            return Err("get-position expects 1 argument (object-id)".to_string());
        }
        
        let id = match &args[0] {
            Value::Object(id) => *id,
            _ => return Err("Argument must be an object ID".to_string()),
        };
        
        SCENE.with(|scene| {
            let scene = scene.borrow();
            
            if let Some(node) = scene.nodes.get(&id) {
                Ok(node.transform.position.to_value())
            } else if let Some(camera) = scene.cameras.get(&id) {
                Ok(camera.position.to_value())
            } else {
                Err(format!("Object {:?} not found", id))
            }
        })
    })
}

/// Matrix multiplication for transforms (performance-critical)
pub fn intrinsic_matrix_multiply() -> NativeFn {
    Rc::new(|args| {
        if args.len() != 2 {
            return Err("matrix-multiply expects 2 arguments".to_string());
        }
        
        // In a real implementation, this would do actual 4x4 matrix multiplication
        // For now, just return a placeholder
        Ok(Value::Nil)
    })
}

/// Ray-cast for picking objects
pub fn intrinsic_raycast() -> NativeFn {
    Rc::new(|args| {
        if args.len() != 2 {
            return Err("raycast expects 2 arguments (origin, direction)".to_string());
        }
        
        let _origin = Vec3::from_value(&args[0])?;
        let _direction = Vec3::from_value(&args[1])?;
        
        // In a real implementation, this would perform actual raycasting
        // For now, return nil (no hit)
        Ok(Value::Nil)
    })
}

/// Register all scene intrinsics in an environment
pub fn register_scene_intrinsics() -> HashMap<String, NativeFn> {
    let mut intrinsics = HashMap::new();
    
    intrinsics.insert("create-camera".to_string(), intrinsic_create_camera());
    intrinsics.insert("create-cube".to_string(), intrinsic_create_cube());
    intrinsics.insert("create-sphere".to_string(), intrinsic_create_sphere());
    intrinsics.insert("update-transform".to_string(), intrinsic_update_transform());
    intrinsics.insert("rotate".to_string(), intrinsic_rotate());
    intrinsics.insert("scale".to_string(), intrinsic_scale());
    intrinsics.insert("get-position".to_string(), intrinsic_get_position());
    intrinsics.insert("matrix-multiply".to_string(), intrinsic_matrix_multiply());
    intrinsics.insert("raycast".to_string(), intrinsic_raycast());
    
    intrinsics
}

#[cfg(test)]
mod tests {
    use super::*;
    
    #[test]
    fn test_vec3_conversion() {
        let v = Vec3::new(1.0, 2.0, 3.0);
        let val = v.to_value();
        let v2 = Vec3::from_value(&val).unwrap();
        assert_eq!(v, v2);
    }
    
    #[test]
    fn test_create_camera() {
        let create_camera = intrinsic_create_camera();
        let pos = Value::Vector(vec![Value::Float(0.0), Value::Float(5.0), Value::Float(10.0)]);
        let result = create_camera(&[pos]).unwrap();
        
        match result {
            Value::Object(_) => (), // Success
            _ => panic!("Expected object ID"),
        }
    }
    
    #[test]
    fn test_create_and_move_cube() {
        let create_cube = intrinsic_create_cube();
        let update_transform = intrinsic_update_transform();
        
        // Create cube
        let pos1 = Value::Vector(vec![Value::Int(0), Value::Int(0), Value::Int(0)]);
        let cube = create_cube(&[pos1]).unwrap();
        
        // Move cube
        let pos2 = Value::Vector(vec![Value::Int(5), Value::Int(0), Value::Int(0)]);
        let result = update_transform(&[cube.clone(), pos2]).unwrap();
        assert_eq!(result, Value::Nil);
        
        // Get position
        let get_pos = intrinsic_get_position();
        let pos = get_pos(&[cube]).unwrap();
        
        match pos {
            Value::Vector(coords) => {
                assert_eq!(coords[0], Value::Float(5.0));
            }
            _ => panic!("Expected vector"),
        }
    }
}