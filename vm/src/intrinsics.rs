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
    
    pub fn length(&self) -> f32 {
        (self.x * self.x + self.y * self.y + self.z * self.z).sqrt()
    }
}

use std::ops::{Add, Sub};

impl Add for Vec3 {
    type Output = Vec3;
    
    fn add(self, other: Vec3) -> Vec3 {
        Vec3::new(self.x + other.x, self.y + other.y, self.z + other.z)
    }
}

impl Sub for Vec3 {
    type Output = Vec3;
    
    fn sub(self, other: Vec3) -> Vec3 {
        Vec3::new(self.x - other.x, self.y - other.y, self.z - other.z)
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
    pub material: Material,
    pub children: Vec<ObjectId>,
}

/// Simple material system (headless representation)
#[derive(Debug, Clone)]
pub enum Material {
    /// Unlit solid color
    Basic {
        color: [f32; 4],
        animated: bool, // emulate default looping color transition
    },
    /// PBR-like material (similar to Bevy/Three.js standard)
    Standard {
        base_color: [f32; 4],
        metallic: f32,
        roughness: f32,
        emissive: [f32; 3],
    },
    /// Lambert (diffuse-only approximation)
    Lambert {
        color: [f32; 4],
    },
    /// Phong (specular highlights)
    Phong {
        color: [f32; 4],
        shininess: f32,
        specular: [f32; 3],
    },
    /// Toon (cel-shading parameters)
    Toon {
        color: [f32; 4],
        levels: u32,
    },
    /// Normal visualization
    Normal,
}

impl Material {
    pub fn default_animated_basic() -> Self {
        Material::Basic { color: [0.6, 0.8, 1.0, 1.0], animated: true }
    }

    pub fn set_color(&mut self, rgba: [f32; 4]) {
        match self {
            Material::Basic { color, .. } => *color = rgba,
            Material::Standard { base_color, .. } => *base_color = rgba,
            Material::Lambert { color } => *color = rgba,
            Material::Phong { color, .. } => *color = rgba,
            Material::Toon { color, .. } => *color = rgba,
            Material::Normal => {}
        }
    }
}

/// Global scene state (would be managed by the runtime)
pub struct SceneState {
    pub next_id: u64,
    pub nodes: HashMap<ObjectId, SceneNode>,
    pub cameras: HashMap<ObjectId, Camera>,
    pub active_camera: Option<ObjectId>,
}

impl SceneState {
    pub fn new() -> Self {
        SceneState {
            next_id: 1,
            nodes: HashMap::new(),
            cameras: HashMap::new(),
            active_camera: None,
        }
    }
    
    pub fn generate_id(&mut self) -> ObjectId {
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
                material: Material::default_animated_basic(),
                children: Vec::new(),
            };
            
            scene.nodes.insert(id, node);
            Ok(Value::Object(id))
        })
    })
}

/// Create a sphere mesh
/// (create-sphere position [radius] [segments])
pub fn intrinsic_create_sphere() -> NativeFn {
    Rc::new(|args| {
        if args.is_empty() || args.len() > 3 {
            return Err("create-sphere expects 1-3 arguments (position, [radius], [segments])".to_string());
        }
        
        let position = Vec3::from_value(&args[0])?;
        
        let radius = if args.len() > 1 {
            match &args[1] {
                Value::Float(f) => *f as f32,
                Value::Int(i) => *i as f32,
                _ => return Err("Radius must be a number".to_string()),
            }
        } else {
            1.0
        };
        
        let segments = if args.len() > 2 {
            match &args[2] {
                Value::Int(i) => *i as u32,
                _ => return Err("Segments must be an integer".to_string()),
            }
        } else {
            32
        };
        
        SCENE.with(|scene| {
            let mut scene = scene.borrow_mut();
            let id = scene.generate_id();
            
            let mut transform = Transform::identity();
            transform.position = position;
            
            let node = SceneNode {
                id,
                name: format!("sphere_{}", id.0),
                transform,
                mesh_type: Some(format!("sphere:{}:{}", radius, segments)),
                material: Material::default_animated_basic(),
                children: Vec::new(),
            };
            
            scene.nodes.insert(id, node);
            Ok(Value::Object(id))
        })
    })
}

/// Create a cylinder mesh
/// (create-cylinder position [radius] [height] [segments])
pub fn intrinsic_create_cylinder() -> NativeFn {
    Rc::new(|args| {
        if args.is_empty() || args.len() > 4 {
            return Err("create-cylinder expects 1-4 arguments (position, [radius], [height], [segments])".to_string());
        }
        
        let position = Vec3::from_value(&args[0])?;
        
        let radius = if args.len() > 1 {
            match &args[1] {
                Value::Float(f) => *f as f32,
                Value::Int(i) => *i as f32,
                _ => return Err("Radius must be a number".to_string()),
            }
        } else {
            1.0
        };
        
        let height = if args.len() > 2 {
            match &args[2] {
                Value::Float(f) => *f as f32,
                Value::Int(i) => *i as f32,
                _ => return Err("Height must be a number".to_string()),
            }
        } else {
            2.0
        };
        
        let segments = if args.len() > 3 {
            match &args[3] {
                Value::Int(i) => *i as u32,
                _ => return Err("Segments must be an integer".to_string()),
            }
        } else {
            32
        };
        
        SCENE.with(|scene| {
            let mut scene = scene.borrow_mut();
            let id = scene.generate_id();
            
            let mut transform = Transform::identity();
            transform.position = position;
            
            let node = SceneNode {
                id,
                name: format!("cylinder_{}", id.0),
                transform,
                mesh_type: Some(format!("cylinder:{}:{}:{}", radius, height, segments)),
                material: Material::default_animated_basic(),
                children: Vec::new(),
            };
            
            scene.nodes.insert(id, node);
            Ok(Value::Object(id))
        })
    })
}

/// Create a cone mesh
/// (create-cone position [radius] [height] [segments])
pub fn intrinsic_create_cone() -> NativeFn {
    Rc::new(|args| {
        if args.is_empty() || args.len() > 4 {
            return Err("create-cone expects 1-4 arguments (position, [radius], [height], [segments])".to_string());
        }
        
        let position = Vec3::from_value(&args[0])?;
        
        let radius = if args.len() > 1 {
            match &args[1] {
                Value::Float(f) => *f as f32,
                Value::Int(i) => *i as f32,
                _ => return Err("Radius must be a number".to_string()),
            }
        } else {
            1.0
        };
        
        let height = if args.len() > 2 {
            match &args[2] {
                Value::Float(f) => *f as f32,
                Value::Int(i) => *i as f32,
                _ => return Err("Height must be a number".to_string()),
            }
        } else {
            2.0
        };
        
        let segments = if args.len() > 3 {
            match &args[3] {
                Value::Int(i) => *i as u32,
                _ => return Err("Segments must be an integer".to_string()),
            }
        } else {
            32
        };
        
        SCENE.with(|scene| {
            let mut scene = scene.borrow_mut();
            let id = scene.generate_id();
            
            let mut transform = Transform::identity();
            transform.position = position;
            
            let node = SceneNode {
                id,
                name: format!("cone_{}", id.0),
                transform,
                mesh_type: Some(format!("cone:{}:{}:{}", radius, height, segments)),
                material: Material::default_animated_basic(),
                children: Vec::new(),
            };
            
            scene.nodes.insert(id, node);
            Ok(Value::Object(id))
        })
    })
}

/// Create a pyramid mesh
/// (create-pyramid position [base-width] [base-depth] [height])
pub fn intrinsic_create_pyramid() -> NativeFn {
    Rc::new(|args| {
        if args.is_empty() || args.len() > 4 {
            return Err("create-pyramid expects 1-4 arguments (position, [base-width], [base-depth], [height])".to_string());
        }
        
        let position = Vec3::from_value(&args[0])?;
        
        let base_width = if args.len() > 1 {
            match &args[1] {
                Value::Float(f) => *f as f32,
                Value::Int(i) => *i as f32,
                _ => return Err("Base width must be a number".to_string()),
            }
        } else {
            2.0
        };
        
        let base_depth = if args.len() > 2 {
            match &args[2] {
                Value::Float(f) => *f as f32,
                Value::Int(i) => *i as f32,
                _ => return Err("Base depth must be a number".to_string()),
            }
        } else {
            2.0
        };
        
        let height = if args.len() > 3 {
            match &args[3] {
                Value::Float(f) => *f as f32,
                Value::Int(i) => *i as f32,
                _ => return Err("Height must be a number".to_string()),
            }
        } else {
            2.0
        };
        
        SCENE.with(|scene| {
            let mut scene = scene.borrow_mut();
            let id = scene.generate_id();
            
            let mut transform = Transform::identity();
            transform.position = position;
            
            let node = SceneNode {
                id,
                name: format!("pyramid_{}", id.0),
                transform,
                mesh_type: Some(format!("pyramid:{}:{}:{}", base_width, base_depth, height)),
                material: Material::default_animated_basic(),
                children: Vec::new(),
            };
            
            scene.nodes.insert(id, node);
            Ok(Value::Object(id))
        })
    })
}

/// Create a wedge mesh
/// (create-wedge position [width] [height] [depth])
pub fn intrinsic_create_wedge() -> NativeFn {
    Rc::new(|args| {
        if args.is_empty() || args.len() > 4 {
            return Err("create-wedge expects 1-4 arguments (position, [width], [height], [depth])".to_string());
        }
        
        let position = Vec3::from_value(&args[0])?;
        
        let width = if args.len() > 1 {
            match &args[1] {
                Value::Float(f) => *f as f32,
                Value::Int(i) => *i as f32,
                _ => return Err("Width must be a number".to_string()),
            }
        } else {
            2.0
        };
        
        let height = if args.len() > 2 {
            match &args[2] {
                Value::Float(f) => *f as f32,
                Value::Int(i) => *i as f32,
                _ => return Err("Height must be a number".to_string()),
            }
        } else {
            2.0
        };
        
        let depth = if args.len() > 3 {
            match &args[3] {
                Value::Float(f) => *f as f32,
                Value::Int(i) => *i as f32,
                _ => return Err("Depth must be a number".to_string()),
            }
        } else {
            2.0
        };
        
        SCENE.with(|scene| {
            let mut scene = scene.borrow_mut();
            let id = scene.generate_id();
            
            let mut transform = Transform::identity();
            transform.position = position;
            
            let node = SceneNode {
                id,
                name: format!("wedge_{}", id.0),
                transform,
                mesh_type: Some(format!("wedge:{}:{}:{}", width, height, depth)),
                material: Material::default_animated_basic(),
                children: Vec::new(),
            };
            
            scene.nodes.insert(id, node);
            Ok(Value::Object(id))
        })
    })
}

/// Create a torus (donut) mesh
/// (create-torus position [major-radius] [minor-radius] [segments] [rings])
pub fn intrinsic_create_torus() -> NativeFn {
    Rc::new(|args| {
        if args.is_empty() || args.len() > 5 {
            return Err("create-torus expects 1-5 arguments (position, [major-radius], [minor-radius], [segments], [rings])".to_string());
        }
        
        let position = Vec3::from_value(&args[0])?;
        
        let major_radius = if args.len() > 1 {
            match &args[1] {
                Value::Float(f) => *f as f32,
                Value::Int(i) => *i as f32,
                _ => return Err("Major radius must be a number".to_string()),
            }
        } else {
            1.0
        };
        
        let minor_radius = if args.len() > 2 {
            match &args[2] {
                Value::Float(f) => *f as f32,
                Value::Int(i) => *i as f32,
                _ => return Err("Minor radius must be a number".to_string()),
            }
        } else {
            0.3
        };
        
        let segments = if args.len() > 3 {
            match &args[3] {
                Value::Int(i) => *i as u32,
                _ => return Err("Segments must be an integer".to_string()),
            }
        } else {
            24
        };
        
        let rings = if args.len() > 4 {
            match &args[4] {
                Value::Int(i) => *i as u32,
                _ => return Err("Rings must be an integer".to_string()),
            }
        } else {
            18
        };
        
        SCENE.with(|scene| {
            let mut scene = scene.borrow_mut();
            let id = scene.generate_id();
            
            let mut transform = Transform::identity();
            transform.position = position;
            
            let node = SceneNode {
                id,
                name: format!("torus_{}", id.0),
                transform,
                mesh_type: Some(format!("torus:{}:{}:{}:{}", major_radius, minor_radius, segments, rings)),
                material: Material::default_animated_basic(),
                children: Vec::new(),
            };
            
            scene.nodes.insert(id, node);
            Ok(Value::Object(id))
        })
    })
}

/// Create a plane mesh
/// (create-plane position [width] [height] [subdivisions])
pub fn intrinsic_create_plane() -> NativeFn {
    Rc::new(|args| {
        if args.is_empty() || args.len() > 4 {
            return Err("create-plane expects 1-4 arguments (position, [width], [height], [subdivisions])".to_string());
        }
        
        let position = Vec3::from_value(&args[0])?;
        
        let width = if args.len() > 1 {
            match &args[1] {
                Value::Float(f) => *f as f32,
                Value::Int(i) => *i as f32,
                _ => return Err("Width must be a number".to_string()),
            }
        } else {
            10.0
        };
        
        let height = if args.len() > 2 {
            match &args[2] {
                Value::Float(f) => *f as f32,
                Value::Int(i) => *i as f32,
                _ => return Err("Height must be a number".to_string()),
            }
        } else {
            10.0
        };
        
        let subdivisions = if args.len() > 3 {
            match &args[3] {
                Value::Int(i) => *i as u32,
                _ => return Err("Subdivisions must be an integer".to_string()),
            }
        } else {
            10
        };
        
        SCENE.with(|scene| {
            let mut scene = scene.borrow_mut();
            let id = scene.generate_id();
            
            let mut transform = Transform::identity();
            transform.position = position;
            
            let node = SceneNode {
                id,
                name: format!("plane_{}", id.0),
                transform,
                mesh_type: Some(format!("plane:{}:{}:{}", width, height, subdivisions)),
                material: Material::default_animated_basic(),
                children: Vec::new(),
            };
            
            scene.nodes.insert(id, node);
            Ok(Value::Object(id))
        })
    })
}

/// Create a capsule mesh
/// (create-capsule position [radius] [height] [segments])
pub fn intrinsic_create_capsule() -> NativeFn {
    Rc::new(|args| {
        if args.is_empty() || args.len() > 4 {
            return Err("create-capsule expects 1-4 arguments (position, [radius], [height], [segments])".to_string());
        }
        
        let position = Vec3::from_value(&args[0])?;
        
        let radius = if args.len() > 1 {
            match &args[1] {
                Value::Float(f) => *f as f32,
                Value::Int(i) => *i as f32,
                _ => return Err("Radius must be a number".to_string()),
            }
        } else {
            0.5
        };
        
        let height = if args.len() > 2 {
            match &args[2] {
                Value::Float(f) => *f as f32,
                Value::Int(i) => *i as f32,
                _ => return Err("Height must be a number".to_string()),
            }
        } else {
            2.0
        };
        
        let segments = if args.len() > 3 {
            match &args[3] {
                Value::Int(i) => *i as u32,
                _ => return Err("Segments must be an integer".to_string()),
            }
        } else {
            24
        };
        
        SCENE.with(|scene| {
            let mut scene = scene.borrow_mut();
            let id = scene.generate_id();
            
            let mut transform = Transform::identity();
            transform.position = position;
            
            let node = SceneNode {
                id,
                name: format!("capsule_{}", id.0),
                transform,
                mesh_type: Some(format!("capsule:{}:{}:{}", radius, height, segments)),
                material: Material::default_animated_basic(),
                children: Vec::new(),
            };
            
            scene.nodes.insert(id, node);
            Ok(Value::Object(id))
        })
    })
}

/// Create an icosahedron mesh
/// (create-icosahedron position [radius])
pub fn intrinsic_create_icosahedron() -> NativeFn {
    Rc::new(|args| {
        if args.is_empty() || args.len() > 2 {
            return Err("create-icosahedron expects 1-2 arguments (position, [radius])".to_string());
        }
        
        let position = Vec3::from_value(&args[0])?;
        
        let radius = if args.len() > 1 {
            match &args[1] {
                Value::Float(f) => *f as f32,
                Value::Int(i) => *i as f32,
                _ => return Err("Radius must be a number".to_string()),
            }
        } else {
            1.0
        };
        
        SCENE.with(|scene| {
            let mut scene = scene.borrow_mut();
            let id = scene.generate_id();
            
            let mut transform = Transform::identity();
            transform.position = position;
            
            let node = SceneNode {
                id,
                name: format!("icosahedron_{}", id.0),
                transform,
                mesh_type: Some(format!("icosahedron:{}", radius)),
                material: Material::default_animated_basic(),
                children: Vec::new(),
            };
            
            scene.nodes.insert(id, node);
            Ok(Value::Object(id))
        })
    })
}

/// Create an octahedron mesh
/// (create-octahedron position [radius])
pub fn intrinsic_create_octahedron() -> NativeFn {
    Rc::new(|args| {
        if args.is_empty() || args.len() > 2 {
            return Err("create-octahedron expects 1-2 arguments (position, [radius])".to_string());
        }
        
        let position = Vec3::from_value(&args[0])?;
        
        let radius = if args.len() > 1 {
            match &args[1] {
                Value::Float(f) => *f as f32,
                Value::Int(i) => *i as f32,
                _ => return Err("Radius must be a number".to_string()),
            }
        } else {
            1.0
        };
        
        SCENE.with(|scene| {
            let mut scene = scene.borrow_mut();
            let id = scene.generate_id();
            
            let mut transform = Transform::identity();
            transform.position = position;
            
            let node = SceneNode {
                id,
                name: format!("octahedron_{}", id.0),
                transform,
                mesh_type: Some(format!("octahedron:{}", radius)),
                material: Material::default_animated_basic(),
                children: Vec::new(),
            };
            
            scene.nodes.insert(id, node);
            Ok(Value::Object(id))
        })
    })
}

/// Create a tetrahedron mesh
/// (create-tetrahedron position [radius])
pub fn intrinsic_create_tetrahedron() -> NativeFn {
    Rc::new(|args| {
        if args.is_empty() || args.len() > 2 {
            return Err("create-tetrahedron expects 1-2 arguments (position, [radius])".to_string());
        }
        
        let position = Vec3::from_value(&args[0])?;
        
        let radius = if args.len() > 1 {
            match &args[1] {
                Value::Float(f) => *f as f32,
                Value::Int(i) => *i as f32,
                _ => return Err("Radius must be a number".to_string()),
            }
        } else {
            1.0
        };
        
        SCENE.with(|scene| {
            let mut scene = scene.borrow_mut();
            let id = scene.generate_id();
            
            let mut transform = Transform::identity();
            transform.position = position;
            
            let node = SceneNode {
                id,
                name: format!("tetrahedron_{}", id.0),
                transform,
                mesh_type: Some(format!("tetrahedron:{}", radius)),
                material: Material::default_animated_basic(),
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

/// Set color on an object (stores as RGBA on the scene node)
/// (set-color object-id [r g b a]) where components are 0..1
pub fn intrinsic_set_color() -> NativeFn {
    Rc::new(|args| {
        if args.len() != 2 {
            return Err("set-color expects 2 arguments (object-id, [r g b a])".to_string());
        }

        let id = match &args[0] {
            Value::Object(id) => *id,
            _ => return Err("First argument must be an object ID".to_string()),
        };

        let rgba = parse_color_value(&args[1])?;

        SCENE.with(|scene| {
            let mut scene = scene.borrow_mut();
            if let Some(node) = scene.nodes.get_mut(&id) {
                node.material.set_color(rgba);
                Ok(Value::Nil)
            } else {
                Err(format!("Object {:?} not found", id))
            }
        })
    })
}

fn parse_color_value(val: &Value) -> Result<[f32; 4], String> {
    match val {
        // Vector [r g b] or [r g b a] with components in 0..1 or 0..255
        Value::Vector(v) => {
            if v.len() == 3 {
                let r = normalize_component(&v[0])?;
                let g = normalize_component(&v[1])?;
                let b = normalize_component(&v[2])?;
                Ok([r, g, b, 1.0])
            } else if v.len() == 4 {
                let r = normalize_component(&v[0])?;
                let g = normalize_component(&v[1])?;
                let b = normalize_component(&v[2])?;
                let a = normalize_component(&v[3])?; // allow 0..255 or 0..1
                Ok([r, g, b, a])
            } else {
                Err("Color vector must have 3 or 4 components".to_string())
            }
        }
        // Hex string or named color
        Value::Str(s) => parse_color_string(s),
        // Map { :r .. :g .. :b .. [:a ..] } or {"r":..}
        Value::Map(m) => {
            let r = m.get("r").ok_or("Missing r in color map")?;
            let g = m.get("g").ok_or("Missing g in color map")?;
            let b = m.get("b").ok_or("Missing b in color map")?;
            let a = m.get("a");
            let rr = normalize_component(r)?;
            let gg = normalize_component(g)?;
            let bb = normalize_component(b)?;
            let aa = a.map(normalize_component).transpose()?.unwrap_or(1.0);
            Ok([rr, gg, bb, aa])
        }
        _ => Err("Unsupported color value; use [r g b a], \"#RRGGBB\", or {r g b a}".to_string()),
    }
}

fn normalize_component(v: &Value) -> Result<f32, String> {
    match v {
        Value::Float(f) => {
            if *f > 1.0 { Ok((*f as f32) / 255.0) } else { Ok(*f as f32) }
        }
        Value::Int(n) => {
            if *n > 1 { Ok((*n as f32) / 255.0) } else { Ok(*n as f32) }
        }
        _ => Err("Color component must be a number".to_string()),
    }
}

fn parse_color_string(s: &str) -> Result<[f32; 4], String> {
    let lower = s.to_ascii_lowercase();
    if let Some(rgba) = named_color(&lower) {
        return Ok(rgba);
    }
    let hex = lower.trim_start_matches('#');
    let parse_hex2 = |h: &str| u8::from_str_radix(h, 16).map_err(|_| "Invalid hex".to_string());
    let (r, g, b, a) = match hex.len() {
        3 => {
            let r = parse_hex2(&hex[0..1].repeat(2))?;
            let g = parse_hex2(&hex[1..2].repeat(2))?;
            let b = parse_hex2(&hex[2..3].repeat(2))?;
            (r, g, b, 255)
        }
        4 => {
            let r = parse_hex2(&hex[0..1].repeat(2))?;
            let g = parse_hex2(&hex[1..2].repeat(2))?;
            let b = parse_hex2(&hex[2..3].repeat(2))?;
            let a = parse_hex2(&hex[3..4].repeat(2))?;
            (r, g, b, a)
        }
        6 => {
            let r = parse_hex2(&hex[0..2])?;
            let g = parse_hex2(&hex[2..4])?;
            let b = parse_hex2(&hex[4..6])?;
            (r, g, b, 255)
        }
        8 => {
            let r = parse_hex2(&hex[0..2])?;
            let g = parse_hex2(&hex[2..4])?;
            let b = parse_hex2(&hex[4..6])?;
            let a = parse_hex2(&hex[6..8])?;
            (r, g, b, a)
        }
        _ => return Err("Invalid color string; use #RGB, #RGBA, #RRGGBB, or #RRGGBBAA".to_string()),
    };
    Ok([
        r as f32 / 255.0,
        g as f32 / 255.0,
        b as f32 / 255.0,
        a as f32 / 255.0,
    ])
}

fn named_color(name: &str) -> Option<[f32; 4]> {
    let c = |r: u8, g: u8, b: u8| [r as f32 / 255.0, g as f32 / 255.0, b as f32 / 255.0, 1.0];
    match name {
        "red" => Some(c(255, 0, 0)),
        "green" => Some(c(0, 128, 0)),
        "blue" => Some(c(0, 0, 255)),
        "white" => Some(c(255, 255, 255)),
        "black" => Some(c(0, 0, 0)),
        "yellow" => Some(c(255, 255, 0)),
        "magenta" => Some(c(255, 0, 255)),
        "cyan" => Some(c(0, 255, 255)),
        "orange" => Some(c(255, 165, 0)),
        "purple" => Some(c(128, 0, 128)),
        "pink" => Some(c(255, 192, 203)),
        "gray" | "grey" => Some(c(128, 128, 128)),
        _ => None,
    }
}

/// Register all scene intrinsics in an environment
pub fn register_scene_intrinsics() -> HashMap<String, NativeFn> {
    let mut intrinsics = HashMap::new();
    
    // Camera and scene control
    intrinsics.insert("create-camera".to_string(), intrinsic_create_camera());
    
    // Basic primitives
    intrinsics.insert("create-cube".to_string(), intrinsic_create_cube());
    intrinsics.insert("create-sphere".to_string(), intrinsic_create_sphere());
    intrinsics.insert("create-cylinder".to_string(), intrinsic_create_cylinder());
    intrinsics.insert("create-cone".to_string(), intrinsic_create_cone());
    intrinsics.insert("create-pyramid".to_string(), intrinsic_create_pyramid());
    intrinsics.insert("create-wedge".to_string(), intrinsic_create_wedge());
    intrinsics.insert("create-torus".to_string(), intrinsic_create_torus());
    intrinsics.insert("create-plane".to_string(), intrinsic_create_plane());
    intrinsics.insert("create-capsule".to_string(), intrinsic_create_capsule());
    
    // Platonic solids
    intrinsics.insert("create-icosahedron".to_string(), intrinsic_create_icosahedron());
    intrinsics.insert("create-octahedron".to_string(), intrinsic_create_octahedron());
    intrinsics.insert("create-tetrahedron".to_string(), intrinsic_create_tetrahedron());
    
    // Transform operations
    intrinsics.insert("update-transform".to_string(), intrinsic_update_transform());
    intrinsics.insert("rotate".to_string(), intrinsic_rotate());
    intrinsics.insert("scale".to_string(), intrinsic_scale());
    intrinsics.insert("get-position".to_string(), intrinsic_get_position());
    
    // Utility functions
    intrinsics.insert("matrix-multiply".to_string(), intrinsic_matrix_multiply());
    intrinsics.insert("raycast".to_string(), intrinsic_raycast());
    intrinsics.insert("set-color".to_string(), intrinsic_set_color());
    intrinsics.insert("set-material".to_string(), intrinsic_set_material());
    
    intrinsics
}

/// Set material on an object from a map
/// Example map:
///   {"type":"basic", "color":"#ff8844", "animated": false}
///   {"type":"standard", "base_color":[1 0.5 0 1], "metallic":0.2, "roughness":0.8, "emissive":[0 0 0]}
pub fn intrinsic_set_material() -> NativeFn {
    Rc::new(|args| {
        if args.len() != 2 { return Err("set-material expects (object-id, map)".to_string()); }

        let id = match &args[0] { Value::Object(id) => *id, _ => return Err("First argument must be an object ID".to_string()) };
        let mat_val = &args[1];

        let mut to_basic = || Material::default_animated_basic();
        let mut parse_bool = |v: &Value| match v {
            Value::Bool(b) => Ok(*b),
            Value::Str(s) if s.eq_ignore_ascii_case("true") => Ok(true),
            Value::Str(s) if s.eq_ignore_ascii_case("false") => Ok(false),
            _ => Err("Expected boolean".to_string()),
        };

        let material = match mat_val {
            Value::Map(m) => {
                // Helper to get value by key from map supporting both keywords and strings
                let get = |k: &str| m.get(k).or_else(|| m.get(&k.to_string()));
                // Type
                let typ = get("type");
                let tstr = match typ { Some(Value::Str(s)) => s.as_str(), Some(Value::Symbol(sym)) => sym.0.as_str(), _ => "basic" };
                match tstr.to_ascii_lowercase().as_str() {
                    "basic" => {
                        let mut mat = Material::default_animated_basic();
                        if let Some(color_v) = get("color") { mat.set_color(parse_color_value(color_v)?); }
                        if let Some(anim_v) = get("animated") { if let Material::Basic { animated, .. } = &mut mat { *animated = parse_bool(anim_v)?; } }
                        mat
                    }
                    "standard" => {
                        let base_color = if let Some(v) = get("base_color") { parse_color_value(v)? } else { [1.0,1.0,1.0,1.0] };
                        let metallic = if let Some(v) = get("metallic") { number_to_f32(v)? } else { 0.0 };
                        let roughness = if let Some(v) = get("roughness") { number_to_f32(v)? } else { 0.5 };
                        let emissive = if let Some(v) = get("emissive") { parse_vec3_value(v)? } else { [0.0,0.0,0.0] };
                        Material::Standard { base_color, metallic, roughness, emissive }
                    }
                    "lambert" => {
                        let color = if let Some(v) = get("color") { parse_color_value(v)? } else { [1.0,1.0,1.0,1.0] };
                        Material::Lambert { color }
                    }
                    "phong" => {
                        let color = if let Some(v) = get("color") { parse_color_value(v)? } else { [1.0,1.0,1.0,1.0] };
                        let shininess = if let Some(v) = get("shininess") { number_to_f32(v)? } else { 30.0 };
                        let specular = if let Some(v) = get("specular") { parse_vec3_value(v)? } else { [1.0,1.0,1.0] };
                        Material::Phong { color, shininess, specular }
                    }
                    "toon" => {
                        let color = if let Some(v) = get("color") { parse_color_value(v)? } else { [1.0,1.0,1.0,1.0] };
                        let levels = if let Some(v) = get("levels") { number_to_u32(v)? } else { 4 };
                        Material::Toon { color, levels }
                    }
                    "normal" => Material::Normal,
                    _ => to_basic(),
                }
            }
            _ => to_basic(),
        };

        SCENE.with(|scene| {
            let mut scene = scene.borrow_mut();
            if let Some(node) = scene.nodes.get_mut(&id) {
                node.material = material;
                Ok(Value::Nil)
            } else {
                Err(format!("Object {:?} not found", id))
            }
        })
    })
}

fn number_to_f32(v: &Value) -> Result<f32, String> {
    match v {
        Value::Float(f) => Ok(*f as f32),
        Value::Int(i) => Ok(*i as f32),
        _ => Err("Expected number".to_string()),
    }
}

fn number_to_u32(v: &Value) -> Result<u32, String> {
    match v {
        Value::Int(i) if *i >= 0 => Ok(*i as u32),
        Value::Float(f) if *f >= 0.0 => Ok(*f as u32),
        _ => Err("Expected non-negative number".to_string()),
    }
}

fn parse_vec3_value(v: &Value) -> Result<[f32; 3], String> {
    match v {
        Value::Vector(items) if items.len() == 3 => Ok([
            number_to_f32(&items[0])?,
            number_to_f32(&items[1])?,
            number_to_f32(&items[2])?,
        ]),
        _ => Err("Expected 3-element vector".to_string()),
    }
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
