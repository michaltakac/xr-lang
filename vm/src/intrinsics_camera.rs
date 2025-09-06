//! Enhanced Camera Intrinsics for XR-Lang
//! Provides flexible camera creation and manipulation functions

use crate::value::{Value, ObjectId, NativeFn};
use crate::intrinsics::{Vec3, Camera, SCENE};
use std::rc::Rc;
use std::collections::HashMap;

/// Parse keyword arguments from a list
fn parse_kwargs(args: &[Value]) -> Result<HashMap<String, Value>, String> {
    let mut kwargs = HashMap::new();
    let mut i = 0;
    
    while i < args.len() {
        // Check for keyword (symbol starting with :)
        if let Value::Keyword(ref k) = args[i] {
            if i + 1 >= args.len() {
                return Err(format!("Keyword {} missing value", k.0));
            }
            kwargs.insert(k.0.clone(), args[i + 1].clone());
            i += 2;
        } else {
            return Err(format!("Expected keyword argument at position {}, got {:?}", i, args[i]));
        }
    }
    
    Ok(kwargs)
}

/// Create a perspective camera with keyword arguments
/// (create-perspective-camera 
///   :position [x y z]
///   :target [x y z]       ; optional, default [0 0 0]
///   :fov 75              ; optional, default 60
///   :up [0 1 0]          ; optional, default [0 1 0]
///   :near 0.1            ; optional, default 0.1
///   :far 1000)           ; optional, default 1000
pub fn intrinsic_create_perspective_camera() -> NativeFn {
    Rc::new(|args| {
        let kwargs = parse_kwargs(args)?;
        
        // Required: position
        let position = kwargs.get("position")
            .ok_or("create-perspective-camera requires :position")?;
        let position = Vec3::from_value(position)?;
        
        // Optional: target
        let target = if let Some(t) = kwargs.get("target") {
            Vec3::from_value(t)?
        } else {
            Vec3::new(0.0, 0.0, 0.0)
        };
        
        // Optional: fov
        let fov = if let Some(f) = kwargs.get("fov") {
            match f {
                Value::Float(v) => *v as f32,
                Value::Int(v) => *v as f32,
                _ => return Err("FOV must be a number".to_string()),
            }
        } else {
            60.0
        };
        
        // Optional: up vector
        let _up = if let Some(u) = kwargs.get("up") {
            Vec3::from_value(u)?
        } else {
            Vec3::new(0.0, 1.0, 0.0)
        };
        
        // Optional: near/far planes
        let _near = if let Some(n) = kwargs.get("near") {
            match n {
                Value::Float(v) => *v as f32,
                Value::Int(v) => *v as f32,
                _ => 0.1,
            }
        } else {
            0.1
        };
        
        let _far = if let Some(f) = kwargs.get("far") {
            match f {
                Value::Float(v) => *v as f32,
                Value::Int(v) => *v as f32,
                _ => 1000.0,
            }
        } else {
            1000.0
        };
        
        // Create camera in scene
        SCENE.with(|scene| {
            let mut scene = scene.borrow_mut();
            let id = scene.generate_id();
            
            let camera = Camera {
                position,
                target,
                fov,
            };
            
            scene.cameras.insert(id, camera);
            Ok(Value::Object(id))
        })
    })
}

/// Create an orthographic camera
/// (create-orthographic-camera
///   :position [x y z]
///   :target [x y z]
///   :size 10             ; or :left/:right/:top/:bottom
///   :near 0.1
///   :far 100)
pub fn intrinsic_create_orthographic_camera() -> NativeFn {
    Rc::new(|args| {
        let kwargs = parse_kwargs(args)?;
        
        let position = kwargs.get("position")
            .ok_or("create-orthographic-camera requires :position")?;
        let position = Vec3::from_value(position)?;
        
        let target = if let Some(t) = kwargs.get("target") {
            Vec3::from_value(t)?
        } else {
            Vec3::new(0.0, 0.0, 0.0)
        };
        
        // For now, create as regular camera (orthographic projection would be in renderer)
        SCENE.with(|scene| {
            let mut scene = scene.borrow_mut();
            let id = scene.generate_id();
            
            let camera = Camera {
                position,
                target,
                fov: 0.0, // 0 indicates orthographic
            };
            
            scene.cameras.insert(id, camera);
            Ok(Value::Object(id))
        })
    })
}

/// Make camera look at a specific point
/// (look-at camera-id [x y z])
pub fn intrinsic_look_at() -> NativeFn {
    Rc::new(|args| {
        if args.len() != 2 {
            return Err("look-at expects 2 arguments (camera-id, target)".to_string());
        }
        
        let id = match &args[0] {
            Value::Object(id) => *id,
            _ => return Err("First argument must be a camera ID".to_string()),
        };
        
        let target = Vec3::from_value(&args[1])?;
        
        SCENE.with(|scene| {
            let mut scene = scene.borrow_mut();
            
            if let Some(camera) = scene.cameras.get_mut(&id) {
                camera.target = target;
                Ok(Value::Nil)
            } else {
                Err(format!("Camera {:?} not found", id))
            }
        })
    })
}

/// Create an orbit camera that circles around a target
/// (orbit-camera
///   :target [0 0 0]
///   :radius 10
///   :theta 45           ; horizontal angle
///   :phi 30)            ; vertical angle
pub fn intrinsic_orbit_camera() -> NativeFn {
    Rc::new(|args| {
        let kwargs = parse_kwargs(args)?;
        
        let target = if let Some(t) = kwargs.get("target") {
            Vec3::from_value(t)?
        } else {
            Vec3::new(0.0, 0.0, 0.0)
        };
        
        let radius = if let Some(r) = kwargs.get("radius") {
            match r {
                Value::Float(v) => *v as f32,
                Value::Int(v) => *v as f32,
                _ => 10.0,
            }
        } else {
            10.0
        };
        
        let theta = if let Some(t) = kwargs.get("theta") {
            match t {
                Value::Float(v) => (*v as f32).to_radians(),
                Value::Int(v) => (*v as f32).to_radians(),
                _ => 0.0,
            }
        } else {
            0.0
        };
        
        let phi = if let Some(p) = kwargs.get("phi") {
            match p {
                Value::Float(v) => (*v as f32).to_radians(),
                Value::Int(v) => (*v as f32).to_radians(),
                _ => std::f32::consts::FRAC_PI_4,
            }
        } else {
            std::f32::consts::FRAC_PI_4
        };
        
        // Calculate position from spherical coordinates
        let x = radius * phi.sin() * theta.cos();
        let y = radius * phi.cos();
        let z = radius * phi.sin() * theta.sin();
        let position = Vec3::new(x, y, z) + target;
        
        SCENE.with(|scene| {
            let mut scene = scene.borrow_mut();
            let id = scene.generate_id();
            
            let camera = Camera {
                position,
                target,
                fov: 60.0,
            };
            
            scene.cameras.insert(id, camera);
            Ok(Value::Object(id))
        })
    })
}

/// Set camera field of view
/// (set-fov camera-id degrees)
pub fn intrinsic_set_fov() -> NativeFn {
    Rc::new(|args| {
        if args.len() != 2 {
            return Err("set-fov expects 2 arguments (camera-id, degrees)".to_string());
        }
        
        let id = match &args[0] {
            Value::Object(id) => *id,
            _ => return Err("First argument must be a camera ID".to_string()),
        };
        
        let fov = match &args[1] {
            Value::Float(f) => *f as f32,
            Value::Int(i) => *i as f32,
            _ => return Err("FOV must be a number".to_string()),
        };
        
        SCENE.with(|scene| {
            let mut scene = scene.borrow_mut();
            
            if let Some(camera) = scene.cameras.get_mut(&id) {
                camera.fov = fov;
                Ok(Value::Nil)
            } else {
                Err(format!("Camera {:?} not found", id))
            }
        })
    })
}

/// Create a first-person camera
/// (create-fps-camera
///   :position [x y z]
///   :yaw 0
///   :pitch 0
///   :roll 0)
pub fn intrinsic_create_fps_camera() -> NativeFn {
    Rc::new(|args| {
        let kwargs = parse_kwargs(args)?;
        
        let position = kwargs.get("position")
            .ok_or("create-fps-camera requires :position")?;
        let position = Vec3::from_value(position)?;
        
        let yaw = if let Some(y) = kwargs.get("yaw") {
            match y {
                Value::Float(v) => (*v as f32).to_radians(),
                Value::Int(v) => (*v as f32).to_radians(),
                _ => 0.0,
            }
        } else {
            0.0
        };
        
        let pitch = if let Some(p) = kwargs.get("pitch") {
            match p {
                Value::Float(v) => (*v as f32).to_radians(),
                Value::Int(v) => (*v as f32).to_radians(),
                _ => 0.0,
            }
        } else {
            0.0
        };
        
        // Calculate target from yaw and pitch
        let target_x = position.x + yaw.cos() * pitch.cos();
        let target_y = position.y + pitch.sin();
        let target_z = position.z + yaw.sin() * pitch.cos();
        let target = Vec3::new(target_x, target_y, target_z);
        
        SCENE.with(|scene| {
            let mut scene = scene.borrow_mut();
            let id = scene.generate_id();
            
            let camera = Camera {
                position,
                target,
                fov: 75.0, // Typical FPS FOV
            };
            
            scene.cameras.insert(id, camera);
            Ok(Value::Object(id))
        })
    })
}

/// Create a third-person follow camera
/// (create-follow-camera
///   :target entity-id
///   :offset [x y z]
///   :damping 0.1)
pub fn intrinsic_create_follow_camera() -> NativeFn {
    Rc::new(|args| {
        let kwargs = parse_kwargs(args)?;
        
        let target_id = match kwargs.get("target") {
            Some(Value::Object(id)) => *id,
            _ => return Err("create-follow-camera requires :target entity".to_string()),
        };
        
        let offset = if let Some(o) = kwargs.get("offset") {
            Vec3::from_value(o)?
        } else {
            Vec3::new(0.0, 5.0, -10.0) // Default third-person offset
        };
        
        // Get target position
        let target_pos = SCENE.with(|scene| {
            let scene = scene.borrow();
            scene.nodes.get(&target_id)
                .map(|node| node.transform.position.clone())
                .unwrap_or(Vec3::new(0.0, 0.0, 0.0))
        });
        
        let position = target_pos + offset;
        
        SCENE.with(|scene| {
            let mut scene = scene.borrow_mut();
            let id = scene.generate_id();
            
            let camera = Camera {
                position,
                target: target_pos,
                fov: 60.0,
            };
            
            scene.cameras.insert(id, camera);
            Ok(Value::Object(id))
        })
    })
}

/// Create a cinematic camera with depth of field
/// (create-cinematic-camera
///   :position [x y z]
///   :target [x y z]
///   :fov 35
///   :aperture 1.4
///   :focus-distance 15)
pub fn intrinsic_create_cinematic_camera() -> NativeFn {
    Rc::new(|args| {
        let kwargs = parse_kwargs(args)?;
        
        let position = kwargs.get("position")
            .ok_or("create-cinematic-camera requires :position")?;
        let position = Vec3::from_value(position)?;
        
        let target = if let Some(t) = kwargs.get("target") {
            Vec3::from_value(t)?
        } else {
            Vec3::new(0.0, 0.0, 0.0)
        };
        
        let fov = if let Some(f) = kwargs.get("fov") {
            match f {
                Value::Float(v) => *v as f32,
                Value::Int(v) => *v as f32,
                _ => 35.0,
            }
        } else {
            35.0 // Narrower FOV for cinematic look
        };
        
        // Note: aperture and focus-distance would be handled by renderer
        // Store them as metadata for now
        
        SCENE.with(|scene| {
            let mut scene = scene.borrow_mut();
            let id = scene.generate_id();
            
            let camera = Camera {
                position,
                target,
                fov,
            };
            
            scene.cameras.insert(id, camera);
            Ok(Value::Object(id))
        })
    })
}

/// Move camera by relative offset
/// (move-camera camera-id [dx dy dz])
pub fn intrinsic_move_camera() -> NativeFn {
    Rc::new(|args| {
        if args.len() != 2 {
            return Err("move-camera expects 2 arguments (camera-id, offset)".to_string());
        }
        
        let id = match &args[0] {
            Value::Object(id) => *id,
            _ => return Err("First argument must be a camera ID".to_string()),
        };
        
        let offset = Vec3::from_value(&args[1])?;
        
        SCENE.with(|scene| {
            let mut scene = scene.borrow_mut();
            
            if let Some(camera) = scene.cameras.get_mut(&id) {
                camera.position = camera.position + offset;
                camera.target = camera.target + offset; // Move target too to maintain look direction
                Ok(Value::Nil)
            } else {
                Err(format!("Camera {:?} not found", id))
            }
        })
    })
}

/// Rotate camera by euler angles
/// (rotate-camera camera-id [rx ry rz])
pub fn intrinsic_rotate_camera() -> NativeFn {
    Rc::new(|args| {
        if args.len() != 2 {
            return Err("rotate-camera expects 2 arguments (camera-id, rotation)".to_string());
        }
        
        let id = match &args[0] {
            Value::Object(id) => *id,
            _ => return Err("First argument must be a camera ID".to_string()),
        };
        
        let rotation = Vec3::from_value(&args[1])?;
        
        SCENE.with(|scene| {
            let mut scene = scene.borrow_mut();
            
            if let Some(camera) = scene.cameras.get_mut(&id) {
                // Simple rotation: rotate target around camera position
                let dir = camera.target - camera.position;
                let distance = dir.length();
                
                // Apply rotation (simplified - would need proper matrix math in production)
                let yaw = rotation.y.to_radians();
                let pitch = rotation.x.to_radians();
                
                let new_x = distance * yaw.cos() * pitch.cos();
                let new_y = distance * pitch.sin();
                let new_z = distance * yaw.sin() * pitch.cos();
                
                camera.target = camera.position + Vec3::new(new_x, new_y, new_z);
                Ok(Value::Nil)
            } else {
                Err(format!("Camera {:?} not found", id))
            }
        })
    })
}

/// Zoom camera (adjust FOV)
/// (zoom-camera camera-id factor)
pub fn intrinsic_zoom_camera() -> NativeFn {
    Rc::new(|args| {
        if args.len() != 2 {
            return Err("zoom-camera expects 2 arguments (camera-id, factor)".to_string());
        }
        
        let id = match &args[0] {
            Value::Object(id) => *id,
            _ => return Err("First argument must be a camera ID".to_string()),
        };
        
        let factor = match &args[1] {
            Value::Float(f) => *f as f32,
            Value::Int(i) => *i as f32,
            _ => return Err("Zoom factor must be a number".to_string()),
        };
        
        SCENE.with(|scene| {
            let mut scene = scene.borrow_mut();
            
            if let Some(camera) = scene.cameras.get_mut(&id) {
                camera.fov = (camera.fov / factor).clamp(10.0, 120.0);
                Ok(Value::Nil)
            } else {
                Err(format!("Camera {:?} not found", id))
            }
        })
    })
}

/// Get camera position
/// (get-camera-position camera-id)
pub fn intrinsic_get_camera_position() -> NativeFn {
    Rc::new(|args| {
        if args.len() != 1 {
            return Err("get-camera-position expects 1 argument (camera-id)".to_string());
        }
        
        let id = match &args[0] {
            Value::Object(id) => *id,
            _ => return Err("Argument must be a camera ID".to_string()),
        };
        
        SCENE.with(|scene| {
            let scene = scene.borrow();
            
            if let Some(camera) = scene.cameras.get(&id) {
                Ok(Value::Vector(vec![
                    Value::Float(camera.position.x as f64),
                    Value::Float(camera.position.y as f64),
                    Value::Float(camera.position.z as f64),
                ]))
            } else {
                Err(format!("Camera {:?} not found", id))
            }
        })
    })
}

/// Get camera target
/// (get-camera-target camera-id)
pub fn intrinsic_get_camera_target() -> NativeFn {
    Rc::new(|args| {
        if args.len() != 1 {
            return Err("get-camera-target expects 1 argument (camera-id)".to_string());
        }
        
        let id = match &args[0] {
            Value::Object(id) => *id,
            _ => return Err("Argument must be a camera ID".to_string()),
        };
        
        SCENE.with(|scene| {
            let scene = scene.borrow();
            
            if let Some(camera) = scene.cameras.get(&id) {
                Ok(Value::Vector(vec![
                    Value::Float(camera.target.x as f64),
                    Value::Float(camera.target.y as f64),
                    Value::Float(camera.target.z as f64),
                ]))
            } else {
                Err(format!("Camera {:?} not found", id))
            }
        })
    })
}

/// Get camera FOV
/// (get-camera-fov camera-id)
pub fn intrinsic_get_camera_fov() -> NativeFn {
    Rc::new(|args| {
        if args.len() != 1 {
            return Err("get-camera-fov expects 1 argument (camera-id)".to_string());
        }
        
        let id = match &args[0] {
            Value::Object(id) => *id,
            _ => return Err("Argument must be a camera ID".to_string()),
        };
        
        SCENE.with(|scene| {
            let scene = scene.borrow();
            
            if let Some(camera) = scene.cameras.get(&id) {
                Ok(Value::Float(camera.fov as f64))
            } else {
                Err(format!("Camera {:?} not found", id))
            }
        })
    })
}

/// Set the active camera
/// (set-active-camera camera-id)
pub fn intrinsic_set_active_camera() -> NativeFn {
    Rc::new(|args| {
        if args.len() != 1 {
            return Err("set-active-camera expects 1 argument (camera-id)".to_string());
        }
        
        let id = match &args[0] {
            Value::Object(id) => *id,
            _ => return Err("Argument must be a camera ID".to_string()),
        };
        
        SCENE.with(|scene| {
            let mut scene = scene.borrow_mut();
            
            if scene.cameras.contains_key(&id) {
                scene.active_camera = Some(id);
                Ok(Value::Nil)
            } else {
                Err(format!("Camera {:?} not found", id))
            }
        })
    })
}

/// Cycle through all cameras
/// (cycle-cameras)
pub fn intrinsic_cycle_cameras() -> NativeFn {
    Rc::new(|_args| {
        SCENE.with(|scene| {
            let mut scene = scene.borrow_mut();
            
            if scene.cameras.is_empty() {
                return Err("No cameras in scene".to_string());
            }
            
            let camera_ids: Vec<ObjectId> = scene.cameras.keys().cloned().collect();
            
            let next_id = if let Some(current) = scene.active_camera {
                // Find current index and get next
                if let Some(idx) = camera_ids.iter().position(|&id| id == current) {
                    camera_ids[(idx + 1) % camera_ids.len()]
                } else {
                    camera_ids[0]
                }
            } else {
                camera_ids[0]
            };
            
            scene.active_camera = Some(next_id);
            Ok(Value::Object(next_id))
        })
    })
}

/// Shake camera effect
/// (shake-camera camera-id intensity duration)
pub fn intrinsic_shake_camera() -> NativeFn {
    Rc::new(|args| {
        if args.len() != 3 {
            return Err("shake-camera expects 3 arguments (camera-id, intensity, duration)".to_string());
        }
        
        let id = match &args[0] {
            Value::Object(id) => *id,
            _ => return Err("First argument must be a camera ID".to_string()),
        };
        
        let _intensity = match &args[1] {
            Value::Float(f) => *f as f32,
            Value::Int(i) => *i as f32,
            _ => return Err("Intensity must be a number".to_string()),
        };
        
        let _duration = match &args[2] {
            Value::Float(f) => *f as f32,
            Value::Int(i) => *i as f32,
            _ => return Err("Duration must be a number".to_string()),
        };
        
        // Note: Actual shake implementation would require animation system
        // For now, just validate camera exists
        SCENE.with(|scene| {
            let scene = scene.borrow();
            if scene.cameras.contains_key(&id) {
                Ok(Value::Nil) // Would trigger shake animation
            } else {
                Err(format!("Camera {:?} not found", id))
            }
        })
    })
}

/// Register enhanced camera intrinsics
pub fn register_camera_intrinsics() -> HashMap<String, NativeFn> {
    let mut intrinsics = HashMap::new();
    
    // Camera creation
    intrinsics.insert("create-perspective-camera".to_string(), intrinsic_create_perspective_camera());
    intrinsics.insert("create-orthographic-camera".to_string(), intrinsic_create_orthographic_camera());
    intrinsics.insert("create-fps-camera".to_string(), intrinsic_create_fps_camera());
    intrinsics.insert("create-follow-camera".to_string(), intrinsic_create_follow_camera());
    intrinsics.insert("create-cinematic-camera".to_string(), intrinsic_create_cinematic_camera());
    intrinsics.insert("orbit-camera".to_string(), intrinsic_orbit_camera());
    
    // Camera manipulation
    intrinsics.insert("look-at".to_string(), intrinsic_look_at());
    intrinsics.insert("move-camera".to_string(), intrinsic_move_camera());
    intrinsics.insert("rotate-camera".to_string(), intrinsic_rotate_camera());
    intrinsics.insert("zoom-camera".to_string(), intrinsic_zoom_camera());
    intrinsics.insert("set-fov".to_string(), intrinsic_set_fov());
    intrinsics.insert("shake-camera".to_string(), intrinsic_shake_camera());
    
    // Camera utilities
    intrinsics.insert("get-camera-position".to_string(), intrinsic_get_camera_position());
    intrinsics.insert("get-camera-target".to_string(), intrinsic_get_camera_target());
    intrinsics.insert("get-camera-fov".to_string(), intrinsic_get_camera_fov());
    
    // Camera switching
    intrinsics.insert("set-active-camera".to_string(), intrinsic_set_active_camera());
    intrinsics.insert("cycle-cameras".to_string(), intrinsic_cycle_cameras());
    
    intrinsics
}