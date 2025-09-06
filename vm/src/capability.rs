use std::collections::HashMap;
use std::sync::Arc;
use crate::value::Value;

pub trait Capability: Send + Sync {
    fn name(&self) -> &str;
    fn version(&self) -> &str;
    fn call(&self, method: &str, args: Vec<Value>) -> Result<Value, String>;
}

pub struct CapabilityTable {
    capabilities: HashMap<String, Arc<dyn Capability>>,
    platform_info: PlatformInfo,
}

#[derive(Debug, Clone)]
pub struct PlatformInfo {
    pub name: String,
    pub version: String,
    pub vendor: String,
    pub device_type: DeviceType,
    pub display_info: DisplayInfo,
    pub input_capabilities: Vec<InputCapability>,
}

#[derive(Debug, Clone, Copy)]
pub enum DeviceType {
    HeadMountedDisplay,
    HandheldAR,
    Desktop,
    Mobile,
    Web,
}

#[derive(Debug, Clone)]
pub struct DisplayInfo {
    pub resolution: (u32, u32),
    pub refresh_rate: f32,
    pub fov: Option<f32>,
    pub ipd: Option<f32>,
}

#[derive(Debug, Clone, Copy)]
pub enum InputCapability {
    Controllers,
    HandTracking,
    EyeTracking,
    VoiceInput,
    TouchScreen,
    MouseKeyboard,
    Gesture,
}

impl CapabilityTable {
    pub fn new(platform_info: PlatformInfo) -> Self {
        Self {
            capabilities: HashMap::new(),
            platform_info,
        }
    }

    pub fn register(&mut self, capability: Arc<dyn Capability>) {
        self.capabilities.insert(capability.name().to_string(), capability);
    }

    pub fn get(&self, name: &str) -> Option<Arc<dyn Capability>> {
        self.capabilities.get(name).cloned()
    }

    pub fn has(&self, name: &str) -> bool {
        self.capabilities.contains_key(name)
    }

    pub fn list(&self) -> Vec<String> {
        self.capabilities.keys().cloned().collect()
    }

    pub fn platform_info(&self) -> &PlatformInfo {
        &self.platform_info
    }

    pub fn call(&self, capability: &str, method: &str, args: Vec<Value>) -> Result<Value, String> {
        self.get(capability)
            .ok_or_else(|| format!("Capability '{}' not found", capability))?
            .call(method, args)
    }
}

pub struct XRTrackingCapability {
    version: String,
}

impl XRTrackingCapability {
    pub fn new() -> Self {
        Self {
            version: "1.0.0".to_string(),
        }
    }
}

impl Capability for XRTrackingCapability {
    fn name(&self) -> &str {
        "xr.tracking"
    }

    fn version(&self) -> &str {
        &self.version
    }

    fn call(&self, method: &str, args: Vec<Value>) -> Result<Value, String> {
        match method {
            "get_head_pose" => {
                Ok(Value::Map({
                    let mut map = HashMap::new();
                    map.insert("position".to_string(), Value::Vector(vec![
                        Value::Float(0.0),
                        Value::Float(1.6),
                        Value::Float(0.0),
                    ]));
                    map.insert("rotation".to_string(), Value::Vector(vec![
                        Value::Float(0.0),
                        Value::Float(0.0),
                        Value::Float(0.0),
                        Value::Float(1.0),
                    ]));
                    map
                }))
            }
            "get_controller_pose" => {
                let hand = args.get(0)
                    .and_then(|v| if let Value::Str(s) = v { Some(s.as_str()) } else { None })
                    .unwrap_or("left");
                
                Ok(Value::Map({
                    let mut map = HashMap::new();
                    map.insert("hand".to_string(), Value::Str(hand.to_string()));
                    map.insert("position".to_string(), Value::Vector(vec![
                        Value::Float(if hand == "left" { -0.3 } else { 0.3 }),
                        Value::Float(1.2),
                        Value::Float(-0.5),
                    ]));
                    map
                }))
            }
            _ => Err(format!("Unknown method '{}' for xr.tracking", method)),
        }
    }
}

pub struct SceneCapability {
    version: String,
}

impl SceneCapability {
    pub fn new() -> Self {
        Self {
            version: "1.0.0".to_string(),
        }
    }
}

impl Capability for SceneCapability {
    fn name(&self) -> &str {
        "scene"
    }

    fn version(&self) -> &str {
        &self.version
    }

    fn call(&self, method: &str, args: Vec<Value>) -> Result<Value, String> {
        match method {
            "create_cube" => {
                let size = args.get(0)
                    .and_then(|v| if let Value::Float(f) = v { Some(*f) } else { None })
                    .unwrap_or(1.0);
                
                Ok(Value::Map({
                    let mut map = HashMap::new();
                    map.insert("type".to_string(), Value::Str("cube".to_string()));
                    map.insert("id".to_string(), Value::Str(format!("cube_{}", rand::random::<u32>())));
                    map.insert("size".to_string(), Value::Float(size));
                    map
                }))
            }
            "create_sphere" => {
                let radius = args.get(0)
                    .and_then(|v| if let Value::Float(f) = v { Some(*f) } else { None })
                    .unwrap_or(0.5);
                
                Ok(Value::Map({
                    let mut map = HashMap::new();
                    map.insert("type".to_string(), Value::Str("sphere".to_string()));
                    map.insert("id".to_string(), Value::Str(format!("sphere_{}", rand::random::<u32>())));
                    map.insert("radius".to_string(), Value::Float(radius));
                    map
                }))
            }
            "update_transform" => {
                if args.len() < 2 {
                    return Err("update_transform requires entity_id and transform".to_string());
                }
                Ok(Value::Bool(true))
            }
            _ => Err(format!("Unknown method '{}' for scene", method)),
        }
    }
}

pub struct NetworkCapability {
    version: String,
}

impl NetworkCapability {
    pub fn new() -> Self {
        Self {
            version: "1.0.0".to_string(),
        }
    }
}

impl Capability for NetworkCapability {
    fn name(&self) -> &str {
        "network"
    }

    fn version(&self) -> &str {
        &self.version
    }

    fn call(&self, method: &str, args: Vec<Value>) -> Result<Value, String> {
        match method {
            "fetch" => {
                let url = args.get(0)
                    .and_then(|v| if let Value::Str(s) = v { Some(s.clone()) } else { None })
                    .ok_or("fetch requires a URL string")?;
                
                Ok(Value::Map({
                    let mut map = HashMap::new();
                    map.insert("status".to_string(), Value::Int(200));
                    map.insert("url".to_string(), Value::Str(url));
                    map.insert("body".to_string(), Value::Str("Mock response".to_string()));
                    map
                }))
            }
            "connect_websocket" => {
                let url = args.get(0)
                    .and_then(|v| if let Value::Str(s) = v { Some(s.clone()) } else { None })
                    .ok_or("connect_websocket requires a URL string")?;
                
                Ok(Value::Map({
                    let mut map = HashMap::new();
                    map.insert("connected".to_string(), Value::Bool(true));
                    map.insert("url".to_string(), Value::Str(url));
                    map.insert("id".to_string(), Value::Str(format!("ws_{}", rand::random::<u32>())));
                    map
                }))
            }
            _ => Err(format!("Unknown method '{}' for network", method)),
        }
    }
}

pub fn create_platform_capabilities(device_type: DeviceType) -> CapabilityTable {
    let platform_info = match device_type {
        DeviceType::HeadMountedDisplay => PlatformInfo {
            name: "Meta Quest 3".to_string(),
            version: "1.0.0".to_string(),
            vendor: "Meta".to_string(),
            device_type,
            display_info: DisplayInfo {
                resolution: (2064, 2208),
                refresh_rate: 120.0,
                fov: Some(110.0),
                ipd: Some(63.0),
            },
            input_capabilities: vec![
                InputCapability::Controllers,
                InputCapability::HandTracking,
                InputCapability::EyeTracking,
                InputCapability::VoiceInput,
            ],
        },
        DeviceType::Desktop => PlatformInfo {
            name: "Desktop".to_string(),
            version: "1.0.0".to_string(),
            vendor: "Generic".to_string(),
            device_type,
            display_info: DisplayInfo {
                resolution: (1920, 1080),
                refresh_rate: 60.0,
                fov: None,
                ipd: None,
            },
            input_capabilities: vec![
                InputCapability::MouseKeyboard,
            ],
        },
        DeviceType::Web => PlatformInfo {
            name: "WebXR".to_string(),
            version: "1.0.0".to_string(),
            vendor: "Browser".to_string(),
            device_type,
            display_info: DisplayInfo {
                resolution: (1920, 1080),
                refresh_rate: 60.0,
                fov: None,
                ipd: None,
            },
            input_capabilities: vec![
                InputCapability::MouseKeyboard,
                InputCapability::TouchScreen,
            ],
        },
        _ => PlatformInfo {
            name: "Unknown".to_string(),
            version: "1.0.0".to_string(),
            vendor: "Unknown".to_string(),
            device_type,
            display_info: DisplayInfo {
                resolution: (1920, 1080),
                refresh_rate: 60.0,
                fov: None,
                ipd: None,
            },
            input_capabilities: vec![],
        },
    };

    let mut table = CapabilityTable::new(platform_info);
    
    table.register(Arc::new(SceneCapability::new()));
    table.register(Arc::new(NetworkCapability::new()));
    
    if matches!(device_type, DeviceType::HeadMountedDisplay | DeviceType::HandheldAR) {
        table.register(Arc::new(XRTrackingCapability::new()));
    }
    
    table
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_capability_registration() {
        let mut table = CapabilityTable::new(PlatformInfo {
            name: "Test".to_string(),
            version: "1.0.0".to_string(),
            vendor: "Test".to_string(),
            device_type: DeviceType::Desktop,
            display_info: DisplayInfo {
                resolution: (1920, 1080),
                refresh_rate: 60.0,
                fov: None,
                ipd: None,
            },
            input_capabilities: vec![],
        });

        table.register(Arc::new(SceneCapability::new()));
        assert!(table.has("scene"));
        assert!(!table.has("nonexistent"));
    }

    #[test]
    fn test_capability_call() {
        let table = create_platform_capabilities(DeviceType::Desktop);
        
        let result = table.call("scene", "create_cube", vec![Value::Float(2.0)]).unwrap();
        if let Value::Map(map) = result {
            assert_eq!(map.get("type"), Some(&Value::Str("cube".to_string())));
            assert_eq!(map.get("size"), Some(&Value::Float(2.0)));
        } else {
            panic!("Expected Map result");
        }
    }

    #[test]
    fn test_platform_specific_capabilities() {
        let vr_table = create_platform_capabilities(DeviceType::HeadMountedDisplay);
        assert!(vr_table.has("xr.tracking"));
        assert!(vr_table.has("scene"));

        let desktop_table = create_platform_capabilities(DeviceType::Desktop);
        assert!(!desktop_table.has("xr.tracking"));
        assert!(desktop_table.has("scene"));
    }
}