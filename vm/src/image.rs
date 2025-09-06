use std::collections::HashMap;
use serde::{Serialize, Deserialize};
use crate::bytecode::ByteCode;
use crate::persistence::PersistenceLayer;

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct XRLangImage {
    pub version: String,
    pub created_at: u64,
    pub platform: Platform,
    pub bytecode: Vec<u8>,  // Serialized bytecode
    pub globals: HashMap<String, Vec<u8>>,
    pub metadata: ImageMetadata,
    pub capabilities_required: Vec<String>,
    pub snapshot_data: Option<Vec<u8>>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ImageMetadata {
    pub name: String,
    pub author: String,
    pub description: String,
    pub xr_lang_version: String,
    pub dependencies: Vec<Dependency>,
    pub entry_point: String,
    pub checksum: String,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Dependency {
    pub name: String,
    pub version: String,
    pub capabilities: Vec<String>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub enum Platform {
    Universal,
    MetaQuest,
    AppleVisionPro,
    MagicLeap,
    HoloLens,
    WebXR,
    Desktop,
    Mobile,
}

impl XRLangImage {
    pub fn new(name: String, bytecode: Vec<ByteCode>) -> Self {
        // Serialize the bytecode for storage
        let serialized_bytecode = Self::serialize_bytecode(&bytecode);
        
        Self {
            version: "1.0.0".to_string(),
            created_at: std::time::SystemTime::now()
                .duration_since(std::time::UNIX_EPOCH)
                .unwrap()
                .as_secs(),
            platform: Platform::Universal,
            bytecode: serialized_bytecode,
            globals: HashMap::new(),
            metadata: ImageMetadata {
                name,
                author: "XR-Lang".to_string(),
                description: String::new(),
                xr_lang_version: env!("CARGO_PKG_VERSION").to_string(),
                dependencies: Vec::new(),
                entry_point: "main".to_string(),
                checksum: String::new(),
            },
            capabilities_required: Vec::new(),
            snapshot_data: None,
        }
    }
    
    fn serialize_bytecode(bytecode: &[ByteCode]) -> Vec<u8> {
        // For now, use a simple format - this would be replaced with proper serialization
        let json = serde_json::to_string(&format!("{:?}", bytecode)).unwrap_or_default();
        json.into_bytes()
    }
    
    pub fn deserialize_bytecode(&self) -> Result<Vec<ByteCode>, String> {
        // For now, return empty vec - this would be replaced with proper deserialization
        Ok(Vec::new())
    }

    pub fn with_snapshot(mut self, persistence: &PersistenceLayer) -> Self {
        if let Ok(snapshot_data) = persistence.export_snapshot() {
            self.snapshot_data = Some(snapshot_data);
        }
        self
    }

    pub fn add_capability(&mut self, capability: String) {
        if !self.capabilities_required.contains(&capability) {
            self.capabilities_required.push(capability);
        }
    }

    pub fn add_global(&mut self, name: String, data: Vec<u8>) {
        self.globals.insert(name, data);
    }

    pub fn serialize(&self) -> Result<Vec<u8>, String> {
        bincode::serialize(self).map_err(|e| format!("Failed to serialize image: {}", e))
    }

    pub fn deserialize(data: &[u8]) -> Result<Self, String> {
        bincode::deserialize(data).map_err(|e| format!("Failed to deserialize image: {}", e))
    }

    pub fn calculate_checksum(&mut self) {
        use sha2::{Sha256, Digest};
        let mut hasher = Sha256::new();
        hasher.update(&self.version);
        hasher.update(self.created_at.to_le_bytes());
        hasher.update(&self.bytecode);
        self.metadata.checksum = format!("{:x}", hasher.finalize());
    }

    pub fn validate_platform(&self, target: Platform) -> bool {
        self.platform == Platform::Universal || self.platform == target
    }

    pub fn validate_capabilities(&self, available: &[String]) -> Result<(), Vec<String>> {
        let missing: Vec<String> = self.capabilities_required
            .iter()
            .filter(|cap| !available.contains(cap))
            .cloned()
            .collect();
        
        if missing.is_empty() {
            Ok(())
        } else {
            Err(missing)
        }
    }
}

pub struct ImageBuilder {
    image: XRLangImage,
}

impl ImageBuilder {
    pub fn new(name: String) -> Self {
        Self {
            image: XRLangImage::new(name, Vec::new()),
        }
    }

    pub fn with_bytecode(mut self, bytecode: Vec<ByteCode>) -> Self {
        self.image.bytecode = XRLangImage::serialize_bytecode(&bytecode);
        self
    }

    pub fn with_platform(mut self, platform: Platform) -> Self {
        self.image.platform = platform;
        self
    }

    pub fn with_author(mut self, author: String) -> Self {
        self.image.metadata.author = author;
        self
    }

    pub fn with_description(mut self, description: String) -> Self {
        self.image.metadata.description = description;
        self
    }

    pub fn with_entry_point(mut self, entry_point: String) -> Self {
        self.image.metadata.entry_point = entry_point;
        self
    }

    pub fn add_dependency(mut self, dep: Dependency) -> Self {
        self.image.metadata.dependencies.push(dep);
        self
    }

    pub fn require_capability(mut self, capability: String) -> Self {
        self.image.add_capability(capability);
        self
    }

    pub fn build(mut self) -> XRLangImage {
        self.image.calculate_checksum();
        self.image
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::bytecode::OpCode;
    use crate::value::Value;

    #[test]
    fn test_image_creation() {
        let bytecode = vec![
            ByteCode { op: OpCode::Push(Value::Int(42)), line: 1, column: 1 },
            ByteCode { op: OpCode::Push(Value::Int(8)), line: 1, column: 5 },
            ByteCode { op: OpCode::Add, line: 1, column: 7 },
        ];

        let image = ImageBuilder::new("test-app".to_string())
            .with_bytecode(bytecode)
            .with_author("Test Author".to_string())
            .with_description("Test XR application".to_string())
            .build();

        assert_eq!(image.metadata.name, "test-app");
        assert!(!image.bytecode.is_empty()); // Bytecode is serialized, so check it's not empty
        assert!(!image.metadata.checksum.is_empty());
    }

    #[test]
    fn test_image_serialization() {
        let image = ImageBuilder::new("test-app".to_string())
            .with_bytecode(vec![ByteCode { op: OpCode::Add, line: 1, column: 1 }])
            .build();

        let serialized = image.serialize().unwrap();
        let deserialized = XRLangImage::deserialize(&serialized).unwrap();

        assert_eq!(deserialized.metadata.name, image.metadata.name);
        assert_eq!(deserialized.bytecode.len(), image.bytecode.len());
        assert_eq!(deserialized.metadata.checksum, image.metadata.checksum);
    }

    #[test]
    fn test_capability_validation() {
        let mut image = XRLangImage::new("test".to_string(), Vec::new());
        image.add_capability("xr.tracking".to_string());
        image.add_capability("xr.hand_tracking".to_string());

        let available = vec!["xr.tracking".to_string(), "xr.hand_tracking".to_string()];
        assert!(image.validate_capabilities(&available).is_ok());

        let limited = vec!["xr.tracking".to_string()];
        let result = image.validate_capabilities(&limited);
        assert!(result.is_err());
        assert_eq!(result.unwrap_err(), vec!["xr.hand_tracking"]);
    }

    #[test]
    fn test_platform_validation() {
        let universal_image = ImageBuilder::new("app".to_string())
            .with_platform(Platform::Universal)
            .build();

        assert!(universal_image.validate_platform(Platform::MetaQuest));
        assert!(universal_image.validate_platform(Platform::WebXR));

        let quest_image = ImageBuilder::new("app".to_string())
            .with_platform(Platform::MetaQuest)
            .build();

        assert!(quest_image.validate_platform(Platform::MetaQuest));
        assert!(!quest_image.validate_platform(Platform::AppleVisionPro));
    }
}