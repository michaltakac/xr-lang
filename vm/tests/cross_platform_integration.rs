//! Cross-Platform Integration Tests
//! 
//! Demonstrates the portable image system and capability-based platform abstraction

#[cfg(test)]
mod cross_platform_tests {
    use vm::image::{XRLangImage, ImageBuilder, Platform};
    use vm::capability::{create_platform_capabilities, DeviceType};
    use vm::bytecode::{VM, ByteCode, OpCode};
    use vm::value::Value;
    use std::rc::Rc;
    
    #[test]
    fn test_portable_image_creation() {
        // Create bytecode for a simple XR scene
        let bytecode = vec![
            ByteCode { 
                op: OpCode::Push(Value::Float(1.0)), 
                line: 1, column: 1 
            },
            ByteCode { 
                op: OpCode::CallCapability {
                    capability: "scene".to_string(),
                    method: "create_cube".to_string(),
                    arg_count: 1,
                }, 
                line: 1, column: 5 
            },
        ];
        
        // Build the image
        let image = ImageBuilder::new("xr-app".to_string())
            .with_bytecode(bytecode)
            .with_platform(Platform::Universal)
            .with_author("XR Developer".to_string())
            .with_description("Cross-platform XR application".to_string())
            .require_capability("scene".to_string())
            .require_capability("xr.tracking".to_string())
            .build();
        
        // Verify image properties
        assert_eq!(image.metadata.name, "xr-app");
        assert_eq!(image.platform, Platform::Universal);
        assert!(image.capabilities_required.contains(&"scene".to_string()));
        assert!(image.capabilities_required.contains(&"xr.tracking".to_string()));
        assert!(!image.metadata.checksum.is_empty());
    }
    
    #[test]
    fn test_image_serialization_deserialization() {
        let bytecode = vec![
            ByteCode { op: OpCode::Add, line: 1, column: 1 }
        ];
        
        let original = ImageBuilder::new("test".to_string())
            .with_bytecode(bytecode)
            .with_platform(Platform::MetaQuest)
            .require_capability("xr.tracking".to_string())
            .build();
        
        // Serialize the image
        let serialized = original.serialize().unwrap();
        
        // Deserialize it back
        let deserialized = XRLangImage::deserialize(&serialized).unwrap();
        
        // Verify integrity
        assert_eq!(deserialized.metadata.name, original.metadata.name);
        assert_eq!(deserialized.platform, original.platform);
        assert_eq!(deserialized.capabilities_required, original.capabilities_required);
        assert_eq!(deserialized.metadata.checksum, original.metadata.checksum);
    }
    
    #[test]
    fn test_platform_capability_validation() {
        let image = ImageBuilder::new("vr-app".to_string())
            .with_platform(Platform::Universal)
            .require_capability("xr.tracking".to_string())
            .require_capability("xr.hand_tracking".to_string())
            .require_capability("scene".to_string())
            .build();
        
        // Test with VR headset capabilities
        let vr_capabilities = vec![
            "xr.tracking".to_string(),
            "xr.hand_tracking".to_string(),
            "scene".to_string(),
        ];
        assert!(image.validate_capabilities(&vr_capabilities).is_ok());
        
        // Test with desktop capabilities (missing XR features)
        let desktop_capabilities = vec![
            "scene".to_string(),
        ];
        let result = image.validate_capabilities(&desktop_capabilities);
        assert!(result.is_err());
        let missing = result.unwrap_err();
        assert!(missing.contains(&"xr.tracking".to_string()));
        assert!(missing.contains(&"xr.hand_tracking".to_string()));
    }
    
    #[test]
    fn test_platform_specific_execution() {
        // Create capability tables for different platforms
        let vr_capabilities = Rc::new(create_platform_capabilities(DeviceType::HeadMountedDisplay));
        let desktop_capabilities = Rc::new(create_platform_capabilities(DeviceType::Desktop));
        
        // Create bytecode that uses scene capability (available on all platforms)
        let scene_bytecode = vec![
            ByteCode { 
                op: OpCode::Push(Value::Float(2.0)), 
                line: 1, column: 1 
            },
            ByteCode { 
                op: OpCode::CallCapability {
                    capability: "scene".to_string(),
                    method: "create_sphere".to_string(),
                    arg_count: 1,
                }, 
                line: 1, column: 5 
            },
        ];
        
        // Execute on VR platform
        let mut vr_vm = VM::with_capabilities(vr_capabilities.clone());
        let vr_result = vr_vm.execute(scene_bytecode.clone(), None).unwrap();
        if let Value::Map(map) = vr_result {
            assert_eq!(map.get("type"), Some(&Value::Str("sphere".to_string())));
            assert_eq!(map.get("radius"), Some(&Value::Float(2.0)));
        } else {
            panic!("Expected Map result");
        }
        
        // Execute on Desktop platform
        let mut desktop_vm = VM::with_capabilities(desktop_capabilities);
        let desktop_result = desktop_vm.execute(scene_bytecode, None).unwrap();
        if let Value::Map(map) = desktop_result {
            assert_eq!(map.get("type"), Some(&Value::Str("sphere".to_string())));
            assert_eq!(map.get("radius"), Some(&Value::Float(2.0)));
        } else {
            panic!("Expected Map result");
        }
    }
    
    #[test]
    fn test_xr_specific_capability() {
        let vr_capabilities = Rc::new(create_platform_capabilities(DeviceType::HeadMountedDisplay));
        let mut vr_vm = VM::with_capabilities(vr_capabilities);
        
        // Create bytecode that uses XR-specific capability
        let xr_bytecode = vec![
            ByteCode { 
                op: OpCode::CallCapability {
                    capability: "xr.tracking".to_string(),
                    method: "get_head_pose".to_string(),
                    arg_count: 0,
                }, 
                line: 1, column: 1 
            },
        ];
        
        // Should work on VR platform
        let result = vr_vm.execute(xr_bytecode.clone(), None).unwrap();
        if let Value::Map(map) = result {
            assert!(map.contains_key("position"));
            assert!(map.contains_key("rotation"));
        } else {
            panic!("Expected Map result with pose data");
        }
        
        // Desktop platform without XR tracking
        let desktop_capabilities = Rc::new(create_platform_capabilities(DeviceType::Desktop));
        let mut desktop_vm = VM::with_capabilities(desktop_capabilities);
        
        // Should fail on desktop (no xr.tracking capability)
        let desktop_result = desktop_vm.execute(xr_bytecode, None);
        assert!(desktop_result.is_err());
        assert!(desktop_result.unwrap_err().contains("Capability 'xr.tracking' not found"));
    }
    
    #[test]
    fn test_platform_info() {
        let quest_table = create_platform_capabilities(DeviceType::HeadMountedDisplay);
        let platform_info = quest_table.platform_info();
        
        assert_eq!(platform_info.name, "Meta Quest 3");
        assert_eq!(platform_info.device_type as u32, DeviceType::HeadMountedDisplay as u32);
        assert_eq!(platform_info.display_info.resolution, (2064, 2208));
        assert_eq!(platform_info.display_info.refresh_rate, 120.0);
        assert!(platform_info.display_info.fov.is_some());
        assert!(platform_info.input_capabilities.len() > 0);
    }
    
    #[test]
    fn test_image_platform_targeting() {
        // Universal image works on any platform
        let universal_image = ImageBuilder::new("app".to_string())
            .with_platform(Platform::Universal)
            .build();
        
        assert!(universal_image.validate_platform(Platform::MetaQuest));
        assert!(universal_image.validate_platform(Platform::AppleVisionPro));
        assert!(universal_image.validate_platform(Platform::Desktop));
        assert!(universal_image.validate_platform(Platform::WebXR));
        
        // Platform-specific image only works on target platform
        let quest_image = ImageBuilder::new("quest-app".to_string())
            .with_platform(Platform::MetaQuest)
            .build();
        
        assert!(quest_image.validate_platform(Platform::MetaQuest));
        assert!(!quest_image.validate_platform(Platform::AppleVisionPro));
        assert!(!quest_image.validate_platform(Platform::Desktop));
    }
}