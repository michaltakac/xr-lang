// Integration test for Code Sync functionality
use gpu::code_sync::CodeSync;
use gpu::runtime_state::{RuntimeState, AuthoringMode, CameraState};
use gpu::math::Vec3;
use std::fs;

#[test]
fn test_code_sync_modes() -> anyhow::Result<()> {
    // Create test DSL file
    let test_file = "/tmp/test_sync.xrdsl";
    let original_content = r#"(defscene3d test
  (camera
    (position 0 5 15)
    (target 0 0 0)
    (fov 45))
    
  (object cube1 cube
    (position 0 0 0)
    (scale 1.0 1.0 1.0)))"#;
    
    fs::write(test_file, original_content)?;
    
    // Create code sync with runtime state
    let mut code_sync = CodeSync::new();
    code_sync.set_source_path(test_file);
    
    let mut runtime_state = RuntimeState::new();
    
    // Test 1: In Design mode (should not sync)
    runtime_state.authoring_mode = AuthoringMode::Design;
    
    code_sync.queue_camera_update(&CameraState {
        position: Vec3::new(10.0, 20.0, 30.0),
        target: Vec3::new(1.0, 2.0, 3.0),
        fov: std::f32::consts::FRAC_PI_4,
    });
    
    code_sync.sync_to_file(&runtime_state)?;
    
    let content = fs::read_to_string(test_file)?;
    assert_eq!(content, original_content, "Design mode should not modify file");
    
    // Test 2: In Play mode (should not sync)
    runtime_state.authoring_mode = AuthoringMode::Play;
    
    code_sync.queue_camera_update(&CameraState {
        position: Vec3::new(11.0, 21.0, 31.0),
        target: Vec3::new(2.0, 3.0, 4.0),
        fov: std::f32::consts::FRAC_PI_4,
    });
    
    code_sync.sync_to_file(&runtime_state)?;
    
    let content = fs::read_to_string(test_file)?;
    assert_eq!(content, original_content, "Play mode should not modify file");
    
    // Test 3: In Live mode (should sync)
    runtime_state.authoring_mode = AuthoringMode::Live;
    
    code_sync.queue_camera_update(&CameraState {
        position: Vec3::new(12.0, 22.0, 32.0),
        target: Vec3::new(3.0, 4.0, 5.0),
        fov: std::f32::consts::FRAC_PI_3,
    });
    
    code_sync.queue_object_update("cube1", 
        Vec3::new(1.0, 2.0, 3.0),
        Vec3::new(2.0, 2.0, 2.0));
    
    code_sync.sync_to_file(&runtime_state)?;
    
    let content = fs::read_to_string(test_file)?;
    assert_ne!(content, original_content, "Live mode should modify file");
    
    // Check specific changes
    assert!(content.contains("12.00 22.00 32.00"), "Camera position should be updated");
    assert!(content.contains("3.00 4.00 5.00"), "Camera target should be updated");
    assert!(content.contains("1.00 2.00 3.00"), "Object position should be updated");
    assert!(content.contains("2.00 2.00 2.00"), "Object scale should be updated");
    
    // Clean up
    fs::remove_file(test_file)?;
    
    Ok(())
}