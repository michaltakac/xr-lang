// Test harness for Code Sync functionality
use gpu::code_sync::CodeSync;
use gpu::runtime_state::{RuntimeState, AuthoringMode, CameraState};
use gpu::math::Vec3;
use std::fs;

fn main() -> anyhow::Result<()> {
    println!("Testing Code Sync functionality...");
    
    // Create test DSL file
    let test_file = "test_sync.xrdsl";
    let original_content = r#"(defscene3d test
  (camera
    (position 0 5 15)
    (target 0 0 0)
    (fov 45))
    
  (object cube1 cube
    (position 0 0 0)
    (scale 1.0 1.0 1.0)))"#;
    
    fs::write(test_file, original_content)?;
    println!("✅ Created test file: {}", test_file);
    
    // Create code sync with runtime state
    let mut code_sync = CodeSync::new();
    code_sync.set_source_path(test_file);
    
    let mut runtime_state = RuntimeState::new();
    
    // Test 1: In Design mode (should not sync)
    println!("\n📌 Test 1: Design mode (should not sync)");
    runtime_state.authoring_mode = AuthoringMode::Design;
    
    code_sync.queue_camera_update(&CameraState {
        position: Vec3::new(10.0, 20.0, 30.0),
        target: Vec3::new(1.0, 2.0, 3.0),
        fov: std::f32::consts::FRAC_PI_4,
    });
    
    code_sync.sync_to_file(&runtime_state)?;
    
    let content = fs::read_to_string(test_file)?;
    if content == original_content {
        println!("✅ Design mode: File unchanged (correct)");
    } else {
        println!("❌ Design mode: File was modified (incorrect)");
    }
    
    // Test 2: In Play mode (should not sync)
    println!("\n📌 Test 2: Play mode (should not sync)");
    runtime_state.authoring_mode = AuthoringMode::Play;
    
    code_sync.queue_camera_update(&CameraState {
        position: Vec3::new(11.0, 21.0, 31.0),
        target: Vec3::new(2.0, 3.0, 4.0),
        fov: std::f32::consts::FRAC_PI_4,
    });
    
    code_sync.sync_to_file(&runtime_state)?;
    
    let content = fs::read_to_string(test_file)?;
    if content == original_content {
        println!("✅ Play mode: File unchanged (correct)");
    } else {
        println!("❌ Play mode: File was modified (incorrect)");
    }
    
    // Test 3: In Live mode (should sync)
    println!("\n📌 Test 3: Live mode (should sync)");
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
    if content != original_content {
        println!("✅ Live mode: File was modified (correct)");
        println!("Modified content:\n{}", content);
        
        // Check specific changes
        if content.contains("12.00 22.00 32.00") {
            println!("  ✅ Camera position updated");
        }
        if content.contains("3.00 4.00 5.00") {
            println!("  ✅ Camera target updated");
        }
        if content.contains("1.00 2.00 3.00") {
            println!("  ✅ Object position updated");
        }
        if content.contains("2.00 2.00 2.00") {
            println!("  ✅ Object scale updated");
        }
    } else {
        println!("❌ Live mode: File was not modified (incorrect)");
    }
    
    // Clean up
    fs::remove_file(test_file)?;
    println!("\n✅ Test complete, cleaned up test file");
    
    Ok(())
}