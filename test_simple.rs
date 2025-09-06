use vm::test_framework::*;
use vm::value::Value;

fn main() {
    println!("Testing camera function registration...");
    
    let mut harness = TestHarness::new();
    
    // Try the simplest test first
    let result = harness.execute(r#"
        (println "Testing println")
        42
    "#);
    
    match result {
        Ok(val) => println!("Basic test passed: {:?}", val),
        Err(e) => println!("Basic test failed: {}", e),
    }
    
    // Now test camera creation with debug output
    println!("\nTesting camera creation...");
    let result = harness.execute(r#"
        (println "About to create camera")
        (create-perspective-camera 
            :position [10 5 15]
            :target [0 0 0]
            :fov 75)
    "#);
    
    match result {
        Ok(val) => println!("Camera test passed: {:?}", val),
        Err(e) => println!("Camera test failed: {}", e),
    }
}