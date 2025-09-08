use vm::test_framework::TestHarness;
use std::path::PathBuf;

#[test]
fn test_vm_macros_demo_scene_builds() {
    let mut harness = TestHarness::new();
    harness.reset();

    // Execute the VM macros demo example
    let path = PathBuf::from(env!("CARGO_MANIFEST_DIR"))
        .join("..")
        .join("examples")
        .join("vm_macros_demo.xrl");
    let res = harness.execute_file(path.to_str().unwrap());
    assert!(res.is_ok(), "executing vm_macros_demo.xrl failed: {:?}", res.err());

    // Snapshot and validate there are several objects
    let snap = harness.capture_scene_snapshot("vm_macros_demo");
    // We expect at least camera + several primitives
    assert!(snap.nodes.len() >= 6, "expected multiple scene nodes, got {}", snap.nodes.len());
}
