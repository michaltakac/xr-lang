// MeshBasicMaterial shader - similar to Three.js MeshBasicMaterial
// A simple material that doesn't respond to lights

struct CameraUniforms {
    view_proj: mat4x4<f32>,
    model: mat4x4<f32>,
    time: f32,
}

struct MaterialUniforms {
    color: vec4<f32>,
    opacity: f32,
}

@group(0) @binding(0)
var<uniform> camera: CameraUniforms;

@group(1) @binding(0)
var<uniform> material: MaterialUniforms;

struct VertexInput {
    @location(0) position: vec3<f32>,
    @location(1) normal: vec3<f32>,
    @location(2) uv: vec2<f32>,
}

struct VertexOutput {
    @builtin(position) clip_position: vec4<f32>,
    @location(0) uv: vec2<f32>,
    @location(1) world_position: vec3<f32>,
}

@vertex
fn vs_main(model_vertex: VertexInput) -> VertexOutput {
    var out: VertexOutput;
    
    // Transform vertex position to world space
    let world_position = camera.model * vec4<f32>(model_vertex.position, 1.0);
    out.world_position = world_position.xyz;
    
    // Transform to clip space
    out.clip_position = camera.view_proj * world_position;
    
    // Pass through UV coordinates
    out.uv = model_vertex.uv;
    
    return out;
}

@fragment
fn fs_main(in: VertexOutput) -> @location(0) vec4<f32> {
    // MeshBasicMaterial simply returns the color without any lighting calculations
    var final_color = material.color;
    
    // Apply opacity
    final_color.a = material.opacity;
    
    // Discard fully transparent fragments
    if (final_color.a < 0.01) {
        discard;
    }
    
    return final_color;
}