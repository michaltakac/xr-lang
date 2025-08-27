// UI Panel shader for rendering 3D code editor panels

struct Uniforms {
    view_proj: mat4x4<f32>,
    model: mat4x4<f32>,
}

@group(0) @binding(0)
var<uniform> uniforms: Uniforms;

struct VertexInput {
    @location(0) position: vec3<f32>,
    @location(1) color: vec4<f32>,
    @location(2) uv: vec2<f32>,
}

struct VertexOutput {
    @builtin(position) clip_position: vec4<f32>,
    @location(0) color: vec4<f32>,
    @location(1) uv: vec2<f32>,
}

@vertex
fn vs_main(in: VertexInput) -> VertexOutput {
    var out: VertexOutput;
    let world_pos = uniforms.model * vec4<f32>(in.position, 1.0);
    out.clip_position = uniforms.view_proj * world_pos;
    out.color = in.color;
    out.uv = in.uv;
    return out;
}

@fragment
fn fs_main(in: VertexOutput) -> @location(0) vec4<f32> {
    // Create a gradient background for the panel
    let gradient = mix(
        vec4<f32>(0.1, 0.1, 0.2, 0.9),  // Dark blue
        vec4<f32>(0.15, 0.15, 0.25, 0.95), // Slightly lighter blue
        in.uv.y
    );
    
    // Add border effect
    let border_width = 0.02;
    let border = 
        smoothstep(0.0, border_width, in.uv.x) *
        smoothstep(0.0, border_width, in.uv.y) *
        smoothstep(0.0, border_width, 1.0 - in.uv.x) *
        smoothstep(0.0, border_width, 1.0 - in.uv.y);
    
    // Mix gradient with border color
    let border_color = vec4<f32>(0.3, 0.6, 0.9, 1.0);
    let final_color = mix(border_color, gradient, border);
    
    // Add some transparency
    return vec4<f32>(final_color.rgb, final_color.a * 0.85);
}