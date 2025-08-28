// Transform gizmo shader for interactive object manipulation

struct GizmoUniforms {
    view_proj: mat4x4<f32>,
    model: mat4x4<f32>,
    scale_factor: f32,
}

@group(0) @binding(0)
var<uniform> uniforms: GizmoUniforms;

struct VertexInput {
    @location(0) position: vec3<f32>,
    @location(1) color: vec4<f32>,
}

struct VertexOutput {
    @builtin(position) clip_position: vec4<f32>,
    @location(0) color: vec4<f32>,
}

@vertex
fn vs_main(input: VertexInput) -> VertexOutput {
    var out: VertexOutput;
    
    // Transform position to world space then to clip space
    let world_pos = uniforms.model * vec4<f32>(input.position, 1.0);
    out.clip_position = uniforms.view_proj * world_pos;
    out.color = input.color;
    
    return out;
}

@fragment
fn fs_main(input: VertexOutput) -> @location(0) vec4<f32> {
    // Simple pass-through with some brightness adjustment for visibility
    return vec4<f32>(input.color.rgb * 1.2, input.color.a);
}