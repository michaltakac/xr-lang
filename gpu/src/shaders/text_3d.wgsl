// 3D Text rendering shader for UI components

struct TextUniforms {
    transform: mat4x4<f32>,
}

@group(0) @binding(0)
var<uniform> uniforms: TextUniforms;

@group(0) @binding(1)
var font_texture: texture_2d<f32>;

@group(0) @binding(2)
var font_sampler: sampler;

struct VertexInput {
    @location(0) position: vec3<f32>,
    @location(1) uv: vec2<f32>,
    @location(2) color: vec4<f32>,
}

struct VertexOutput {
    @builtin(position) clip_position: vec4<f32>,
    @location(0) uv: vec2<f32>,
    @location(1) color: vec4<f32>,
}

@vertex
fn vs_main(input: VertexInput) -> VertexOutput {
    var out: VertexOutput;
    
    out.clip_position = uniforms.transform * vec4<f32>(input.position, 1.0);
    out.uv = input.uv;
    out.color = input.color;
    
    return out;
}

@fragment
fn fs_main(input: VertexOutput) -> @location(0) vec4<f32> {
    // Sample the SDF texture
    let distance = textureSample(font_texture, font_sampler, input.uv).r;
    
    // SDF rendering with smoothstep for anti-aliasing
    // 0.5 is the edge of the glyph in SDF space
    // Adjust the smoothstep range for sharper or softer edges
    let width = 0.7;  // Controls edge softness
    let edge = 0.5;   // The actual edge position in SDF
    
    // Calculate screen-space derivatives for adaptive anti-aliasing
    let dfdx = dpdx(distance);
    let dfdy = dpdy(distance);
    let grad_len = length(vec2<f32>(dfdx, dfdy));
    let pixel_dist = grad_len * width;
    
    // Use smoothstep for anti-aliased edge
    let alpha = smoothstep(edge - pixel_dist, edge + pixel_dist, distance);
    
    // Apply text color with calculated alpha
    return vec4<f32>(input.color.rgb, input.color.a * alpha);
}