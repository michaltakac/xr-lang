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
    
    // SDF rendering with improved anti-aliasing
    // 0.5 is the edge of the glyph in SDF space
    let edge = 0.5;   // The actual edge position in SDF
    
    // Calculate screen-space derivatives for adaptive anti-aliasing
    let dfdx = dpdx(distance);
    let dfdy = dpdy(distance);
    let grad_len = length(vec2<f32>(dfdx, dfdy));
    
    // Adaptive width based on screen-space gradient
    // This ensures consistent edge quality at all viewing distances
    let width = min(grad_len * 1.5, 0.15);  // Clamped for sharper edges
    
    // Use smoothstep for anti-aliased edge
    let alpha = smoothstep(edge - width, edge + width, distance);
    
    // Optional: Add slight outline for better visibility
    // Uncomment for dark outline effect
    // let outline_edge = 0.4;
    // let outline_alpha = smoothstep(outline_edge - width, outline_edge + width, distance);
    // let final_color = mix(vec4<f32>(0.0, 0.0, 0.0, outline_alpha), 
    //                       vec4<f32>(input.color.rgb, alpha), 
    //                       alpha);
    
    // Apply gamma correction for better color reproduction
    let gamma_corrected = pow(input.color.rgb, vec3<f32>(2.2));
    
    // Apply text color with calculated alpha
    return vec4<f32>(gamma_corrected, input.color.a * alpha);
}