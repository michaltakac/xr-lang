// Performance Monitor Shader
// Renders stats overlay with background panel and text

struct Uniforms {
    screen_size: vec2<f32>,
    panel_position: vec2<f32>,
    panel_size: vec2<f32>,
    background_color: vec4<f32>,
    text_color: vec4<f32>,
    _padding: vec2<f32>,
}

@group(0) @binding(0)
var<uniform> uniforms: Uniforms;

struct VertexInput {
    @location(0) position: vec2<f32>,
    @location(1) color: vec4<f32>,
    @location(2) uv: vec2<f32>,
}

struct VertexOutput {
    @builtin(position) clip_position: vec4<f32>,
    @location(0) color: vec4<f32>,
    @location(1) uv: vec2<f32>,
}

@vertex
fn vs_main(input: VertexInput) -> VertexOutput {
    var output: VertexOutput;
    
    // Convert panel position to NDC
    let screen_pos = uniforms.panel_position + input.position;
    let ndc_x = (screen_pos.x / uniforms.screen_size.x) * 2.0 - 1.0;
    let ndc_y = 1.0 - (screen_pos.y / uniforms.screen_size.y) * 2.0;
    
    output.clip_position = vec4<f32>(ndc_x, ndc_y, 0.0, 1.0);
    output.color = input.color;
    output.uv = input.uv;
    
    return output;
}

@fragment
fn fs_main(input: VertexOutput) -> @location(0) vec4<f32> {
    // For now, just render the background color
    // TODO: Add text rendering using SDF or bitmap fonts
    return input.color;
}