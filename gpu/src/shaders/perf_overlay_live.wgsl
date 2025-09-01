// Live Performance Overlay Shader

struct Uniforms {
    screen_size: vec2<f32>,
    panel_pos: vec2<f32>,
    panel_size: vec2<f32>,
    _padding1: vec2<f32>,
    bg_color: vec4<f32>,
    text_color: vec4<f32>,
    current_fps: f32,
    avg_frame_ms: f32,
    draw_calls: f32,
    triangle_count: f32,
}

@group(0) @binding(0)
var<uniform> uniforms: Uniforms;

struct VertexInput {
    @location(0) position: vec2<f32>,
    @location(1) color: vec4<f32>,
}

struct VertexOutput {
    @builtin(position) clip_position: vec4<f32>,
    @location(0) color: vec4<f32>,
    @location(1) uv: vec2<f32>,
}

@vertex
fn vs_main(input: VertexInput) -> VertexOutput {
    var output: VertexOutput;
    
    // Transform to screen space then NDC
    let screen_pos = uniforms.panel_pos + input.position;
    let ndc_x = (screen_pos.x / uniforms.screen_size.x) * 2.0 - 1.0;
    let ndc_y = 1.0 - (screen_pos.y / uniforms.screen_size.y) * 2.0;
    
    output.clip_position = vec4<f32>(ndc_x, ndc_y, 0.0, 1.0);
    output.color = input.color;
    output.uv = input.position / uniforms.panel_size;
    
    return output;
}

@fragment
fn fs_main(input: VertexOutput) -> @location(0) vec4<f32> {
    var color = input.color;
    
    // Add subtle gradient to background
    if (input.color.a > 0.9) {  // Background panel
        let gradient = 1.0 - input.uv.y * 0.2;
        color = vec4<f32>(
            color.rgb * gradient,
            color.a
        );
        
        // Add border
        let border_width = 0.01;
        if (input.uv.x < border_width || input.uv.x > (1.0 - border_width) ||
            input.uv.y < border_width || input.uv.y > (1.0 - border_width)) {
            color = vec4<f32>(0.2, 0.2, 0.2, 0.95);
        }
    }
    
    return color;
}