// Performance Overlay Shader - 2D rendering on top of 3D scene

struct Uniforms {
    screen_size: vec2<f32>,
    panel_pos: vec2<f32>,
    panel_size: vec2<f32>,
    _padding1: vec2<f32>,
    bg_color: vec4<f32>,
    text_color: vec4<f32>,
}

@group(0) @binding(0)
var<uniform> uniforms: Uniforms;

struct VertexInput {
    @location(0) position: vec2<f32>,
    @location(1) tex_coords: vec2<f32>,
    @builtin(instance_index) instance_id: u32,
}

struct VertexOutput {
    @builtin(position) clip_position: vec4<f32>,
    @location(0) color: vec4<f32>,
    @location(1) uv: vec2<f32>,
    @location(2) instance_id: f32,
}

@vertex
fn vs_main(input: VertexInput) -> VertexOutput {
    var output: VertexOutput;
    
    var pos = input.position;
    var color = uniforms.bg_color;
    
    if (input.instance_id == 0u) {
        // Main background panel
        pos = pos * uniforms.panel_size + uniforms.panel_pos;
        color = uniforms.bg_color;
    } else if (input.instance_id < 21u) {
        // FPS/GPU graph bars
        let bar_width = 8.0;
        let bar_height = 40.0;
        let bar_x = uniforms.panel_pos.x + 10.0 + f32(input.instance_id - 1u) * (bar_width + 2.0);
        let bar_y = uniforms.panel_pos.y + 50.0;
        
        // Simulate GPU usage (in real implementation, this would be based on actual metrics)
        let height_factor = sin(f32(input.instance_id) * 0.5 + uniforms.screen_size.x * 0.001) * 0.5 + 0.5;
        
        pos = vec2<f32>(
            pos.x * bar_width + bar_x,
            pos.y * bar_height * height_factor + bar_y
        );
        
        // Color based on performance
        if (height_factor > 0.6) {
            color = vec4<f32>(0.0, 1.0, 0.0, 0.8); // Green
        } else if (height_factor > 0.3) {
            color = vec4<f32>(1.0, 1.0, 0.0, 0.8); // Yellow
        } else {
            color = vec4<f32>(1.0, 0.0, 0.0, 0.8); // Red
        }
    } else if (input.instance_id == 21u) {
        // "GPU" label
        pos = vec2<f32>(
            pos.x * 30.0 + uniforms.panel_pos.x + 10.0,
            pos.y * 10.0 + uniforms.panel_pos.y + 35.0
        );
        color = vec4<f32>(0.8, 0.8, 0.8, 1.0);
    } else if (input.instance_id == 22u) {
        // "FPS" label
        pos = vec2<f32>(
            pos.x * 25.0 + uniforms.panel_pos.x + 10.0,
            pos.y * 10.0 + uniforms.panel_pos.y + 10.0
        );
        color = vec4<f32>(0.8, 0.8, 0.8, 1.0);
    }
    
    // Convert to NDC (-1 to 1)
    let ndc_x = (pos.x / uniforms.screen_size.x) * 2.0 - 1.0;
    let ndc_y = 1.0 - (pos.y / uniforms.screen_size.y) * 2.0;
    
    output.clip_position = vec4<f32>(ndc_x, ndc_y, 0.0, 1.0);
    output.color = color;
    output.uv = input.tex_coords;
    output.instance_id = f32(input.instance_id);
    
    return output;
}

@fragment
fn fs_main(input: VertexOutput) -> @location(0) vec4<f32> {
    var color = input.color;
    
    // Add some visual effects
    if (input.instance_id == 0.0) {
        // Background panel - add border effect
        let border_width = 0.02;
        if (input.uv.x < border_width || input.uv.x > (1.0 - border_width) ||
            input.uv.y < border_width || input.uv.y > (1.0 - border_width)) {
            color = vec4<f32>(0.3, 0.3, 0.3, 0.9);
        }
    }
    
    return color;
}