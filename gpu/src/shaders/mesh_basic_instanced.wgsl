// Instanced rendering shader for MeshBasicMaterial
// Optimized for rendering many objects with a single draw call

struct Camera {
    view: mat4x4<f32>,
    proj: mat4x4<f32>,
    position: vec3<f32>,
    _padding: f32,
}

struct Material {
    color: vec4<f32>,     // offset 0, size 16
    opacity: f32,         // offset 16, size 4  
    _padding: vec3<f32>,  // offset 20, size 12 (but aligned to 16, so starts at 32)
}

@group(0) @binding(0)
var<uniform> camera: Camera;

@group(1) @binding(0)
var<uniform> material: Material;

struct VertexInput {
    @location(0) position: vec3<f32>,
    @location(1) normal: vec3<f32>,
    @location(2) uv: vec2<f32>,
    // Instance data
    @location(3) instance_matrix_0: vec4<f32>,
    @location(4) instance_matrix_1: vec4<f32>,
    @location(5) instance_matrix_2: vec4<f32>,
    @location(6) instance_matrix_3: vec4<f32>,
    @location(7) instance_color: vec4<f32>,
}

struct VertexOutput {
    @builtin(position) clip_position: vec4<f32>,
    @location(0) world_position: vec3<f32>,
    @location(1) world_normal: vec3<f32>,
    @location(2) uv: vec2<f32>,
    @location(3) color: vec4<f32>,
}

@vertex
fn vs_main(input: VertexInput) -> VertexOutput {
    var output: VertexOutput;
    
    // Reconstruct instance model matrix
    let model = mat4x4<f32>(
        input.instance_matrix_0,
        input.instance_matrix_1,
        input.instance_matrix_2,
        input.instance_matrix_3
    );
    
    // Transform position and normal to world space
    let world_position = model * vec4<f32>(input.position, 1.0);
    output.world_position = world_position.xyz;
    
    // Transform normal (assuming uniform scaling)
    let normal_matrix = mat3x3<f32>(
        model[0].xyz,
        model[1].xyz,
        model[2].xyz
    );
    output.world_normal = normalize(normal_matrix * input.normal);
    
    // Final clip position
    output.clip_position = camera.proj * camera.view * world_position;
    
    // Pass through UV and instance color
    output.uv = input.uv;
    output.color = input.instance_color;
    
    return output;
}

@fragment
fn fs_main(input: VertexOutput) -> @location(0) vec4<f32> {
    // Combine material color with instance color
    var final_color = material.color * input.color;
    final_color.a = material.opacity * input.color.a;
    
    // Simple shading based on normal (optional)
    let light_dir = normalize(vec3<f32>(1.0, 1.0, 1.0));
    let ndotl = max(dot(input.world_normal, light_dir), 0.0);
    let ambient = 0.4;
    let diffuse = 0.6;
    let lighting = ambient + diffuse * ndotl;
    
    final_color = vec4<f32>(final_color.rgb * lighting, final_color.a);
    
    return final_color;
}