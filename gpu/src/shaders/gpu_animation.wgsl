// GPU-based animation system inspired by UE5's Mass Entity system
// Evaluates behaviors entirely on GPU using compute shaders

struct InstanceData {
    model_matrix: mat4x4<f32>,
    color: vec4<f32>,
    animation_time: f32,
    animation_speed: f32,
    animation_amplitude: f32,
    bounding_radius: f32,
    visible: u32,
    _padding: vec3<u32>,
}

struct AnimationParams {
    delta_time: f32,
    total_time: f32,
    instance_count: u32,
    _padding: u32,
}

// Input/Output buffers
@group(0) @binding(0) var<storage, read_write> instances: array<InstanceData>;
@group(0) @binding(1) var<uniform> params: AnimationParams;

// Wave animation behavior
fn wave_animation(instance_id: u32, dt: f32) -> mat4x4<f32> {
    let pos = instances[instance_id].model_matrix[3].xyz;
    let new_time = instances[instance_id].animation_time + dt;
    let speed = instances[instance_id].animation_speed;
    let amplitude = instances[instance_id].animation_amplitude;
    
    // Wave function based on position
    let wave_offset = sin(new_time * speed + pos.x * 0.2 + pos.z * 0.2);
    let base_y = pos.y - amplitude * sin(instances[instance_id].animation_time * speed + pos.x * 0.2 + pos.z * 0.2);
    let new_y = base_y + amplitude * wave_offset;
    
    var transform = instances[instance_id].model_matrix;
    transform[3].y = new_y;
    
    // Update animation time
    instances[instance_id].animation_time = new_time;
    
    return transform;
}

// Rotation animation behavior
fn rotate_animation(instance_id: u32, speed: f32, dt: f32) -> mat4x4<f32> {
    let angle = speed * dt;
    let c = cos(angle);
    let s = sin(angle);
    
    // Y-axis rotation matrix
    let rotation = mat4x4<f32>(
        vec4<f32>(c, 0.0, s, 0.0),
        vec4<f32>(0.0, 1.0, 0.0, 0.0),
        vec4<f32>(-s, 0.0, c, 0.0),
        vec4<f32>(0.0, 0.0, 0.0, 1.0)
    );
    
    return rotation * instances[instance_id].model_matrix;
}

// Pulse animation behavior
fn pulse_animation(instance_id: u32, dt: f32) -> mat4x4<f32> {
    let new_time = instances[instance_id].animation_time + dt;
    let speed = instances[instance_id].animation_speed;
    let scale_factor = 1.0 + 0.3 * sin(new_time * speed);
    
    var transform = instances[instance_id].model_matrix;
    
    // Extract and modify scale
    let scale_x = length(vec3<f32>(transform[0].x, transform[0].y, transform[0].z));
    let scale_y = length(vec3<f32>(transform[1].x, transform[1].y, transform[1].z));
    let scale_z = length(vec3<f32>(transform[2].x, transform[2].y, transform[2].z));
    
    // Apply pulse to scale
    transform[0].x = transform[0].x / scale_x * (scale_x * scale_factor);
    transform[1].y = transform[1].y / scale_y * (scale_y * scale_factor);
    transform[2].z = transform[2].z / scale_z * (scale_z * scale_factor);
    
    instances[instance_id].animation_time = new_time;
    
    return transform;
}

@compute @workgroup_size(64, 1, 1)
fn animate_instances(@builtin(global_invocation_id) global_id: vec3<u32>) {
    let instance_id = global_id.x;
    
    if (instance_id >= params.instance_count) {
        return;
    }
    
    let dt = params.delta_time;
    
    // Skip if instance is culled
    if (instances[instance_id].visible == 0u) {
        return;
    }
    
    // Determine animation type based on instance ID pattern
    // In production, this would be stored per-instance
    let animation_type = instance_id % 3u;
    
    var new_transform: mat4x4<f32>;
    
    switch animation_type {
        case 0u: {
            // Wave animation
            new_transform = wave_animation(instance_id, dt);
        }
        case 1u: {
            // Rotation animation
            new_transform = rotate_animation(instance_id, instances[instance_id].animation_speed, dt);
        }
        case 2u: {
            // Pulse animation
            new_transform = pulse_animation(instance_id, dt);
        }
        default: {
            new_transform = instances[instance_id].model_matrix;
        }
    }
    
    instances[instance_id].model_matrix = new_transform;
}