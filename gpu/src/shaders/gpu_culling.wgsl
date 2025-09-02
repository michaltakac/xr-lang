// GPU-driven culling system inspired by UE5's Nanite
// Performs frustum and occlusion culling entirely on GPU

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

struct CullingUniforms {
    view_proj: mat4x4<f32>,
    camera_pos: vec3<f32>,
    instance_count: u32,
    frustum_planes: array<vec4<f32>, 6>,  // 6 frustum planes
    lod_distances: vec4<f32>,  // LOD transition distances
}

struct DrawCommand {
    vertex_count: u32,
    instance_count: u32,
    first_vertex: u32,
    first_instance: u32,
}

// Buffers
@group(0) @binding(0) var<storage, read_write> instances: array<InstanceData>;
@group(0) @binding(1) var<uniform> culling: CullingUniforms;
@group(0) @binding(2) var<storage, read_write> visible_instances: array<u32>;
@group(0) @binding(3) var<storage, read_write> draw_commands: array<DrawCommand>;
@group(0) @binding(4) var<storage, read_write> visible_count: atomic<u32>;

// Hi-Z occlusion buffer for hierarchical occlusion culling (future enhancement)
// @group(0) @binding(5) var hi_z_texture: texture_2d<f32>;
// @group(0) @binding(6) var hi_z_sampler: sampler;

// Extract position from transform matrix
fn get_position(transform: mat4x4<f32>) -> vec3<f32> {
    return transform[3].xyz;
}

// Frustum culling test
fn frustum_cull(position: vec3<f32>, radius: f32) -> bool {
    for (var i = 0u; i < 6u; i = i + 1u) {
        let plane = culling.frustum_planes[i];
        let distance = dot(plane.xyz, position) + plane.w;
        if (distance < -radius) {
            return false;  // Outside frustum
        }
    }
    return true;  // Inside frustum
}

// Distance-based LOD selection
fn select_lod(position: vec3<f32>) -> u32 {
    let distance = length(culling.camera_pos - position);
    
    if (distance < culling.lod_distances.x) {
        return 0u;  // Highest detail
    } else if (distance < culling.lod_distances.y) {
        return 1u;  // Medium detail
    } else if (distance < culling.lod_distances.z) {
        return 2u;  // Low detail
    } else if (distance < culling.lod_distances.w) {
        return 3u;  // Lowest detail
    }
    
    return 4u;  // Culled (too far)
}

// Hierarchical Z-buffer occlusion test (disabled - requires Hi-Z texture)
// fn occlusion_cull(position: vec3<f32>, radius: f32) -> bool {
//     // Project bounding sphere to screen space
//     let clip_pos = culling.view_proj * vec4<f32>(position, 1.0);
//     let ndc = clip_pos.xyz / clip_pos.w;
//     
//     // Convert to texture coordinates
//     let uv = (ndc.xy + 1.0) * 0.5;
//     
//     // Sample Hi-Z buffer at appropriate mip level based on projected size
//     let projected_radius = radius / clip_pos.w;
//     let mip_level = clamp(log2(projected_radius * 512.0), 0.0, 8.0);
//     
//     let depth_sample = textureSampleLevel(hi_z_texture, hi_z_sampler, uv, mip_level).r;
//     
//     // Conservative depth test
//     return ndc.z <= depth_sample + 0.001;  // Small bias for floating point precision
// }

@compute @workgroup_size(64, 1, 1)
fn cull_instances(@builtin(global_invocation_id) global_id: vec3<u32>) {
    let instance_id = global_id.x;
    
    if (instance_id >= culling.instance_count) {
        return;
    }
    
    let position = get_position(instances[instance_id].model_matrix);
    let radius = instances[instance_id].bounding_radius;
    
    // Perform culling tests
    var visible = true;
    
    // 1. Frustum culling
    if (!frustum_cull(position, radius)) {
        visible = false;
    }
    
    // 2. Distance culling (LOD selection)
    let lod = select_lod(position);
    if (lod >= 4u) {
        visible = false;
    }
    
    // 3. Occlusion culling (optional, requires Hi-Z buffer)
    // Uncomment when Hi-Z buffer is available
    // if (visible && !occlusion_cull(position, radius)) {
    //     visible = false;
    // }
    
    // Write visibility result
    instances[instance_id].visible = select(0u, 1u, visible);
    
    // Add to visible list if not culled
    if (visible) {
        let index = atomicAdd(&visible_count, 1u);
        visible_instances[index] = instance_id;
    }
}

// Second pass: Generate indirect draw commands from visible instances
@compute @workgroup_size(1, 1, 1)
fn generate_draw_commands() {
    let count = atomicLoad(&visible_count);
    
    // Generate single indirect draw command for all visible instances
    draw_commands[0].vertex_count = 36u;  // Vertices per mesh (cube)
    draw_commands[0].instance_count = count;
    draw_commands[0].first_vertex = 0u;
    draw_commands[0].first_instance = 0u;
}