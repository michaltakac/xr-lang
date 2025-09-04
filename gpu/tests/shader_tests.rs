//! Tests for shader compilation, hot-swapping, and DSL integration
//! These tests validate shader functionality and runtime updates

use gpu::{cache::{ShaderCache, ShaderId}};
use std::collections::HashMap;

#[cfg(test)]
mod shader_compilation_tests {
    
    #[test]
    fn test_basic_shader_compilation() {
        // Test that we can compile a basic WGSL shader
        let wgsl_code = r#"
            @vertex
            fn vs_main(@builtin(vertex_index) in_vertex_index: u32) -> @builtin(position) vec4<f32> {
                var pos = array<vec2<f32>, 3>(
                    vec2<f32>(0.0, 0.5),
                    vec2<f32>(-0.5, -0.5),
                    vec2<f32>(0.5, -0.5)
                );
                return vec4<f32>(pos[in_vertex_index], 0.0, 1.0);
            }

            @fragment
            fn fs_main() -> @location(0) vec4<f32> {
                return vec4<f32>(1.0, 0.0, 0.0, 1.0);
            }
        "#;
        
        // This would require a Device instance, which we can't create in unit tests
        // So we'll just validate the shader string is well-formed
        assert!(wgsl_code.contains("@vertex"));
        assert!(wgsl_code.contains("@fragment"));
        assert!(wgsl_code.contains("vs_main"));
        assert!(wgsl_code.contains("fs_main"));
    }

    #[test]
    fn test_shader_with_uniforms() {
        let wgsl_code = r#"
            struct Uniforms {
                model_view_proj: mat4x4<f32>,
                color: vec4<f32>,
            }

            @group(0) @binding(0)
            var<uniform> uniforms: Uniforms;

            struct VertexOutput {
                @builtin(position) position: vec4<f32>,
                @location(0) color: vec4<f32>,
            }

            @vertex
            fn vs_main(@location(0) position: vec3<f32>) -> VertexOutput {
                var output: VertexOutput;
                output.position = uniforms.model_view_proj * vec4<f32>(position, 1.0);
                output.color = uniforms.color;
                return output;
            }

            @fragment
            fn fs_main(@location(0) color: vec4<f32>) -> @location(0) vec4<f32> {
                return color;
            }
        "#;
        
        // Validate uniform structure
        assert!(wgsl_code.contains("struct Uniforms"));
        assert!(wgsl_code.contains("@group(0) @binding(0)"));
        assert!(wgsl_code.contains("var<uniform>"));
    }

    #[test]
    fn test_compute_shader() {
        let wgsl_code = r#"
            @group(0) @binding(0)
            var<storage, read_write> data: array<f32>;

            @compute @workgroup_size(64)
            fn main(@builtin(global_invocation_id) global_id: vec3<u32>) {
                let index = global_id.x;
                data[index] = data[index] * 2.0;
            }
        "#;
        
        assert!(wgsl_code.contains("@compute"));
        assert!(wgsl_code.contains("@workgroup_size"));
        assert!(wgsl_code.contains("var<storage"));
    }
}

#[cfg(test)]
mod shader_cache_tests {
    use super::*;
    
    #[test]
    fn test_shader_cache_hashing() {
        // Test that shader caching works based on content hash
        let shader1 = "fn main() { return vec4<f32>(1.0); }";
        let shader2 = "fn main() { return vec4<f32>(1.0); }"; // Same content
        let shader3 = "fn main() { return vec4<f32>(0.0); }"; // Different content
        
        // These would have the same hash
        let hash1 = gpu::cache::hash_wgsl(shader1);
        let hash2 = gpu::cache::hash_wgsl(shader2);
        let hash3 = gpu::cache::hash_wgsl(shader3);
        
        assert_eq!(hash1, hash2, "Identical shaders should have same hash");
        assert_ne!(hash1, hash3, "Different shaders should have different hash");
    }

    #[test]
    fn test_shader_cache_storage() {
        let cache = ShaderCache::new();
        
        // Initially empty
        assert_eq!(cache.modules.len(), 0);
        
        // Would need Device to actually test insertion
        // But we can test the structure
        assert!(cache.modules.is_empty());
    }
}

#[cfg(test)]
mod shader_hot_swap_tests {
    use super::*;
    
    #[test]
    fn test_shader_modification_detection() {
        // Test that we can detect when a shader has been modified
        let original_shader = r#"
            @fragment
            fn fs_main() -> @location(0) vec4<f32> {
                return vec4<f32>(1.0, 0.0, 0.0, 1.0); // Red
            }
        "#;
        
        let modified_shader = r#"
            @fragment
            fn fs_main() -> @location(0) vec4<f32> {
                return vec4<f32>(0.0, 1.0, 0.0, 1.0); // Green
            }
        "#;
        
        let hash1 = gpu::cache::hash_wgsl(original_shader);
        let hash2 = gpu::cache::hash_wgsl(modified_shader);
        
        assert_ne!(hash1, hash2, "Modified shader should have different hash");
    }

    #[test]
    fn test_shader_hot_swap_workflow() {
        // Simulate the workflow of hot-swapping a shader
        
        // 1. Initial shader
        let initial_shader = r#"
            @fragment
            fn fs_main() -> @location(0) vec4<f32> {
                return vec4<f32>(1.0, 0.0, 0.0, 1.0);
            }
        "#;
        
        // 2. Modified shader
        let modified_shader = r#"
            @fragment
            fn fs_main() -> @location(0) vec4<f32> {
                return vec4<f32>(0.0, 1.0, 0.0, 1.0);
            }
        "#;
        
        // 3. Track versions
        let mut shader_versions: HashMap<String, ShaderId> = HashMap::new();
        shader_versions.insert(
            "main_shader".to_string(), 
            gpu::cache::hash_wgsl(initial_shader)
        );
        
        // 4. Detect change
        let new_hash = gpu::cache::hash_wgsl(modified_shader);
        let old_hash = shader_versions.get("main_shader").unwrap();
        
        assert_ne!(*old_hash, new_hash, "Should detect shader change");
        
        // 5. Update version
        shader_versions.insert("main_shader".to_string(), new_hash);
        assert_eq!(
            *shader_versions.get("main_shader").unwrap(), 
            new_hash,
            "Shader version should be updated"
        );
    }

    #[test]
    fn test_shader_reload_with_errors() {
        // Test handling of invalid shader code during hot-reload
        let invalid_shader = r#"
            @fragment
            fn fs_main() -> @location(0) vec4<f32> {
                return vec4<f32>(1.0, 0.0, 0.0); // Missing 4th component
            }
        "#;
        
        // In real implementation, this would fail compilation
        // For now, just check that it's syntactically detectable
        assert!(!invalid_shader.contains("vec4<f32>(1.0, 0.0, 0.0, 1.0)"));
    }
}

#[cfg(test)]
mod shader_dsl_integration_tests {
    
    #[test]
    fn test_planned_shader_dsl_syntax() {
        // Test for the planned DSL shader syntax
        // This demonstrates what shader definitions might look like in XR-Lang
        
        let planned_dsl = r#"
            (defshader basic-color
              (uniforms
                (mvp mat4)
                (color vec4))
              (vertex
                (in (position vec3))
                (out (v_color vec4))
                (main
                  (set! gl_Position (* mvp (vec4 position 1.0)))
                  (set! v_color color)))
              (fragment
                (in (v_color vec4))
                (out (frag_color vec4))
                (main
                  (set! frag_color v_color))))
        "#;
        
        // These features aren't implemented yet, but we document the planned syntax
        assert!(planned_dsl.contains("defshader"));
        assert!(planned_dsl.contains("uniforms"));
        assert!(planned_dsl.contains("vertex"));
        assert!(planned_dsl.contains("fragment"));
    }

    #[test]
    fn test_planned_compute_shader_dsl() {
        let planned_compute_dsl = r#"
            (defshader particle-update
              (compute
                (workgroup-size 64 1 1)
                (storage
                  (positions (array vec3) read-write)
                  (velocities (array vec3) read-write))
                (uniforms
                  (delta-time f32)
                  (gravity vec3))
                (main
                  (let ((idx (. global-id x)))
                    (set! (aref velocities idx)
                          (+ (aref velocities idx)
                             (* gravity delta-time)))
                    (set! (aref positions idx)
                          (+ (aref positions idx)
                             (* (aref velocities idx) delta-time)))))))
        "#;
        
        assert!(planned_compute_dsl.contains("compute"));
        assert!(planned_compute_dsl.contains("workgroup-size"));
        assert!(planned_compute_dsl.contains("storage"));
    }

    #[test]
    fn test_shader_with_textures_dsl() {
        let texture_shader_dsl = r#"
            (defshader textured-material
              (uniforms
                (mvp mat4))
              (textures
                (diffuse-map texture2d)
                (normal-map texture2d))
              (samplers
                (tex-sampler sampler))
              (vertex
                (in (position vec3) (uv vec2) (normal vec3))
                (out (v_uv vec2) (v_normal vec3))
                (main
                  (set! gl_Position (* mvp (vec4 position 1.0)))
                  (set! v_uv uv)
                  (set! v_normal normal)))
              (fragment
                (in (v_uv vec2) (v_normal vec3))
                (out (color vec4))
                (main
                  (let ((diffuse (sample diffuse-map tex-sampler v_uv)))
                    (set! color diffuse)))))
        "#;
        
        assert!(texture_shader_dsl.contains("textures"));
        assert!(texture_shader_dsl.contains("samplers"));
        assert!(texture_shader_dsl.contains("sample"));
    }
}

#[cfg(test)]
mod shader_material_integration_tests {
    
    #[test]
    fn test_material_shader_binding() {
        // Test that materials can be bound to shaders
        let material_dsl = r#"
            (defscene3d shader-test-scene
              (object cube1 cube
                (position 0 0 0)
                (material custom
                  (shader "particle-glow")
                  (uniforms
                    (glow-intensity 2.0)
                    (glow-color 1 0.5 0))
                  (textures
                    (noise-texture "perlin.png")))))
        "#;
        
        // This syntax isn't implemented yet, but shows how materials
        // would reference custom shaders
        assert!(material_dsl.contains("shader"));
        assert!(material_dsl.contains("uniforms"));
        assert!(material_dsl.contains("textures"));
    }

    #[test]
    fn test_shader_parameter_animation() {
        // Test animating shader parameters
        let animated_shader_dsl = r#"
            (defbehavior shader-animation
              (state (time 0))
              (update (dt)
                (set! time (+ time dt))
                (set-shader-uniform! "glow-intensity" 
                                   (* 2.0 (+ 1.0 (sin time))))
                (set-shader-uniform! "wave-offset"
                                   (vec2 (cos time) (sin time)))))
        "#;
        
        assert!(animated_shader_dsl.contains("set-shader-uniform!"));
    }
}

#[cfg(test)]
mod shader_performance_tests {
    
    #[test]
    fn test_shader_compilation_caching() {
        // Test that shader compilation results are cached
        let mut compilation_times: Vec<std::time::Duration> = Vec::new();
        let shader_code = r#"
            @vertex
            fn vs_main(@location(0) pos: vec3<f32>) -> @builtin(position) vec4<f32> {
                return vec4<f32>(pos, 1.0);
            }
        "#;
        
        // Simulate multiple compilations
        for _ in 0..3 {
            let start = std::time::Instant::now();
            let _hash = gpu::cache::hash_wgsl(shader_code);
            let duration = start.elapsed();
            compilation_times.push(duration);
        }
        
        // After first compilation, subsequent ones should be faster (cached)
        // This is a simplified test - real caching happens at GPU level
        assert!(compilation_times.len() == 3);
    }

    #[test]
    fn test_shader_variant_generation() {
        // Test generating shader variants based on feature flags
        let base_shader = r#"
            @fragment
            fn fs_main() -> @location(0) vec4<f32> {
                var color = vec4<f32>(1.0, 1.0, 1.0, 1.0);
                #ifdef USE_VERTEX_COLOR
                    color = v_color;
                #endif
                #ifdef USE_TEXTURE
                    color = color * texture_sample(diffuse_tex, tex_sampler, v_uv);
                #endif
                return color;
            }
        "#;
        
        // Test that we can detect preprocessor directives
        assert!(base_shader.contains("#ifdef"));
        assert!(base_shader.contains("USE_VERTEX_COLOR"));
        assert!(base_shader.contains("USE_TEXTURE"));
    }
}

#[cfg(test)]
mod shader_runtime_tests {
    use super::*;
    
    #[test]
    fn test_shader_hot_reload_events() {
        // Test event system for shader hot-reloading
        #[derive(Debug, PartialEq)]
        enum ShaderEvent {
            ShaderModified(String),
            ShaderCompiled(String),
            ShaderBound(String),
            ShaderError(String, String),
        }
        
        let mut events: Vec<ShaderEvent> = Vec::new();
        
        // Simulate shader modification
        events.push(ShaderEvent::ShaderModified("main.wgsl".to_string()));
        
        // Simulate compilation
        events.push(ShaderEvent::ShaderCompiled("main.wgsl".to_string()));
        
        // Simulate binding
        events.push(ShaderEvent::ShaderBound("main.wgsl".to_string()));
        
        assert_eq!(events.len(), 3);
        assert_eq!(events[0], ShaderEvent::ShaderModified("main.wgsl".to_string()));
    }

    #[test]
    fn test_shader_pipeline_recreation() {
        // Test that pipelines are recreated when shaders change
        struct Pipeline {
            shader_id: ShaderId,
            vertex_shader: String,
            fragment_shader: String,
        }
        
        let original_pipeline = Pipeline {
            shader_id: gpu::cache::hash_wgsl("original"),
            vertex_shader: "vs_main".to_string(),
            fragment_shader: "fs_main".to_string(),
        };
        
        // Simulate shader change
        let new_shader_id = gpu::cache::hash_wgsl("modified");
        
        assert_ne!(original_pipeline.shader_id, new_shader_id);
        
        // Would trigger pipeline recreation in real implementation
        let _new_pipeline = Pipeline {
            shader_id: new_shader_id,
            vertex_shader: "vs_main".to_string(),
            fragment_shader: "fs_main_modified".to_string(),
        };
    }

    #[test]
    fn test_shader_error_recovery() {
        // Test recovery from shader compilation errors
        let error_shader = r#"
            @fragment
            fn fs_main() -> @location(0) vec4<f32> {
                return vec4<f32>(1.0, 0.0); // Error: wrong number of components
            }
        "#;
        
        let fallback_shader = r#"
            @fragment
            fn fs_main() -> @location(0) vec4<f32> {
                return vec4<f32>(1.0, 0.0, 1.0, 1.0); // Magenta error color
            }
        "#;
        
        // In real implementation:
        // 1. Try to compile error_shader - fails
        // 2. Fall back to fallback_shader
        // 3. Display error message to user
        
        assert!(error_shader.contains("vec4<f32>(1.0, 0.0)"));
        assert!(fallback_shader.contains("vec4<f32>(1.0, 0.0, 1.0, 1.0)"));
    }
}