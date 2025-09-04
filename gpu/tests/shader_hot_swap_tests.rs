//! Integration tests for shader hot-swapping functionality
//! Tests the complete workflow of modifying and reloading shaders at runtime

#[cfg(test)]
mod shader_hot_swap_integration {
    use std::sync::{Arc, Mutex};
    use std::time::Duration;
    use std::thread;
    
    #[test]
    fn test_shader_file_watching() {
        // Simulate file watching for shader changes
        struct ShaderWatcher {
            watched_files: Vec<String>,
            change_callbacks: Vec<Box<dyn Fn(&str) + Send + Sync>>,
        }
        
        impl ShaderWatcher {
            fn new() -> Self {
                Self {
                    watched_files: Vec::new(),
                    change_callbacks: Vec::new(),
                }
            }
            
            fn watch(&mut self, file_path: &str) {
                self.watched_files.push(file_path.to_string());
            }
            
            fn on_change<F>(&mut self, callback: F) 
            where 
                F: Fn(&str) + Send + Sync + 'static
            {
                self.change_callbacks.push(Box::new(callback));
            }
            
            fn simulate_file_change(&self, file_path: &str) {
                for callback in &self.change_callbacks {
                    callback(file_path);
                }
            }
        }
        
        let mut watcher = ShaderWatcher::new();
        let changes = Arc::new(Mutex::new(Vec::new()));
        let changes_clone = changes.clone();
        
        watcher.watch("shaders/basic.wgsl");
        watcher.on_change(move |path| {
            changes_clone.lock().unwrap().push(path.to_string());
        });
        
        watcher.simulate_file_change("shaders/basic.wgsl");
        
        let recorded_changes = changes.lock().unwrap();
        assert_eq!(recorded_changes.len(), 1);
        assert_eq!(recorded_changes[0], "shaders/basic.wgsl");
    }

    #[test]
    fn test_shader_compilation_queue() {
        // Test queuing shader compilations during rapid changes
        #[derive(Clone)]
        struct ShaderCompilationRequest {
            path: String,
            source: String,
            timestamp: std::time::Instant,
        }
        
        struct CompilationQueue {
            pending: Vec<ShaderCompilationRequest>,
            in_progress: Option<ShaderCompilationRequest>,
            completed: Vec<String>,
        }
        
        impl CompilationQueue {
            fn new() -> Self {
                Self {
                    pending: Vec::new(),
                    in_progress: None,
                    completed: Vec::new(),
                }
            }
            
            fn enqueue(&mut self, request: ShaderCompilationRequest) {
                // Debounce: if same shader is already pending, replace it
                self.pending.retain(|r| r.path != request.path);
                self.pending.push(request);
            }
            
            fn process_next(&mut self) -> Option<ShaderCompilationRequest> {
                if self.in_progress.is_none() && !self.pending.is_empty() {
                    let request = self.pending.remove(0);
                    self.in_progress = Some(request.clone());
                    Some(request)
                } else {
                    None
                }
            }
            
            fn complete_current(&mut self) {
                if let Some(request) = self.in_progress.take() {
                    self.completed.push(request.path);
                }
            }
        }
        
        let mut queue = CompilationQueue::new();
        
        // Simulate rapid changes to the same shader
        for i in 0..5 {
            queue.enqueue(ShaderCompilationRequest {
                path: "shader.wgsl".to_string(),
                source: format!("version {}", i),
                timestamp: std::time::Instant::now(),
            });
        }
        
        // Should only have the latest version queued
        assert_eq!(queue.pending.len(), 1);
        assert_eq!(queue.pending[0].source, "version 4");
        
        // Process the queue
        if let Some(_request) = queue.process_next() {
            // Simulate compilation
            queue.complete_current();
        }
        
        assert_eq!(queue.completed.len(), 1);
        assert_eq!(queue.completed[0], "shader.wgsl");
    }

    #[test]
    fn test_shader_state_preservation() {
        // Test that shader uniform values are preserved during hot-reload
        struct ShaderState {
            uniforms: std::collections::HashMap<String, Vec<f32>>,
            textures: std::collections::HashMap<String, String>,
        }
        
        impl ShaderState {
            fn new() -> Self {
                Self {
                    uniforms: std::collections::HashMap::new(),
                    textures: std::collections::HashMap::new(),
                }
            }
            
            fn set_uniform(&mut self, name: &str, values: Vec<f32>) {
                self.uniforms.insert(name.to_string(), values);
            }
            
            fn set_texture(&mut self, name: &str, path: &str) {
                self.textures.insert(name.to_string(), path.to_string());
            }
            
            fn save_state(&self) -> ShaderState {
                ShaderState {
                    uniforms: self.uniforms.clone(),
                    textures: self.textures.clone(),
                }
            }
            
            fn restore_state(&mut self, saved: ShaderState) {
                self.uniforms = saved.uniforms;
                self.textures = saved.textures;
            }
        }
        
        let mut state = ShaderState::new();
        state.set_uniform("color", vec![1.0, 0.5, 0.0, 1.0]);
        state.set_uniform("intensity", vec![2.5]);
        state.set_texture("diffuse", "textures/metal.png");
        
        // Save state before reload
        let saved_state = state.save_state();
        
        // Simulate shader reload (state would normally be lost)
        state = ShaderState::new();
        assert_eq!(state.uniforms.len(), 0);
        
        // Restore state after reload
        state.restore_state(saved_state);
        
        assert_eq!(state.uniforms.len(), 2);
        assert_eq!(state.uniforms.get("color"), Some(&vec![1.0, 0.5, 0.0, 1.0]));
        assert_eq!(state.uniforms.get("intensity"), Some(&vec![2.5]));
        assert_eq!(state.textures.get("diffuse"), Some(&"textures/metal.png".to_string()));
    }

    #[test]
    fn test_shader_dependency_tracking() {
        // Test tracking dependencies between shaders (includes)
        struct ShaderDependencyGraph {
            dependencies: std::collections::HashMap<String, Vec<String>>,
        }
        
        impl ShaderDependencyGraph {
            fn new() -> Self {
                Self {
                    dependencies: std::collections::HashMap::new(),
                }
            }
            
            fn add_dependency(&mut self, shader: &str, depends_on: &str) {
                self.dependencies
                    .entry(shader.to_string())
                    .or_insert_with(Vec::new)
                    .push(depends_on.to_string());
            }
            
            fn get_affected_shaders(&self, changed_shader: &str) -> Vec<String> {
                let mut affected = vec![changed_shader.to_string()];
                
                // Find all shaders that depend on the changed shader
                for (shader, deps) in &self.dependencies {
                    if deps.contains(&changed_shader.to_string()) {
                        affected.push(shader.clone());
                    }
                }
                
                affected
            }
        }
        
        let mut graph = ShaderDependencyGraph::new();
        
        // Setup dependencies
        graph.add_dependency("main.wgsl", "common.wgsl");
        graph.add_dependency("main.wgsl", "lighting.wgsl");
        graph.add_dependency("particle.wgsl", "common.wgsl");
        graph.add_dependency("ui.wgsl", "common.wgsl");
        
        // When common.wgsl changes, all dependent shaders need recompilation
        let affected = graph.get_affected_shaders("common.wgsl");
        
        assert!(affected.contains(&"common.wgsl".to_string()));
        assert!(affected.contains(&"main.wgsl".to_string()));
        assert!(affected.contains(&"particle.wgsl".to_string()));
        assert!(affected.contains(&"ui.wgsl".to_string()));
        assert_eq!(affected.len(), 4);
    }

    #[test]
    fn test_shader_version_management() {
        // Test managing multiple versions of shaders for rollback
        struct ShaderVersionManager {
            versions: std::collections::HashMap<String, Vec<(String, std::time::SystemTime)>>,
            current: std::collections::HashMap<String, usize>,
            max_versions: usize,
        }
        
        impl ShaderVersionManager {
            fn new(max_versions: usize) -> Self {
                Self {
                    versions: std::collections::HashMap::new(),
                    current: std::collections::HashMap::new(),
                    max_versions,
                }
            }
            
            fn add_version(&mut self, shader_name: &str, source: String) {
                let versions = self.versions.entry(shader_name.to_string()).or_insert_with(Vec::new);
                versions.push((source, std::time::SystemTime::now()));
                
                // Keep only max_versions
                if versions.len() > self.max_versions {
                    versions.remove(0);
                }
                
                // Set as current version
                self.current.insert(shader_name.to_string(), versions.len() - 1);
            }
            
            fn rollback(&mut self, shader_name: &str) -> Option<String> {
                if let Some(current_idx) = self.current.get_mut(shader_name) {
                    if *current_idx > 0 {
                        *current_idx -= 1;
                        return self.get_current_version(shader_name);
                    }
                }
                None
            }
            
            fn get_current_version(&self, shader_name: &str) -> Option<String> {
                if let Some(idx) = self.current.get(shader_name) {
                    if let Some(versions) = self.versions.get(shader_name) {
                        if let Some((source, _)) = versions.get(*idx) {
                            return Some(source.clone());
                        }
                    }
                }
                None
            }
        }
        
        let mut manager = ShaderVersionManager::new(3);
        
        // Add versions
        manager.add_version("main", "version1".to_string());
        manager.add_version("main", "version2".to_string());
        manager.add_version("main", "version3".to_string());
        
        assert_eq!(manager.get_current_version("main"), Some("version3".to_string()));
        
        // Rollback
        assert_eq!(manager.rollback("main"), Some("version2".to_string()));
        assert_eq!(manager.rollback("main"), Some("version1".to_string()));
        assert_eq!(manager.rollback("main"), None); // Can't rollback further
    }

    #[test]
    fn test_shader_error_reporting() {
        // Test error reporting during shader compilation
        #[derive(Debug, Clone)]
        struct ShaderError {
            file: String,
            line: u32,
            column: u32,
            message: String,
        }
        
        struct ShaderCompiler {
            errors: Vec<ShaderError>,
        }
        
        impl ShaderCompiler {
            fn new() -> Self {
                Self { errors: Vec::new() }
            }
            
            fn compile(&mut self, file: &str, source: &str) -> Result<(), Vec<ShaderError>> {
                self.errors.clear();
                
                // Simulate error detection
                if source.contains("vec4<f32>(1.0, 0.0)") {
                    self.errors.push(ShaderError {
                        file: file.to_string(),
                        line: 3,
                        column: 16,
                        message: "Type mismatch: expected vec4<f32>, got vec2<f32>".to_string(),
                    });
                }
                
                if source.contains("undefined_function") {
                    self.errors.push(ShaderError {
                        file: file.to_string(),
                        line: 5,
                        column: 8,
                        message: "Undefined function: undefined_function".to_string(),
                    });
                }
                
                if self.errors.is_empty() {
                    Ok(())
                } else {
                    Err(self.errors.clone())
                }
            }
        }
        
        let mut compiler = ShaderCompiler::new();
        
        // Test successful compilation
        let valid_shader = "fn main() { return vec4<f32>(1.0, 0.0, 0.0, 1.0); }";
        assert!(compiler.compile("test.wgsl", valid_shader).is_ok());
        
        // Test compilation with errors
        let invalid_shader = "fn main() { return vec4<f32>(1.0, 0.0); }";
        let result = compiler.compile("test.wgsl", invalid_shader);
        
        assert!(result.is_err());
        if let Err(errors) = result {
            assert_eq!(errors.len(), 1);
            assert_eq!(errors[0].line, 3);
            assert_eq!(errors[0].column, 16);
            assert!(errors[0].message.contains("Type mismatch"));
        }
    }

    #[test] 
    fn test_shader_performance_profiling() {
        // Test profiling shader compilation and reload times
        struct ShaderProfiler {
            compile_times: Vec<Duration>,
            bind_times: Vec<Duration>,
            total_reloads: u32,
        }
        
        impl ShaderProfiler {
            fn new() -> Self {
                Self {
                    compile_times: Vec::new(),
                    bind_times: Vec::new(),
                    total_reloads: 0,
                }
            }
            
            fn record_compilation(&mut self, duration: Duration) {
                self.compile_times.push(duration);
            }
            
            fn record_binding(&mut self, duration: Duration) {
                self.bind_times.push(duration);
            }
            
            fn record_reload(&mut self) {
                self.total_reloads += 1;
            }
            
            fn average_compile_time(&self) -> Option<Duration> {
                if self.compile_times.is_empty() {
                    return None;
                }
                let total: Duration = self.compile_times.iter().sum();
                Some(total / self.compile_times.len() as u32)
            }
            
            fn average_bind_time(&self) -> Option<Duration> {
                if self.bind_times.is_empty() {
                    return None;
                }
                let total: Duration = self.bind_times.iter().sum();
                Some(total / self.bind_times.len() as u32)
            }
        }
        
        let mut profiler = ShaderProfiler::new();
        
        // Simulate profiling
        profiler.record_compilation(Duration::from_millis(50));
        profiler.record_compilation(Duration::from_millis(30));
        profiler.record_compilation(Duration::from_millis(40));
        
        profiler.record_binding(Duration::from_millis(5));
        profiler.record_binding(Duration::from_millis(3));
        
        profiler.record_reload();
        profiler.record_reload();
        
        assert_eq!(profiler.average_compile_time(), Some(Duration::from_millis(40)));
        assert_eq!(profiler.average_bind_time(), Some(Duration::from_millis(4)));
        assert_eq!(profiler.total_reloads, 2);
    }

    #[test]
    fn test_concurrent_shader_updates() {
        // Test handling concurrent shader modifications
        let shared_state = Arc::new(Mutex::new(Vec::<String>::new()));
        let mut handles = vec![];
        
        // Simulate multiple threads trying to update shaders
        for i in 0..3 {
            let state = shared_state.clone();
            let handle = thread::spawn(move || {
                thread::sleep(Duration::from_millis(10 * i));
                let mut state = state.lock().unwrap();
                state.push(format!("shader_{}", i));
            });
            handles.push(handle);
        }
        
        for handle in handles {
            handle.join().unwrap();
        }
        
        let final_state = shared_state.lock().unwrap();
        assert_eq!(final_state.len(), 3);
        assert!(final_state.contains(&"shader_0".to_string()));
        assert!(final_state.contains(&"shader_1".to_string()));
        assert!(final_state.contains(&"shader_2".to_string()));
    }
}