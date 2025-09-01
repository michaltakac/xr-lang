//! Code synchronization system for Live mode
//! Writes runtime changes back to DSL source files

use std::fs;
use std::path::{Path, PathBuf};
use std::time::{SystemTime, UNIX_EPOCH};
use anyhow::Result;
use crate::runtime_state::{RuntimeState, AuthoringMode, CameraState};
use crate::scene::SceneData;
use crate::math::Vec3;
use std::collections::HashMap;

pub struct CodeSync {
    source_path: Option<PathBuf>,
    last_manual_edit: Option<SystemTime>,
    sync_buffer_secs: u64,  // Don't sync within N seconds of manual edit
    pending_updates: Vec<DslUpdate>,
}

#[derive(Debug, Clone)]
struct DslUpdate {
    path: Vec<String>,  // Path to the value in the AST (e.g., ["camera", "position"])
    value: String,      // Formatted DSL value
    timestamp: SystemTime,
}

impl CodeSync {
    pub fn new() -> Self {
        Self {
            source_path: None,
            last_manual_edit: None,
            sync_buffer_secs: 5,  // 5 second buffer after manual edits
            pending_updates: Vec::new(),
        }
    }
    
    pub fn set_source_path(&mut self, path: impl AsRef<Path>) {
        self.source_path = Some(path.as_ref().to_path_buf());
    }
    
    /// Check if we should sync (not too soon after manual edit)
    pub fn should_sync(&self) -> bool {
        if let Some(last_edit) = self.last_manual_edit {
            if let Ok(elapsed) = last_edit.elapsed() {
                return elapsed.as_secs() > self.sync_buffer_secs;
            }
        }
        true
    }
    
    /// Queue a camera position update
    pub fn queue_camera_update(&mut self, camera: &CameraState) {
        self.pending_updates.push(DslUpdate {
            path: vec!["camera".to_string(), "position".to_string()],
            value: format_vec3(camera.position),
            timestamp: SystemTime::now(),
        });
        
        self.pending_updates.push(DslUpdate {
            path: vec!["camera".to_string(), "target".to_string()],
            value: format_vec3(camera.target),
            timestamp: SystemTime::now(),
        });
        
        self.pending_updates.push(DslUpdate {
            path: vec!["camera".to_string(), "fov".to_string()],
            value: format!("{:.1}", camera.fov.to_degrees()),
            timestamp: SystemTime::now(),
        });
    }
    
    /// Queue an object transform update
    pub fn queue_object_update(&mut self, object_id: &str, position: Vec3, scale: Vec3) {
        self.pending_updates.push(DslUpdate {
            path: vec!["object".to_string(), object_id.to_string(), "position".to_string()],
            value: format_vec3(position),
            timestamp: SystemTime::now(),
        });
        
        self.pending_updates.push(DslUpdate {
            path: vec!["object".to_string(), object_id.to_string(), "scale".to_string()],
            value: format_vec3(scale),
            timestamp: SystemTime::now(),
        });
    }
    
    /// Sync pending updates to the DSL file
    pub fn sync_to_file(&mut self, runtime_state: &RuntimeState) -> Result<()> {
        // Only sync in Live mode
        if runtime_state.authoring_mode != AuthoringMode::Live {
            return Ok(());
        }
        
        // Check if we should sync (not too soon after manual edit)
        if !self.should_sync() {
            return Ok(());
        }
        
        // No pending updates
        if self.pending_updates.is_empty() {
            return Ok(());
        }
        
        let Some(ref path) = self.source_path else {
            return Ok(());
        };
        
        // Read the current file
        let content = fs::read_to_string(path)?;
        
        // Apply updates to the content
        let updated_content = self.apply_updates_to_dsl(&content)?;
        
        // Write back to file
        fs::write(path, updated_content)?;
        
        // Clear pending updates
        self.pending_updates.clear();
        
        println!("✅ Synced {} updates to {}", 
                 self.pending_updates.len(), 
                 path.display());
        
        Ok(())
    }
    
    /// Apply pending updates to DSL content
    fn apply_updates_to_dsl(&self, content: &str) -> Result<String> {
        let mut lines: Vec<String> = content.lines().map(|s| s.to_string()).collect();
        
        for update in &self.pending_updates {
            // Find and update the relevant line
            // This is a simplified implementation - a real one would parse the AST
            if update.path.len() >= 2 {
                match update.path[0].as_str() {
                    "camera" => {
                        self.update_camera_in_lines(&mut lines, &update.path[1], &update.value)?;
                    }
                    "object" => {
                        if update.path.len() >= 3 {
                            self.update_object_in_lines(
                                &mut lines, 
                                &update.path[1], 
                                &update.path[2], 
                                &update.value
                            )?;
                        }
                    }
                    _ => {}
                }
            }
        }
        
        Ok(lines.join("\n"))
    }
    
    /// Update camera properties in DSL lines
    fn update_camera_in_lines(&self, lines: &mut [String], property: &str, value: &str) -> Result<()> {
        let mut in_camera = false;
        let mut depth = 0;
        
        for line in lines.iter_mut() {
            let trimmed = line.trim();
            
            // Track if we're in camera block
            if trimmed.starts_with("(camera") {
                in_camera = true;
                depth = 1;
                continue;
            }
            
            if in_camera {
                // Track parentheses depth
                for ch in trimmed.chars() {
                    if ch == '(' { depth += 1; }
                    if ch == ')' { depth -= 1; }
                }
                
                // Check if this is the property we want to update
                if trimmed.starts_with(&format!("({}", property)) {
                    // Generate the new line with proper indentation
                    let indent = line.len() - line.trim_start().len();
                    let timestamp = format_timestamp();
                    *line = format!("{}({} {})  ; /* auto: {} */",
                                   " ".repeat(indent),
                                   property,
                                   value,
                                   timestamp);
                }
                
                // Exit camera block
                if depth == 0 {
                    in_camera = false;
                }
            }
        }
        
        Ok(())
    }
    
    /// Update object properties in DSL lines
    fn update_object_in_lines(&self, lines: &mut [String], object_id: &str, property: &str, value: &str) -> Result<()> {
        let mut in_object = false;
        let mut depth = 0;
        
        for line in lines.iter_mut() {
            let trimmed = line.trim();
            
            // Track if we're in the right object block
            if trimmed.starts_with(&format!("(object {}", object_id)) {
                in_object = true;
                depth = 1;
                continue;
            }
            
            if in_object {
                // Track parentheses depth
                for ch in trimmed.chars() {
                    if ch == '(' { depth += 1; }
                    if ch == ')' { depth -= 1; }
                }
                
                // Check if this is the property we want to update
                if trimmed.starts_with(&format!("({}", property)) {
                    // Generate the new line with proper indentation
                    let indent = line.len() - line.trim_start().len();
                    let timestamp = format_timestamp();
                    *line = format!("{}({} {})  ; /* auto: {} */",
                                   " ".repeat(indent),
                                   property,
                                   value,
                                   timestamp);
                }
                
                // Exit object block
                if depth == 0 {
                    in_object = false;
                }
            }
        }
        
        Ok(())
    }
    
    /// Detect manual edits to the file
    pub fn detect_manual_edit(&mut self, path: impl AsRef<Path>) -> Result<()> {
        if let Ok(metadata) = fs::metadata(path) {
            if let Ok(modified) = metadata.modified() {
                // Check if this was a recent modification
                if let Ok(elapsed) = modified.elapsed() {
                    if elapsed.as_secs() < 2 {
                        self.last_manual_edit = Some(SystemTime::now());
                        println!("📝 Manual edit detected, pausing auto-sync");
                    }
                }
            }
        }
        Ok(())
    }
}

/// Format a Vec3 for DSL output
fn format_vec3(v: Vec3) -> String {
    format!("{:.2} {:.2} {:.2}", v.x, v.y, v.z)
}

/// Format current timestamp for comments
fn format_timestamp() -> String {
    let now = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .unwrap_or_default();
    
    // Simple timestamp format
    let secs = now.as_secs();
    let hours = (secs / 3600) % 24;
    let mins = (secs / 60) % 60;
    let secs = secs % 60;
    
    format!("{:02}:{:02}:{:02}", hours, mins, secs)
}

#[cfg(test)]
mod tests {
    use super::*;
    
    #[test]
    fn test_format_vec3() {
        let v = Vec3::new(1.234, 5.678, 9.012);
        assert_eq!(format_vec3(v), "1.23 5.68 9.01");
    }
    
    #[test]
    fn test_should_sync() {
        let mut sync = CodeSync::new();
        
        // Should sync by default
        assert!(sync.should_sync());
        
        // Should not sync right after manual edit
        sync.last_manual_edit = Some(SystemTime::now());
        assert!(!sync.should_sync());
    }
}