//! Minimal Scene Differ
//! 
//! Simple differ for detecting changes between scenes without complex reconciliation.
//! Focuses on clarity and predictability over clever algorithms.

use std::collections::{HashMap, HashSet};
use crate::value::Value;

/// Types of changes detected between scenes
#[derive(Debug, Clone, PartialEq)]
pub enum SceneChange {
    /// Object added to scene
    Added { id: String, value: Value },
    
    /// Object removed from scene
    Removed { id: String, value: Value },
    
    /// Object property modified
    Modified {
        id: String,
        property: String,
        old_value: Value,
        new_value: Value,
    },
    
    /// Object type changed (requires full replacement)
    TypeChanged {
        id: String,
        old_type: String,
        new_type: String,
        new_value: Value,
    },
}

/// Result of scene diffing operation
#[derive(Debug, Default)]
pub struct DiffResult {
    pub changes: Vec<SceneChange>,
    pub added_ids: HashSet<String>,
    pub removed_ids: HashSet<String>,
    pub modified_ids: HashSet<String>,
}

impl DiffResult {
    pub fn has_changes(&self) -> bool {
        !self.changes.is_empty()
    }
    
    pub fn change_count(&self) -> usize {
        self.changes.len()
    }
}

/// Simple scene differ for detecting changes
pub struct SceneDiffer;

impl SceneDiffer {
    pub fn new() -> Self {
        Self
    }
    
    /// Compare two scenes and return differences
    pub fn diff(
        &self,
        old_scene: &HashMap<String, Value>,
        new_scene: &HashMap<String, Value>,
    ) -> DiffResult {
        let mut result = DiffResult::default();
        
        // Check for removed objects
        for (id, old_value) in old_scene {
            if !new_scene.contains_key(id) {
                result.changes.push(SceneChange::Removed {
                    id: id.clone(),
                    value: old_value.clone(),
                });
                result.removed_ids.insert(id.clone());
            }
        }
        
        // Check for added and modified objects
        for (id, new_value) in new_scene {
            if let Some(old_value) = old_scene.get(id) {
                // Object exists in both - check for modifications
                self.diff_objects(&mut result, id, old_value, new_value);
            } else {
                // New object added
                result.changes.push(SceneChange::Added {
                    id: id.clone(),
                    value: new_value.clone(),
                });
                result.added_ids.insert(id.clone());
            }
        }
        
        result
    }
    
    /// Compare two objects and detect property changes
    fn diff_objects(
        &self,
        result: &mut DiffResult,
        id: &str,
        old_value: &Value,
        new_value: &Value,
    ) {
        // Check if types match
        if !self.same_type(old_value, new_value) {
            result.changes.push(SceneChange::TypeChanged {
                id: id.to_string(),
                old_type: self.get_type_name(old_value),
                new_type: self.get_type_name(new_value),
                new_value: new_value.clone(),
            });
            result.modified_ids.insert(id.to_string());
            return;
        }
        
        // Compare properties for Map values
        if let (Value::Map(old_props), Value::Map(new_props)) = (old_value, new_value) {
            let mut object_modified = false;
            
            // Check for removed properties
            for (prop_name, old_prop_value) in old_props {
                if !new_props.contains_key(prop_name) {
                    result.changes.push(SceneChange::Modified {
                        id: id.to_string(),
                        property: prop_name.clone(),
                        old_value: old_prop_value.clone(),
                        new_value: Value::Nil,
                    });
                    object_modified = true;
                }
            }
            
            // Check for added or modified properties
            for (prop_name, new_prop_value) in new_props {
                if let Some(old_prop_value) = old_props.get(prop_name) {
                    if !self.values_equal(old_prop_value, new_prop_value) {
                        result.changes.push(SceneChange::Modified {
                            id: id.to_string(),
                            property: prop_name.clone(),
                            old_value: old_prop_value.clone(),
                            new_value: new_prop_value.clone(),
                        });
                        object_modified = true;
                    }
                } else {
                    // New property added
                    result.changes.push(SceneChange::Modified {
                        id: id.to_string(),
                        property: prop_name.clone(),
                        old_value: Value::Nil,
                        new_value: new_prop_value.clone(),
                    });
                    object_modified = true;
                }
            }
            
            if object_modified {
                result.modified_ids.insert(id.to_string());
            }
        } else if !self.values_equal(old_value, new_value) {
            // For non-Map values, treat as complete replacement
            result.changes.push(SceneChange::Modified {
                id: id.to_string(),
                property: "value".to_string(),
                old_value: old_value.clone(),
                new_value: new_value.clone(),
            });
            result.modified_ids.insert(id.to_string());
        }
    }
    
    /// Check if two values have the same type
    fn same_type(&self, a: &Value, b: &Value) -> bool {
        match (a, b) {
            (Value::Nil, Value::Nil) => true,
            (Value::Bool(_), Value::Bool(_)) => true,
            (Value::Int(_), Value::Int(_)) => true,
            (Value::Float(_), Value::Float(_)) => true,
            (Value::Str(_), Value::Str(_)) => true,
            (Value::Symbol(_), Value::Symbol(_)) => true,
            (Value::Keyword(_), Value::Keyword(_)) => true,
            (Value::List(_), Value::List(_)) => true,
            (Value::Vector(_), Value::Vector(_)) => true,
            (Value::Map(_), Value::Map(_)) => true,
            (Value::Set(_), Value::Set(_)) => true,
            (Value::Object(_), Value::Object(_)) => true,
            _ => false,
        }
    }
    
    /// Get human-readable type name
    fn get_type_name(&self, value: &Value) -> String {
        match value {
            Value::Nil => "nil".to_string(),
            Value::Bool(_) => "bool".to_string(),
            Value::Int(_) => "int".to_string(),
            Value::Float(_) => "float".to_string(),
            Value::Str(_) => "string".to_string(),
            Value::Symbol(_) => "symbol".to_string(),
            Value::Keyword(_) => "keyword".to_string(),
            Value::List(_) => "list".to_string(),
            Value::Vector(_) => "vector".to_string(),
            Value::Map(_) => "map".to_string(),
            Value::Set(_) => "set".to_string(),
            Value::Object(_) => "object".to_string(),
            Value::Quote(_) => "quote".to_string(),
            Value::AST(_) => "ast".to_string(),
            Value::Closure(_) => "closure".to_string(),
            Value::Macro(_) => "macro".to_string(),
            Value::NativeFunction(_) => "native-function".to_string(),
            Value::Capability(_) => "capability".to_string(),
            Value::Channel(_) => "channel".to_string(),
            Value::WithMeta { .. } => "with-meta".to_string(),
        }
    }
    
    /// Deep equality check for values
    fn values_equal(&self, a: &Value, b: &Value) -> bool {
        match (a, b) {
            (Value::Nil, Value::Nil) => true,
            (Value::Bool(a), Value::Bool(b)) => a == b,
            (Value::Int(a), Value::Int(b)) => a == b,
            (Value::Float(a), Value::Float(b)) => (a - b).abs() < f64::EPSILON,
            (Value::Str(a), Value::Str(b)) => a == b,
            (Value::Symbol(a), Value::Symbol(b)) => a.0 == b.0,
            (Value::Keyword(a), Value::Keyword(b)) => a.0 == b.0,
            
            (Value::List(a), Value::List(b)) => {
                a.len() == b.len() && 
                a.iter().zip(b.iter()).all(|(av, bv)| self.values_equal(av, bv))
            }
            
            (Value::Vector(a), Value::Vector(b)) => {
                a.len() == b.len() &&
                a.iter().zip(b.iter()).all(|(av, bv)| self.values_equal(av, bv))
            }
            
            (Value::Map(a), Value::Map(b)) => {
                a.len() == b.len() &&
                a.iter().all(|(k, v)| {
                    b.get(k).map_or(false, |bv| self.values_equal(v, bv))
                })
            }
            
            (Value::Set(a), Value::Set(b)) => {
                a.len() == b.len() &&
                a.iter().all(|av| b.iter().any(|bv| self.values_equal(av, bv)))
            }
            
            (Value::Object(a), Value::Object(b)) => a.0 == b.0,
            
            _ => false,
        }
    }
    
    /// Apply a set of changes to a scene
    pub fn apply_changes(
        scene: &mut HashMap<String, Value>,
        changes: &[SceneChange],
    ) {
        for change in changes {
            match change {
                SceneChange::Added { id, value } => {
                    scene.insert(id.clone(), value.clone());
                }
                
                SceneChange::Removed { id, .. } => {
                    scene.remove(id);
                }
                
                SceneChange::Modified { id, property, new_value, .. } => {
                    if let Some(Value::Map(props)) = scene.get_mut(id) {
                        if *new_value == Value::Nil {
                            props.remove(property);
                        } else {
                            props.insert(property.clone(), new_value.clone());
                        }
                    }
                }
                
                SceneChange::TypeChanged { id, new_value, .. } => {
                    scene.insert(id.clone(), new_value.clone());
                }
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    
    fn create_test_scene() -> HashMap<String, Value> {
        let mut scene = HashMap::new();
        
        let mut cube = HashMap::new();
        cube.insert("type".to_string(), Value::Str("cube".to_string()));
        cube.insert("position".to_string(), Value::Vector(vec![
            Value::Float(0.0),
            Value::Float(0.0),
            Value::Float(0.0),
        ]));
        scene.insert("cube1".to_string(), Value::Map(cube));
        
        scene
    }
    
    #[test]
    fn test_no_changes() {
        let differ = SceneDiffer::new();
        let scene = create_test_scene();
        let result = differ.diff(&scene, &scene);
        
        assert!(!result.has_changes());
        assert_eq!(result.change_count(), 0);
    }
    
    #[test]
    fn test_added_object() {
        let differ = SceneDiffer::new();
        let old_scene = create_test_scene();
        let mut new_scene = old_scene.clone();
        
        // Add a sphere
        let mut sphere = HashMap::new();
        sphere.insert("type".to_string(), Value::Str("sphere".to_string()));
        sphere.insert("radius".to_string(), Value::Float(1.0));
        new_scene.insert("sphere1".to_string(), Value::Map(sphere));
        
        let result = differ.diff(&old_scene, &new_scene);
        
        assert!(result.has_changes());
        assert_eq!(result.change_count(), 1);
        assert!(result.added_ids.contains("sphere1"));
        
        match &result.changes[0] {
            SceneChange::Added { id, .. } => assert_eq!(id, "sphere1"),
            _ => panic!("Expected Added change"),
        }
    }
    
    #[test]
    fn test_removed_object() {
        let differ = SceneDiffer::new();
        let old_scene = create_test_scene();
        let new_scene = HashMap::new();
        
        let result = differ.diff(&old_scene, &new_scene);
        
        assert!(result.has_changes());
        assert_eq!(result.change_count(), 1);
        assert!(result.removed_ids.contains("cube1"));
        
        match &result.changes[0] {
            SceneChange::Removed { id, .. } => assert_eq!(id, "cube1"),
            _ => panic!("Expected Removed change"),
        }
    }
    
    #[test]
    fn test_modified_property() {
        let differ = SceneDiffer::new();
        let old_scene = create_test_scene();
        let mut new_scene = old_scene.clone();
        
        // Modify cube position
        if let Some(Value::Map(cube)) = new_scene.get_mut("cube1") {
            cube.insert("position".to_string(), Value::Vector(vec![
                Value::Float(5.0),
                Value::Float(0.0),
                Value::Float(0.0),
            ]));
        }
        
        let result = differ.diff(&old_scene, &new_scene);
        
        assert!(result.has_changes());
        assert_eq!(result.change_count(), 1);
        assert!(result.modified_ids.contains("cube1"));
        
        match &result.changes[0] {
            SceneChange::Modified { id, property, .. } => {
                assert_eq!(id, "cube1");
                assert_eq!(property, "position");
            }
            _ => panic!("Expected Modified change"),
        }
    }
    
    #[test]
    fn test_type_changed() {
        let differ = SceneDiffer::new();
        let old_scene = create_test_scene();
        let mut new_scene = HashMap::new();
        
        // Replace Map with a different type
        new_scene.insert("cube1".to_string(), Value::Str("not a cube anymore".to_string()));
        
        let result = differ.diff(&old_scene, &new_scene);
        
        assert!(result.has_changes());
        assert_eq!(result.change_count(), 1);
        
        match &result.changes[0] {
            SceneChange::TypeChanged { id, old_type, new_type, .. } => {
                assert_eq!(id, "cube1");
                assert_eq!(old_type, "map");
                assert_eq!(new_type, "string");
            }
            _ => panic!("Expected TypeChanged change"),
        }
    }
    
    #[test]
    fn test_apply_changes() {
        let mut scene = create_test_scene();
        
        let changes = vec![
            SceneChange::Added {
                id: "sphere1".to_string(),
                value: Value::Map(HashMap::from([
                    ("type".to_string(), Value::Str("sphere".to_string())),
                ])),
            },
            SceneChange::Modified {
                id: "cube1".to_string(),
                property: "color".to_string(),
                old_value: Value::Nil,
                new_value: Value::Str("blue".to_string()),
            },
        ];
        
        SceneDiffer::apply_changes(&mut scene, &changes);
        
        // Check sphere was added
        assert!(scene.contains_key("sphere1"));
        
        // Check cube color was added
        if let Some(Value::Map(cube)) = scene.get("cube1") {
            assert_eq!(cube.get("color"), Some(&Value::Str("blue".to_string())));
        } else {
            panic!("Cube not found");
        }
    }
    
    #[test]
    fn test_multiple_changes() {
        let differ = SceneDiffer::new();
        let old_scene = create_test_scene();
        let mut new_scene = HashMap::new();
        
        // Add new object
        let mut sphere = HashMap::new();
        sphere.insert("type".to_string(), Value::Str("sphere".to_string()));
        new_scene.insert("sphere1".to_string(), Value::Map(sphere));
        
        // Modify existing object
        let mut cube = HashMap::new();
        cube.insert("type".to_string(), Value::Str("cube".to_string()));
        cube.insert("position".to_string(), Value::Vector(vec![
            Value::Float(10.0),
            Value::Float(0.0),
            Value::Float(0.0),
        ]));
        cube.insert("color".to_string(), Value::Str("green".to_string()));
        new_scene.insert("cube1".to_string(), Value::Map(cube));
        
        let result = differ.diff(&old_scene, &new_scene);
        
        assert!(result.has_changes());
        assert_eq!(result.added_ids.len(), 1);
        assert_eq!(result.modified_ids.len(), 1);
        assert!(result.added_ids.contains("sphere1"));
        assert!(result.modified_ids.contains("cube1"));
    }
}