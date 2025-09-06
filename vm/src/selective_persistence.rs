//! Selective Persistence Policies for XR-Lang
//! 
//! Fine-grained control over what state gets preserved during hot-reload,
//! time-travel debugging, and session persistence. This completes Stage B
//! by providing metacircular control over the persistence layer.

use crate::value::{Value, Symbol, Keyword};
use crate::persistence::{State, ValuePath, Change, JournalEntry, Journal};
use std::collections::{HashMap, HashSet};
use serde::{Serialize, Deserialize};

/// Persistence scope defines when state should be preserved
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub enum PersistenceScope {
    /// Never persist - always reset to initial value
    Never,
    /// Persist only during hot-reload within the same session
    HotReload,
    /// Persist across time-travel debugging
    TimeTravel,
    /// Persist across application sessions
    Session,
    /// Persist permanently to disk
    Permanent,
}

/// Persistence policy for a specific value or path
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct PersistencePolicy {
    /// Path to the value (e.g., ["scene", "camera", "position"])
    pub path: Vec<String>,
    /// Scope of persistence
    pub scope: PersistenceScope,
    /// Optional filter function (as XR-Lang code) to determine if value should persist
    pub filter: Option<String>,
    /// Whether to encrypt when persisting to disk
    pub encrypt: bool,
    /// Version number for migration
    pub version: u32,
}

/// Manages selective persistence policies
pub struct SelectivePersistence {
    /// Active policies by path pattern
    policies: HashMap<String, PersistencePolicy>,
    /// Default policy for unmatched paths
    default_policy: PersistenceScope,
    /// Paths that have been explicitly marked as dirty
    dirty_paths: HashSet<Vec<String>>,
    /// Migration functions for versioned data
    migrations: HashMap<(u32, u32), Box<dyn Fn(&Value) -> Value>>,
}

impl SelectivePersistence {
    pub fn new() -> Self {
        Self {
            policies: HashMap::new(),
            default_policy: PersistenceScope::HotReload,
            dirty_paths: HashSet::new(),
            migrations: HashMap::new(),
        }
    }

    /// Register a persistence policy
    pub fn register_policy(&mut self, policy: PersistencePolicy) {
        let path_key = policy.path.join(".");
        self.policies.insert(path_key, policy);
    }

    /// Set the default persistence scope
    pub fn set_default_scope(&mut self, scope: PersistenceScope) {
        self.default_policy = scope;
    }

    /// Mark a path as dirty (needs persistence)
    pub fn mark_dirty(&mut self, path: Vec<String>) {
        self.dirty_paths.insert(path);
    }

    /// Check if a value at path should be persisted for given scope
    pub fn should_persist(&self, path: &[String], scope: PersistenceScope) -> bool {
        let path_key = path.join(".");
        
        // Check for exact policy match
        if let Some(policy) = self.policies.get(&path_key) {
            return self.scope_includes(policy.scope, scope);
        }

        // Check for pattern matches (e.g., "scene.*" matches "scene.camera.position")
        for (pattern, policy) in &self.policies {
            if self.matches_pattern(pattern, &path_key) {
                return self.scope_includes(policy.scope, scope);
            }
        }

        // Use default policy
        self.scope_includes(self.default_policy, scope)
    }

    /// Check if a scope includes another scope
    fn scope_includes(&self, policy_scope: PersistenceScope, query_scope: PersistenceScope) -> bool {
        use PersistenceScope::*;
        match (policy_scope, query_scope) {
            (Never, _) => false,
            (HotReload, HotReload) => true,
            (TimeTravel, HotReload) | (TimeTravel, TimeTravel) => true,
            (Session, HotReload) | (Session, TimeTravel) | (Session, Session) => true,
            (Permanent, _) => true,
            _ => false,
        }
    }

    /// Check if a path matches a pattern
    fn matches_pattern(&self, pattern: &str, path: &str) -> bool {
        if pattern.ends_with("*") {
            let prefix = &pattern[..pattern.len() - 1];
            path.starts_with(prefix)
        } else {
            pattern == path
        }
    }

    /// Filter state based on persistence policies
    pub fn filter_state(&self, state: &State, scope: PersistenceScope) -> State {
        let mut filtered = State::new();
        
        // Filter the values HashMap based on policies
        for (path, value) in state.iter() {
            let path_segments: Vec<String> = path.0.clone();
            if self.should_persist(&path_segments, scope) {
                filtered.set(path.clone(), value.clone());
            }
        }
        
        filtered
    }


    /// Apply migrations to persisted data
    pub fn migrate(&self, value: &Value, from_version: u32, to_version: u32) -> Value {
        if from_version == to_version {
            return value.clone();
        }

        // Apply migrations in sequence
        let mut current = value.clone();
        for version in from_version..to_version {
            let key = (version, version + 1);
            if let Some(migration) = self.migrations.get(&key) {
                current = migration(&current);
            }
        }
        current
    }

    /// Register a migration function
    pub fn register_migration(&mut self, from: u32, to: u32, migration: Box<dyn Fn(&Value) -> Value>) {
        self.migrations.insert((from, to), migration);
    }

    /// Get all dirty paths and clear the dirty set
    pub fn take_dirty_paths(&mut self) -> HashSet<Vec<String>> {
        std::mem::take(&mut self.dirty_paths)
    }

    /// Create persistence policy from XR-Lang metadata
    pub fn policy_from_meta(&self, meta: &Value) -> Option<PersistencePolicy> {
        match meta {
            Value::Map(map) => {
                let path = self.extract_path(map.get("path")?)?;
                let scope = self.extract_scope(map.get("persist")?)?;
                
                Some(PersistencePolicy {
                    path,
                    scope,
                    filter: self.extract_string(map.get("filter")),
                    encrypt: self.extract_bool(map.get("encrypt")),
                    version: self.extract_int(map.get("version")).unwrap_or(1) as u32,
                })
            }
            _ => None,
        }
    }

    fn extract_path(&self, value: &Value) -> Option<Vec<String>> {
        match value {
            Value::List(items) => {
                items.iter()
                    .map(|v| match v {
                        Value::Symbol(Symbol(s)) => Some(s.clone()),
                        Value::Str(s) => Some(s.clone()),
                        _ => None,
                    })
                    .collect()
            }
            Value::Symbol(Symbol(s)) => Some(vec![s.clone()]),
            Value::Str(s) => Some(vec![s.clone()]),
            _ => None,
        }
    }

    fn extract_scope(&self, value: &Value) -> Option<PersistenceScope> {
        match value {
            Value::Keyword(Keyword(k)) => match k.as_str() {
                "never" => Some(PersistenceScope::Never),
                "hot-reload" => Some(PersistenceScope::HotReload),
                "time-travel" => Some(PersistenceScope::TimeTravel),
                "session" => Some(PersistenceScope::Session),
                "permanent" => Some(PersistenceScope::Permanent),
                _ => None,
            },
            _ => None,
        }
    }

    fn extract_string(&self, value: Option<&Value>) -> Option<String> {
        value.and_then(|v| match v {
            Value::Str(s) => Some(s.clone()),
            _ => None,
        })
    }

    fn extract_bool(&self, value: Option<&Value>) -> bool {
        value.map_or(false, |v| matches!(v, Value::Bool(true)))
    }

    fn extract_int(&self, value: Option<&Value>) -> Option<i64> {
        value.and_then(|v| match v {
            Value::Int(i) => Some(*i),
            _ => None,
        })
    }
}

/// Integration with the persistence layer
impl SelectivePersistence {
    /// Apply selective persistence to a journal
    pub fn filter_journal(&self, journal: &Journal, scope: PersistenceScope) -> Vec<JournalEntry> {
        let mut filtered = Vec::new();
        
        for entry in journal.get_range(0, u64::MAX) {
            // Check if this entry's path should be persisted
            if let Some(path) = self.extract_entry_path(&entry.change) {
                if self.should_persist(&path, scope) {
                    filtered.push(entry.clone());
                }
            }
        }
        
        filtered
    }

    fn extract_entry_path(&self, change: &Change) -> Option<Vec<String>> {
        // Extract path from journal entry change
        match change {
            Change::Create { path, .. } |
            Change::Update { path, .. } |
            Change::Delete { path, .. } => Some(path.0.clone()),
            Change::Move { from, .. } => Some(from.0.clone()),
        }
    }

    /// Create a checkpoint with selective persistence
    pub fn create_checkpoint(&self, state: &State, scope: PersistenceScope) -> State {
        self.filter_state(state, scope)
    }

    /// Merge persisted state with current state
    pub fn merge_states(&self, persisted: &State, current: &State, scope: PersistenceScope) -> State {
        let mut merged = State::new();
        
        // First, copy all current state
        for (path, value) in current.iter() {
            merged.set(path.clone(), value.clone());
        }
        
        // Then overwrite with persisted values that should be preserved
        for (path, value) in persisted.iter() {
            let path_segments: Vec<String> = path.0.clone();
            if self.should_persist(&path_segments, scope) {
                merged.set(path.clone(), value.clone());
            }
        }
        
        merged
    }
}

/// Builder for creating persistence policies from XR-Lang code
pub struct PolicyBuilder {
    path: Vec<String>,
    scope: PersistenceScope,
    filter: Option<String>,
    encrypt: bool,
    version: u32,
}

impl PolicyBuilder {
    pub fn new(path: Vec<String>) -> Self {
        Self {
            path,
            scope: PersistenceScope::HotReload,
            filter: None,
            encrypt: false,
            version: 1,
        }
    }

    pub fn with_scope(mut self, scope: PersistenceScope) -> Self {
        self.scope = scope;
        self
    }

    pub fn with_filter(mut self, filter: String) -> Self {
        self.filter = Some(filter);
        self
    }

    pub fn with_encryption(mut self, encrypt: bool) -> Self {
        self.encrypt = encrypt;
        self
    }

    pub fn with_version(mut self, version: u32) -> Self {
        self.version = version;
        self
    }

    pub fn build(self) -> PersistencePolicy {
        PersistencePolicy {
            path: self.path,
            scope: self.scope,
            filter: self.filter,
            encrypt: self.encrypt,
            version: self.version,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_persistence_scope_hierarchy() {
        let sp = SelectivePersistence::new();
        
        assert!(!sp.scope_includes(PersistenceScope::Never, PersistenceScope::HotReload));
        assert!(sp.scope_includes(PersistenceScope::HotReload, PersistenceScope::HotReload));
        assert!(sp.scope_includes(PersistenceScope::TimeTravel, PersistenceScope::HotReload));
        assert!(sp.scope_includes(PersistenceScope::Session, PersistenceScope::TimeTravel));
        assert!(sp.scope_includes(PersistenceScope::Permanent, PersistenceScope::Session));
    }

    #[test]
    fn test_pattern_matching() {
        let sp = SelectivePersistence::new();
        
        assert!(sp.matches_pattern("scene.*", "scene.camera"));
        assert!(sp.matches_pattern("scene.*", "scene.camera.position"));
        assert!(!sp.matches_pattern("scene.*", "ui.panel"));
        assert!(sp.matches_pattern("scene.camera", "scene.camera"));
    }

    #[test]
    fn test_policy_registration() {
        let mut sp = SelectivePersistence::new();
        
        let policy = PolicyBuilder::new(vec!["scene".to_string(), "camera".to_string()])
            .with_scope(PersistenceScope::Session)
            .build();
        
        sp.register_policy(policy);
        
        assert!(sp.should_persist(&["scene".to_string(), "camera".to_string()], PersistenceScope::HotReload));
        assert!(sp.should_persist(&["scene".to_string(), "camera".to_string()], PersistenceScope::Session));
        assert!(!sp.should_persist(&["scene".to_string(), "camera".to_string()], PersistenceScope::Permanent));
    }

    #[test]
    fn test_state_filtering() {
        let mut sp = SelectivePersistence::new();
        
        // Register policies
        sp.register_policy(
            PolicyBuilder::new(vec!["ui".to_string()])
                .with_scope(PersistenceScope::Never)
                .build()
        );
        
        sp.register_policy(
            PolicyBuilder::new(vec!["scene".to_string()])
                .with_scope(PersistenceScope::Session)
                .build()
        );
        
        // Create test state
        let mut state = State::new();
        state.set(ValuePath::new(vec!["ui".to_string()]), Value::Str("panel".to_string()));
        state.set(ValuePath::new(vec!["scene".to_string()]), Value::Str("3d".to_string()));
        state.set(ValuePath::new(vec!["other".to_string()]), Value::Int(42));
        
        // Filter for hot-reload scope
        let filtered = sp.filter_state(&state, PersistenceScope::HotReload);
        
        assert!(filtered.get(&ValuePath::new(vec!["ui".to_string()])).is_none()); // Never persist
        assert!(filtered.get(&ValuePath::new(vec!["scene".to_string()])).is_some()); // Session includes HotReload
        assert!(filtered.get(&ValuePath::new(vec!["other".to_string()])).is_some()); // Default is HotReload
    }

    #[test]
    fn test_dirty_paths() {
        let mut sp = SelectivePersistence::new();
        
        sp.mark_dirty(vec!["scene".to_string(), "camera".to_string()]);
        sp.mark_dirty(vec!["ui".to_string(), "panel".to_string()]);
        
        let dirty = sp.take_dirty_paths();
        assert_eq!(dirty.len(), 2);
        assert!(dirty.contains(&vec!["scene".to_string(), "camera".to_string()]));
        
        let empty = sp.take_dirty_paths();
        assert!(empty.is_empty());
    }

    #[test]
    fn test_migration() {
        let mut sp = SelectivePersistence::new();
        
        // Register a migration from v1 to v2
        sp.register_migration(1, 2, Box::new(|v| {
            match v {
                Value::Int(i) => Value::Int(i * 2),
                _ => v.clone(),
            }
        }));
        
        let original = Value::Int(21);
        let migrated = sp.migrate(&original, 1, 2);
        
        assert_eq!(migrated, Value::Int(42));
    }

    #[test]
    fn test_policy_from_meta() {
        let sp = SelectivePersistence::new();
        
        let mut meta_map = HashMap::new();
        meta_map.insert(
            "path".to_string(),
            Value::List(vec![
                Value::Symbol(Symbol("scene".to_string())),
                Value::Symbol(Symbol("camera".to_string())),
            ])
        );
        meta_map.insert(
            "persist".to_string(),
            Value::Keyword(Keyword("session".to_string()))
        );
        meta_map.insert(
            "encrypt".to_string(),
            Value::Bool(true)
        );
        meta_map.insert(
            "version".to_string(),
            Value::Int(2)
        );
        
        let meta = Value::Map(meta_map);
        let policy = sp.policy_from_meta(&meta).unwrap();
        
        assert_eq!(policy.path, vec!["scene".to_string(), "camera".to_string()]);
        assert_eq!(policy.scope, PersistenceScope::Session);
        assert!(policy.encrypt);
        assert_eq!(policy.version, 2);
    }

    #[test]
    fn test_merge_states() {
        let mut sp = SelectivePersistence::new();
        
        sp.register_policy(
            PolicyBuilder::new(vec!["persistent".to_string()])
                .with_scope(PersistenceScope::Session)
                .build()
        );
        
        let mut persisted = State::new();
        persisted.set(ValuePath::new(vec!["persistent".to_string()]), Value::Int(100));
        persisted.set(ValuePath::new(vec!["old".to_string()]), Value::Str("data".to_string()));
        
        let mut current = State::new();
        current.set(ValuePath::new(vec!["persistent".to_string()]), Value::Int(200));
        current.set(ValuePath::new(vec!["new".to_string()]), Value::Str("value".to_string()));
        
        let merged = sp.merge_states(&persisted, &current, PersistenceScope::HotReload);
        
        // Persistent value should come from persisted state
        assert_eq!(merged.get(&ValuePath::new(vec!["persistent".to_string()])), Some(&Value::Int(100)));
        // New value should remain from current state
        assert_eq!(merged.get(&ValuePath::new(vec!["new".to_string()])), Some(&Value::Str("value".to_string())));
        // Old value should be preserved based on default policy
        assert_eq!(merged.get(&ValuePath::new(vec!["old".to_string()])), Some(&Value::Str("data".to_string())));
    }
}