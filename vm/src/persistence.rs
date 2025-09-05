//! Persistence Layer for XR-Lang
//! 
//! Event-sourced architecture with journal and snapshots to enable:
//! - Time-travel debugging
//! - State persistence across sessions
//! - Branching and merging of timelines
//! - Prevention of "image drift" issues from Smalltalk

use crate::value::Value;
use std::collections::HashMap;
use serde::{Deserialize, Serialize};
use std::time::{SystemTime, UNIX_EPOCH};

/// Path to a value in the state tree
#[derive(Debug, Clone, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub struct ValuePath(pub Vec<String>);

impl ValuePath {
    pub fn new(segments: Vec<String>) -> Self {
        ValuePath(segments)
    }
    
    pub fn root() -> Self {
        ValuePath(vec![])
    }
    
    pub fn append(&self, segment: String) -> Self {
        let mut segments = self.0.clone();
        segments.push(segment);
        ValuePath(segments)
    }
}

/// Author of a change
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum Author {
    Human(String),
    AI(String),
    System,
    Generated,
}

/// Provenance tracking for code generation
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct Provenance {
    pub author: Author,
    pub timestamp: u64,
    pub description: Option<String>,
    pub confidence: Option<f32>,  // For AI-generated code
}

/// Types of changes that can occur
#[derive(Debug, Clone, PartialEq)]
pub enum Change {
    Create { path: ValuePath, value: Value },
    Update { path: ValuePath, old: Value, new: Value },
    Delete { path: ValuePath, value: Value },
    Move { from: ValuePath, to: ValuePath, value: Value },
}

/// A single entry in the journal
#[derive(Debug, Clone)]
pub struct JournalEntry {
    pub timestamp: u64,
    pub author: Author,
    pub change: Change,
    pub provenance: Provenance,
    pub metadata: HashMap<String, Value>,
}

/// A snapshot of the complete state at a point in time
#[derive(Debug, Clone)]
pub struct Snapshot {
    pub id: SnapshotId,
    pub timestamp: u64,
    pub state: HashMap<ValuePath, Value>,
    pub metadata: HashMap<String, Value>,
}

/// Unique identifier for snapshots
#[derive(Debug, Clone, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub struct SnapshotId(pub String);

/// Branch identifier for parallel timelines
#[derive(Debug, Clone, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub struct BranchId(pub String);

/// The journal maintains the event log
pub struct Journal {
    entries: Vec<JournalEntry>,
    current_branch: BranchId,
    branches: HashMap<BranchId, Vec<JournalEntry>>,
}

impl Journal {
    /// Create a new empty journal
    pub fn new() -> Self {
        Journal {
            entries: Vec::new(),
            current_branch: BranchId("main".to_string()),
            branches: HashMap::new(),
        }
    }
    
    /// Record a new change
    pub fn record(&mut self, change: Change, author: Author) -> Result<(), String> {
        let timestamp = SystemTime::now()
            .duration_since(UNIX_EPOCH)
            .map_err(|e| format!("Time error: {}", e))?
            .as_millis() as u64;
        
        let entry = JournalEntry {
            timestamp,
            author: author.clone(),
            change,
            provenance: Provenance {
                author,
                timestamp,
                description: None,
                confidence: None,
            },
            metadata: HashMap::new(),
        };
        
        self.entries.push(entry);
        Ok(())
    }
    
    /// Replay journal to a specific timestamp
    pub fn replay_to(&self, timestamp: u64) -> State {
        let mut state = State::new();
        
        for entry in &self.entries {
            if entry.timestamp > timestamp {
                break;
            }
            state.apply_change(&entry.change);
        }
        
        state
    }
    
    /// Create a new branch at the specified timestamp
    pub fn branch_at(&mut self, timestamp: u64, branch_name: String) -> Result<BranchId, String> {
        let branch_id = BranchId(branch_name);
        
        // Copy entries up to the branch point
        let branch_entries: Vec<JournalEntry> = self.entries
            .iter()
            .filter(|e| e.timestamp <= timestamp)
            .cloned()
            .collect();
        
        self.branches.insert(branch_id.clone(), branch_entries);
        Ok(branch_id)
    }
    
    /// Switch to a different branch
    pub fn switch_branch(&mut self, branch_id: BranchId) -> Result<(), String> {
        if !self.branches.contains_key(&branch_id) && branch_id != BranchId("main".to_string()) {
            return Err(format!("Branch {:?} does not exist", branch_id));
        }
        
        // Save current branch
        self.branches.insert(self.current_branch.clone(), self.entries.clone());
        
        // Load new branch
        if let Some(entries) = self.branches.get(&branch_id) {
            self.entries = entries.clone();
        } else if branch_id == BranchId("main".to_string()) {
            // Main branch might not be in branches map
            // In that case, keep current entries
        }
        
        self.current_branch = branch_id;
        Ok(())
    }
    
    /// Get all entries between two timestamps
    pub fn get_range(&self, from: u64, to: u64) -> Vec<&JournalEntry> {
        self.entries
            .iter()
            .filter(|e| e.timestamp >= from && e.timestamp <= to)
            .collect()
    }
    
    /// Find entries by author
    pub fn find_by_author(&self, author: &Author) -> Vec<&JournalEntry> {
        self.entries
            .iter()
            .filter(|e| &e.author == author)
            .collect()
    }
}

/// The current state of the system
pub struct State {
    values: HashMap<ValuePath, Value>,
    metadata: HashMap<ValuePath, HashMap<String, Value>>,
}

impl State {
    /// Create a new empty state
    pub fn new() -> Self {
        State {
            values: HashMap::new(),
            metadata: HashMap::new(),
        }
    }
    
    /// Apply a change to the state
    pub fn apply_change(&mut self, change: &Change) {
        match change {
            Change::Create { path, value } => {
                self.values.insert(path.clone(), value.clone());
            }
            Change::Update { path, new, .. } => {
                self.values.insert(path.clone(), new.clone());
            }
            Change::Delete { path, .. } => {
                self.values.remove(path);
                self.metadata.remove(path);
            }
            Change::Move { from, to, value } => {
                self.values.remove(from);
                self.values.insert(to.clone(), value.clone());
                if let Some(meta) = self.metadata.remove(from) {
                    self.metadata.insert(to.clone(), meta);
                }
            }
        }
    }
    
    /// Get a value at a path
    pub fn get(&self, path: &ValuePath) -> Option<&Value> {
        self.values.get(path)
    }
    
    /// Set a value at a path
    pub fn set(&mut self, path: ValuePath, value: Value) -> Change {
        if let Some(old) = self.values.get(&path) {
            let old_value = old.clone();
            self.values.insert(path.clone(), value.clone());
            Change::Update { path, old: old_value, new: value }
        } else {
            self.values.insert(path.clone(), value.clone());
            Change::Create { path, value }
        }
    }
    
    /// Create a snapshot of the current state
    pub fn snapshot(&self) -> Snapshot {
        let timestamp = SystemTime::now()
            .duration_since(UNIX_EPOCH)
            .unwrap()
            .as_millis() as u64;
        
        Snapshot {
            id: SnapshotId(format!("snapshot_{}", timestamp)),
            timestamp,
            state: self.values.clone(),
            metadata: HashMap::new(),
        }
    }
    
    /// Restore from a snapshot
    pub fn restore_from_snapshot(&mut self, snapshot: &Snapshot) {
        self.values = snapshot.state.clone();
        self.metadata.clear();
    }
}

/// Manages snapshots for efficient time-travel
pub struct SnapshotStore {
    snapshots: Vec<Snapshot>,
    max_snapshots: usize,
}

impl SnapshotStore {
    /// Create a new snapshot store
    pub fn new(max_snapshots: usize) -> Self {
        SnapshotStore {
            snapshots: Vec::new(),
            max_snapshots,
        }
    }
    
    /// Add a snapshot to the store
    pub fn add(&mut self, snapshot: Snapshot) {
        self.snapshots.push(snapshot);
        
        // Remove old snapshots if we exceed the limit
        if self.snapshots.len() > self.max_snapshots {
            // Keep every Nth snapshot for sparse coverage of history
            let keep_interval = 10;
            let mut kept = vec![];
            for (i, snap) in self.snapshots.iter().enumerate() {
                if i % keep_interval == 0 || i >= self.snapshots.len() - self.max_snapshots / 2 {
                    kept.push(snap.clone());
                }
            }
            self.snapshots = kept;
        }
    }
    
    /// Find the nearest snapshot before a timestamp
    pub fn find_before(&self, timestamp: u64) -> Option<&Snapshot> {
        self.snapshots
            .iter()
            .rev()
            .find(|s| s.timestamp <= timestamp)
    }
    
    /// Get snapshot by ID
    pub fn get(&self, id: &SnapshotId) -> Option<&Snapshot> {
        self.snapshots.iter().find(|s| &s.id == id)
    }
}

/// Handles conflict resolution when merging branches
pub struct ConflictMerger {
    strategy: MergeStrategy,
}

#[derive(Debug, Clone)]
pub enum MergeStrategy {
    /// Always take changes from the source branch
    TakeSource,
    /// Always take changes from the target branch
    TakeTarget,
    /// Prompt for manual resolution
    Manual,
    /// Use AI to suggest resolution
    AI,
}

impl ConflictMerger {
    pub fn new(strategy: MergeStrategy) -> Self {
        ConflictMerger { strategy }
    }
    
    /// Merge two branches
    pub fn merge(
        &self,
        source: &[JournalEntry],
        target: &[JournalEntry],
    ) -> Result<Vec<JournalEntry>, String> {
        // Find common ancestor
        let common_timestamp = self.find_common_ancestor(source, target);
        
        // Separate changes after divergence
        let source_changes: Vec<_> = source.iter()
            .filter(|e| e.timestamp > common_timestamp)
            .collect();
        let target_changes: Vec<_> = target.iter()
            .filter(|e| e.timestamp > common_timestamp)
            .collect();
        
        // Detect conflicts
        let conflicts = self.detect_conflicts(&source_changes, &target_changes);
        
        if conflicts.is_empty() {
            // No conflicts, merge is straightforward
            let mut result = target.to_vec();
            for entry in source_changes {
                result.push(entry.clone());
            }
            Ok(result)
        } else {
            // Resolve conflicts based on strategy
            self.resolve_conflicts(conflicts, source, target)
        }
    }
    
    fn find_common_ancestor(&self, source: &[JournalEntry], target: &[JournalEntry]) -> u64 {
        // Find the last common timestamp
        let mut common = 0;
        for s_entry in source {
            if target.iter().any(|t| t.timestamp == s_entry.timestamp) {
                common = common.max(s_entry.timestamp);
            }
        }
        common
    }
    
    fn detect_conflicts<'a>(
        &self,
        source: &'a [&'a JournalEntry],
        target: &'a [&'a JournalEntry],
    ) -> Vec<(ValuePath, Vec<&'a JournalEntry>, Vec<&'a JournalEntry>)> {
        let mut conflicts = Vec::new();
        let mut paths_checked = std::collections::HashSet::new();
        
        for s_entry in source {
            let path = match &s_entry.change {
                Change::Create { path, .. } |
                Change::Update { path, .. } |
                Change::Delete { path, .. } => path.clone(),
                Change::Move { from, .. } => from.clone(),
            };
            
            if paths_checked.contains(&path) {
                continue;
            }
            
            let target_changes: Vec<_> = target.iter()
                .filter(|t| {
                    match &t.change {
                        Change::Create { path: p, .. } |
                        Change::Update { path: p, .. } |
                        Change::Delete { path: p, .. } => p == &path,
                        Change::Move { from, .. } => from == &path,
                    }
                })
                .copied()
                .collect();
            
            if !target_changes.is_empty() {
                let source_changes = vec![*s_entry];
                conflicts.push((path.clone(), source_changes, target_changes));
                paths_checked.insert(path);
            }
        }
        
        conflicts
    }
    
    fn resolve_conflicts<'a>(
        &self,
        _conflicts: Vec<(ValuePath, Vec<&'a JournalEntry>, Vec<&'a JournalEntry>)>,
        source: &'a [JournalEntry],
        target: &'a [JournalEntry],
    ) -> Result<Vec<JournalEntry>, String> {
        match self.strategy {
            MergeStrategy::TakeSource => Ok(source.to_vec()),
            MergeStrategy::TakeTarget => Ok(target.to_vec()),
            MergeStrategy::Manual => {
                Err("Manual conflict resolution required".to_string())
            }
            MergeStrategy::AI => {
                Err("AI conflict resolution not yet implemented".to_string())
            }
        }
    }
}

/// Manages the complete persistence layer
pub struct PersistenceLayer {
    pub journal: Journal,
    pub snapshots: SnapshotStore,
    pub merger: ConflictMerger,
    current_state: State,
}

impl PersistenceLayer {
    /// Create a new persistence layer
    pub fn new() -> Self {
        PersistenceLayer {
            journal: Journal::new(),
            snapshots: SnapshotStore::new(100),
            merger: ConflictMerger::new(MergeStrategy::Manual),
            current_state: State::new(),
        }
    }
    
    /// Record a change and update state
    pub fn record_change(&mut self, change: Change, author: Author) -> Result<(), String> {
        self.journal.record(change.clone(), author)?;
        self.current_state.apply_change(&change);
        Ok(())
    }
    
    /// Travel to a specific point in time
    pub fn travel_to(&mut self, timestamp: u64) -> Result<(), String> {
        // Find nearest snapshot before timestamp
        if let Some(snapshot) = self.snapshots.find_before(timestamp) {
            self.current_state.restore_from_snapshot(snapshot);
            
            // Replay journal from snapshot to target time
            let entries = self.journal.get_range(snapshot.timestamp, timestamp);
            for entry in entries {
                self.current_state.apply_change(&entry.change);
            }
        } else {
            // No snapshot, replay from beginning
            self.current_state = self.journal.replay_to(timestamp);
        }
        Ok(())
    }
    
    /// Create a checkpoint (snapshot)
    pub fn checkpoint(&mut self) -> SnapshotId {
        let snapshot = self.current_state.snapshot();
        let id = snapshot.id.clone();
        self.snapshots.add(snapshot);
        id
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    
    #[test]
    fn test_journal_recording() {
        let mut journal = Journal::new();
        let change = Change::Create {
            path: ValuePath::new(vec!["test".to_string()]),
            value: Value::Int(42),
        };
        
        journal.record(change, Author::Human("test".to_string())).unwrap();
        assert_eq!(journal.entries.len(), 1);
    }
    
    #[test]
    fn test_state_changes() {
        let mut state = State::new();
        let path = ValuePath::new(vec!["x".to_string()]);
        
        state.set(path.clone(), Value::Int(1));
        assert_eq!(state.get(&path), Some(&Value::Int(1)));
        
        state.set(path.clone(), Value::Int(2));
        assert_eq!(state.get(&path), Some(&Value::Int(2)));
    }
    
    #[test]
    fn test_time_travel() {
        let mut persistence = PersistenceLayer::new();
        let path = ValuePath::new(vec!["counter".to_string()]);
        
        // Make first change
        let change1 = Change::Create {
            path: path.clone(),
            value: Value::Int(0),
        };
        persistence.record_change(change1, Author::System).unwrap();
        
        // Sleep a tiny bit to ensure timestamps are different
        std::thread::sleep(std::time::Duration::from_millis(10));
        
        // Get timestamp BETWEEN changes
        let timestamp = SystemTime::now()
            .duration_since(UNIX_EPOCH)
            .unwrap()
            .as_millis() as u64;
        
        // Sleep again to ensure second change has different timestamp
        std::thread::sleep(std::time::Duration::from_millis(10));
        
        // Make second change
        let change2 = Change::Update {
            path: path.clone(),
            old: Value::Int(0),
            new: Value::Int(1),
        };
        persistence.record_change(change2, Author::System).unwrap();
        
        // Current state should be 1
        assert_eq!(persistence.current_state.get(&path), Some(&Value::Int(1)));
        
        // Travel back to timestamp (between changes)
        persistence.travel_to(timestamp).unwrap();
        
        // Should be back to 0
        assert_eq!(persistence.current_state.get(&path), Some(&Value::Int(0)));
    }
}