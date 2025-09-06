//! Meta-level Architecture for XR-Lang
//! 
//! Inspired by Smalltalk and CLOS, this module provides a reflective meta-interface
//! that allows introspection and modification of both static entities (classes, methods)
//! and dynamic entities (stack frames, processes) at runtime.
//!
//! References:
//! - Smalltalk-80 Blue Book, Chapter 16
//! - The Art of the Metaobject Protocol (AMOP)
//! - Design Principles Behind Smalltalk

use crate::value::{Value, Symbol};
use crate::live_image::{LiveImage, ExecutionContext, StackFrame};
use std::rc::Rc;
use std::collections::HashMap;
use std::cell::RefCell;

/// Meta-level interface for introspection and modification
pub struct MetaInterface {
    /// Access to the live image
    image: Rc<RefCell<LiveImage>>,
    
    /// Method dictionary for dynamic dispatch
    methods: HashMap<Symbol, MethodInfo>,
    
    /// Class hierarchy (simplified - full CLOS-style would be more complex)
    classes: HashMap<Symbol, ClassInfo>,
    
    /// Active processes/threads
    processes: Vec<Process>,
    
    /// Debugger interface
    debugger: Option<Debugger>,
}

/// Method information for meta-level access
#[derive(Clone, Debug)]
pub struct MethodInfo {
    pub name: Symbol,
    pub class: Option<Symbol>,
    pub source: String,
    pub bytecode: Vec<u8>,
    pub locals: Vec<Symbol>,
    pub captures: Vec<Symbol>,
    pub metadata: HashMap<String, Value>,
}

/// Class information (simplified metaclass)
#[derive(Clone, Debug)]
pub struct ClassInfo {
    pub name: Symbol,
    pub superclass: Option<Symbol>,
    pub instance_variables: Vec<Symbol>,
    pub class_variables: Vec<Symbol>,
    pub methods: Vec<Symbol>,
    pub metaclass: Option<Symbol>,
}

/// Process represents a running computation
#[derive(Clone, Debug)]
pub struct Process {
    pub id: u64,
    pub name: String,
    pub state: ProcessState,
    pub stack: Vec<StackFrame>,
    pub context: ExecutionContext,
    pub priority: u8,
}

#[derive(Clone, Debug, PartialEq)]
pub enum ProcessState {
    Running,
    Suspended,
    Waiting,
    Terminated,
}

/// Debugger for interactive development
pub struct Debugger {
    /// Current process being debugged
    current_process: Option<u64>,
    
    /// Breakpoints
    breakpoints: Vec<Breakpoint>,
    
    /// Watch expressions
    watches: Vec<WatchExpression>,
    
    /// Inspector for live objects
    inspector: ObjectInspector,
}

#[derive(Clone, Debug)]
pub struct Breakpoint {
    pub method: Symbol,
    pub line: Option<usize>,
    pub condition: Option<Value>,
    pub hit_count: usize,
}

#[derive(Clone, Debug)]
pub struct WatchExpression {
    pub expr: Value,
    pub value: Option<Value>,
    pub changed: bool,
}

/// Object inspector for runtime introspection
pub struct ObjectInspector {
    /// Currently inspected objects
    inspected: HashMap<u64, InspectedObject>,
    
    /// Object graph for visualization
    object_graph: ObjectGraph,
}

#[derive(Clone, Debug)]
pub struct InspectedObject {
    pub id: u64,
    pub value: Value,
    pub class: Option<Symbol>,
    pub instance_vars: HashMap<Symbol, Value>,
    pub methods: Vec<Symbol>,
}

#[derive(Clone, Debug)]
pub struct ObjectGraph {
    pub nodes: Vec<ObjectNode>,
    pub edges: Vec<ObjectEdge>,
}

#[derive(Clone, Debug)]
pub struct ObjectNode {
    pub id: u64,
    pub label: String,
    pub value_type: String,
}

#[derive(Clone, Debug)]
pub struct ObjectEdge {
    pub from: u64,
    pub to: u64,
    pub label: String,
}

impl MetaInterface {
    pub fn new(image: Rc<RefCell<LiveImage>>) -> Self {
        MetaInterface {
            image,
            methods: HashMap::new(),
            classes: HashMap::new(),
            processes: Vec::new(),
            debugger: None,
        }
    }
    
    /// --- Static Entity Introspection ---
    
    /// Get all defined methods
    pub fn all_methods(&self) -> Vec<&MethodInfo> {
        self.methods.values().collect()
    }
    
    /// Get method by name
    pub fn get_method(&self, name: &Symbol) -> Option<&MethodInfo> {
        self.methods.get(name)
    }
    
    /// Get all classes
    pub fn all_classes(&self) -> Vec<&ClassInfo> {
        self.classes.values().collect()
    }
    
    /// Get class by name
    pub fn get_class(&self, name: &Symbol) -> Option<&ClassInfo> {
        self.classes.get(name)
    }
    
    /// --- Static Entity Modification ---
    
    /// Add or update a method at runtime
    pub fn define_method(&mut self, name: Symbol, source: String, body: Value) -> Result<(), String> {
        // Parse method definition
        let method_info = MethodInfo {
            name: name.clone(),
            class: None, // Would be determined from context
            source: source.clone(),
            bytecode: Vec::new(), // Would compile to bytecode
            locals: Vec::new(),
            captures: Vec::new(),
            metadata: HashMap::new(),
        };
        
        // Store method
        self.methods.insert(name.clone(), method_info);
        
        // Update live image
        self.image.borrow_mut().apply_live_update(
            crate::live_image::LiveUpdate::FunctionUpdate {
                name,
                old: Value::Nil,
                new: body,
            }
        )?;
        
        Ok(())
    }
    
    /// Modify a class at runtime
    pub fn modify_class(&mut self, name: Symbol, modification: ClassModification) -> Result<(), String> {
        match modification {
            ClassModification::AddMethod(method_name) => {
                if let Some(class) = self.classes.get_mut(&name) {
                    class.methods.push(method_name);
                }
            }
            ClassModification::AddInstanceVariable(var_name) => {
                if let Some(class) = self.classes.get_mut(&name) {
                    class.instance_variables.push(var_name);
                    // Migrate existing instances
                    self.migrate_instances(&name)?;
                }
            }
            ClassModification::ChangeSuperclass(new_super) => {
                if let Some(class) = self.classes.get_mut(&name) {
                    class.superclass = Some(new_super);
                    // Recompute method dispatch
                    self.recompute_dispatch(&name)?;
                }
            }
        }
        Ok(())
    }
    
    /// --- Dynamic Entity Introspection ---
    
    /// Get all running processes
    pub fn all_processes(&self) -> &[Process] {
        &self.processes
    }
    
    /// Get current execution context
    pub fn current_context(&self) -> Option<ExecutionContext> {
        self.processes.iter()
            .find(|p| p.state == ProcessState::Running)
            .map(|p| p.context.clone())
    }
    
    /// Inspect a live object
    pub fn inspect_object(&mut self, value: &Value) -> InspectedObject {
        let id = self.generate_object_id();
        let inspected = InspectedObject {
            id,
            value: value.clone(),
            class: self.object_class(value),
            instance_vars: self.extract_instance_vars(value),
            methods: self.object_methods(value),
        };
        
        if let Some(ref mut debugger) = self.debugger {
            debugger.inspector.inspected.insert(id, inspected.clone());
        }
        
        inspected
    }
    
    /// Get stack trace of current process
    pub fn stack_trace(&self) -> Vec<StackFrame> {
        self.processes.iter()
            .find(|p| p.state == ProcessState::Running)
            .map(|p| p.stack.clone())
            .unwrap_or_default()
    }
    
    /// --- Dynamic Entity Modification ---
    
    /// Modify a stack frame (for debugging)
    pub fn modify_stack_frame(&mut self, frame_index: usize, var: Symbol, value: Value) -> Result<(), String> {
        if let Some(process) = self.processes.iter_mut().find(|p| p.state == ProcessState::Running) {
            if let Some(frame) = process.stack.get_mut(frame_index) {
                frame.locals.insert(var, value);
                Ok(())
            } else {
                Err("Invalid frame index".to_string())
            }
        } else {
            Err("No running process".to_string())
        }
    }
    
    /// Restart a stack frame (continue from a different point)
    pub fn restart_frame(&mut self, frame_index: usize) -> Result<(), String> {
        if let Some(process) = self.processes.iter_mut().find(|p| p.state == ProcessState::Running) {
            // Truncate stack to restart point
            process.stack.truncate(frame_index + 1);
            Ok(())
        } else {
            Err("No running process".to_string())
        }
    }
    
    /// --- Debugger Interface ---
    
    /// Start debugging a process
    pub fn start_debugging(&mut self, process_id: u64) {
        if self.debugger.is_none() {
            self.debugger = Some(Debugger {
                current_process: Some(process_id),
                breakpoints: Vec::new(),
                watches: Vec::new(),
                inspector: ObjectInspector {
                    inspected: HashMap::new(),
                    object_graph: ObjectGraph {
                        nodes: Vec::new(),
                        edges: Vec::new(),
                    },
                },
            });
        }
        
        // Suspend the process
        if let Some(process) = self.processes.iter_mut().find(|p| p.id == process_id) {
            process.state = ProcessState::Suspended;
        }
    }
    
    /// Set a breakpoint
    pub fn set_breakpoint(&mut self, method: Symbol, line: Option<usize>) {
        if let Some(ref mut debugger) = self.debugger {
            debugger.breakpoints.push(Breakpoint {
                method,
                line,
                condition: None,
                hit_count: 0,
            });
        }
    }
    
    /// Step through execution
    pub fn step(&mut self) -> Result<(), String> {
        if let Some(ref debugger) = self.debugger {
            if let Some(process_id) = debugger.current_process {
                // Execute one step
                self.execute_single_step(process_id)?;
            }
        }
        Ok(())
    }
    
    /// Continue execution
    pub fn continue_execution(&mut self) {
        if let Some(ref debugger) = self.debugger {
            if let Some(process_id) = debugger.current_process {
                if let Some(process) = self.processes.iter_mut().find(|p| p.id == process_id) {
                    process.state = ProcessState::Running;
                }
            }
        }
    }
    
    /// --- Helper Methods ---
    
    fn generate_object_id(&self) -> u64 {
        // Simple incrementing ID - would be more sophisticated in practice
        self.processes.len() as u64 + 1000
    }
    
    fn object_class(&self, _value: &Value) -> Option<Symbol> {
        // Determine class of value
        // In full implementation, would look up metaclass
        None
    }
    
    fn extract_instance_vars(&self, value: &Value) -> HashMap<Symbol, Value> {
        match value {
            Value::Map(map) => {
                map.iter()
                    .map(|(k, v)| (Symbol(k.clone()), v.clone()))
                    .collect()
            }
            _ => HashMap::new()
        }
    }
    
    fn object_methods(&self, _value: &Value) -> Vec<Symbol> {
        // Get methods applicable to this object
        // Would do method lookup through class hierarchy
        Vec::new()
    }
    
    fn migrate_instances(&mut self, _class: &Symbol) -> Result<(), String> {
        // Migrate all instances of a class after structure change
        // Similar to Smalltalk's become: operation
        Ok(())
    }
    
    fn recompute_dispatch(&mut self, _class: &Symbol) -> Result<(), String> {
        // Recompute method dispatch after hierarchy change
        Ok(())
    }
    
    fn execute_single_step(&mut self, _process_id: u64) -> Result<(), String> {
        // Execute one instruction/expression
        Ok(())
    }
}

/// Modifications that can be applied to classes
#[derive(Clone, Debug)]
pub enum ClassModification {
    AddMethod(Symbol),
    AddInstanceVariable(Symbol),
    ChangeSuperclass(Symbol),
}

/// Meta-level operations exposed as native functions
pub fn register_meta_functions() -> HashMap<String, Rc<dyn Fn(&[Value]) -> Result<Value, String>>> {
    let mut functions = HashMap::new();
    
    // Introspection functions
    functions.insert("methods-of".to_string(), Rc::new(|args: &[Value]| {
        if args.len() != 1 {
            return Err("methods-of expects 1 argument".to_string());
        }
        // Return list of methods for object
        Ok(Value::List(Vec::new()))
    }) as Rc<dyn Fn(&[Value]) -> Result<Value, String>>);
    
    functions.insert("class-of".to_string(), Rc::new(|args: &[Value]| {
        if args.len() != 1 {
            return Err("class-of expects 1 argument".to_string());
        }
        // Return class of object
        Ok(Value::Symbol(Symbol("Object".to_string())))
    }) as Rc<dyn Fn(&[Value]) -> Result<Value, String>>);
    
    functions.insert("inspect".to_string(), Rc::new(|args: &[Value]| {
        if args.len() != 1 {
            return Err("inspect expects 1 argument".to_string());
        }
        // Open inspector on object
        println!("Inspecting: {:?}", args[0]);
        Ok(args[0].clone())
    }) as Rc<dyn Fn(&[Value]) -> Result<Value, String>>);
    
    functions.insert("stack-trace".to_string(), Rc::new(|_args: &[Value]| {
        // Return current stack trace
        Ok(Value::List(Vec::new()))
    }) as Rc<dyn Fn(&[Value]) -> Result<Value, String>>);
    
    functions.insert("become".to_string(), Rc::new(|args: &[Value]| {
        if args.len() != 2 {
            return Err("become expects 2 arguments".to_string());
        }
        // Smalltalk-style become: - swap identity of two objects
        println!("Swapping identity: {:?} <-> {:?}", args[0], args[1]);
        Ok(Value::Nil)
    }) as Rc<dyn Fn(&[Value]) -> Result<Value, String>>);
    
    functions
}