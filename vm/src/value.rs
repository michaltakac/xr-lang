//! XR-Lang Value Model - The foundation for homoiconicity
//! 
//! This module defines the core value types that XR-Lang can manipulate.
//! Following the Lisp tradition, everything is a value - including code itself.
//! This enables the language to treat code as data and data as code.

use std::collections::HashMap;
use std::fmt;
use std::rc::Rc;
use serde::{Deserialize, Serialize};

/// Symbol type for identifiers
#[derive(Debug, Clone, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub struct Symbol(pub String);

/// Keyword type (like Clojure's keywords)
#[derive(Debug, Clone, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub struct Keyword(pub String);

/// Object ID for scene objects
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub struct ObjectId(pub u64);

/// AST node for homoiconicity
#[derive(Debug, Clone, PartialEq)]
pub struct ASTNode {
    pub node_type: String,
    pub children: Vec<Value>,
    pub metadata: HashMap<String, Value>,
}

/// Closure for functions
#[derive(Debug, Clone)]
pub struct Closure {
    pub params: Vec<Symbol>,
    pub body: Box<Value>,
    pub env: Environment,
}

/// Macro definition
#[derive(Debug, Clone)]
pub struct Macro {
    pub name: Symbol,
    pub params: Vec<Symbol>,
    pub body: Box<Value>,
}

/// Capability for security
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct Capability {
    pub name: String,
    pub requires: Vec<String>,
    pub quota: Option<i64>,
    pub audit: bool,
}

/// Channel for concurrency
#[derive(Debug, Clone)]
pub struct Channel {
    pub id: u64,
    // In a real implementation, this would contain actual channel mechanisms
}

/// Environment for variable bindings
#[derive(Debug, Clone)]
pub struct Environment {
    bindings: Rc<HashMap<Symbol, Value>>,
    parent: Option<Rc<Environment>>,
}

impl Environment {
    pub fn new() -> Self {
        Environment {
            bindings: Rc::new(HashMap::new()),
            parent: None,
        }
    }
    
    pub fn with_parent(parent: Rc<Environment>) -> Self {
        Environment {
            bindings: Rc::new(HashMap::new()),
            parent: Some(parent),
        }
    }
    
    pub fn bind(&mut self, symbol: Symbol, value: Value) {
        Rc::get_mut(&mut self.bindings)
            .expect("Cannot modify shared environment")
            .insert(symbol, value);
    }
    
    pub fn lookup(&self, symbol: &Symbol) -> Option<Value> {
        self.bindings.get(symbol).cloned().or_else(|| {
            self.parent.as_ref().and_then(|p| p.lookup(symbol))
        })
    }
}

/// Core value type - represents all possible values in XR-Lang
#[derive(Clone)]
pub enum Value {
    // Primitives
    Nil,
    Bool(bool),
    Int(i64),
    Float(f64),
    Str(String),
    Symbol(Symbol),
    Keyword(Keyword),
    
    // Collections
    List(Vec<Value>),
    Vector(Vec<Value>),
    Map(HashMap<String, Value>),  // Use String keys to avoid recursion
    Set(Vec<Value>),  // Use Vec instead of HashSet to avoid Hash requirement
    
    // Special types for homoiconicity
    AST(Box<ASTNode>),           // Code as data
    Quote(Box<Value>),           // Quoted expression
    
    // Scene objects
    Object(ObjectId),
    
    // Functions and macros
    Closure(Rc<Closure>),
    Macro(Rc<Macro>),
    NativeFunction(NativeFn),
    
    // Extended types
    Capability(Capability),
    Channel(Rc<Channel>),
    
    // Metadata-carrying value
    WithMeta {
        value: Box<Value>,
        metadata: HashMap<String, Value>,
    },
}

/// Native function type
pub type NativeFn = Rc<dyn Fn(&[Value]) -> Result<Value, String>>;

// Implement Hash for Value to use in HashMap keys
// Note: Only simple values can be hashed properly
impl std::hash::Hash for Value {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        match self {
            Value::Nil => 0.hash(state),
            Value::Bool(b) => (1, b).hash(state),
            Value::Int(i) => (2, i).hash(state),
            Value::Float(f) => (3, f.to_bits()).hash(state),
            Value::Str(s) => (4, s).hash(state),
            Value::Symbol(s) => (5, s).hash(state),
            Value::Keyword(k) => (6, k).hash(state),
            Value::Object(id) => (7, id).hash(state),
            Value::List(items) => {
                8.hash(state);
                items.len().hash(state);
                // Hash first few items to avoid deep recursion
                for item in items.iter().take(3) {
                    item.hash(state);
                }
            },
            Value::Vector(items) => {
                9.hash(state);
                items.len().hash(state);
                for item in items.iter().take(3) {
                    item.hash(state);
                }
            },
            _ => {
                // For complex types that can't be safely hashed (Map, Set, etc.)
                // just use type discriminator
                std::mem::discriminant(self).hash(state);
            }
        }
    }
}

// Manual PartialEq implementation for Value
impl PartialEq for Value {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Value::Nil, Value::Nil) => true,
            (Value::Bool(a), Value::Bool(b)) => a == b,
            (Value::Int(a), Value::Int(b)) => a == b,
            (Value::Float(a), Value::Float(b)) => a == b,
            (Value::Str(a), Value::Str(b)) => a == b,
            (Value::Symbol(a), Value::Symbol(b)) => a == b,
            (Value::Keyword(a), Value::Keyword(b)) => a == b,
            (Value::List(a), Value::List(b)) => a == b,
            (Value::Vector(a), Value::Vector(b)) => a == b,
            (Value::Map(a), Value::Map(b)) => a == b,
            (Value::Set(a), Value::Set(b)) => a == b,
            (Value::AST(a), Value::AST(b)) => a == b,
            (Value::Quote(a), Value::Quote(b)) => a == b,
            (Value::Object(a), Value::Object(b)) => a == b,
            (Value::Closure(a), Value::Closure(b)) => Rc::ptr_eq(a, b),
            (Value::Macro(a), Value::Macro(b)) => Rc::ptr_eq(a, b),
            (Value::NativeFunction(a), Value::NativeFunction(b)) => Rc::ptr_eq(a, b),
            (Value::Capability(a), Value::Capability(b)) => a == b,
            (Value::Channel(a), Value::Channel(b)) => Rc::ptr_eq(a, b),
            (Value::WithMeta { value: a, metadata: ma }, Value::WithMeta { value: b, metadata: mb }) => {
                a == b && ma == mb
            },
            _ => false,
        }
    }
}

impl Eq for Value {}

// Manual PartialEq implementation for Closure
impl PartialEq for Closure {
    fn eq(&self, other: &Self) -> bool {
        self.params == other.params && 
        self.body == other.body &&
        self.env == other.env
    }
}

impl PartialEq for Macro {
    fn eq(&self, other: &Self) -> bool {
        self.name == other.name &&
        self.params == other.params &&
        self.body == other.body
    }
}

impl PartialEq for Channel {
    fn eq(&self, other: &Self) -> bool {
        self.id == other.id
    }
}

impl PartialEq for Environment {
    fn eq(&self, other: &Self) -> bool {
        self.bindings == other.bindings &&
        match (&self.parent, &other.parent) {
            (None, None) => true,
            (Some(a), Some(b)) => Rc::ptr_eq(a, b),
            _ => false,
        }
    }
}

// Implement Debug for Value manually since NativeFn doesn't implement Debug
impl fmt::Debug for Value {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Value::Nil => write!(f, "Nil"),
            Value::Bool(b) => write!(f, "Bool({:?})", b),
            Value::Int(i) => write!(f, "Int({:?})", i),
            Value::Float(fl) => write!(f, "Float({:?})", fl),
            Value::Str(s) => write!(f, "Str({:?})", s),
            Value::Symbol(s) => write!(f, "Symbol({:?})", s),
            Value::Keyword(k) => write!(f, "Keyword({:?})", k),
            Value::List(l) => write!(f, "List({:?})", l),
            Value::Vector(v) => write!(f, "Vector({:?})", v),
            Value::Map(m) => write!(f, "Map({:?})", m),
            Value::Set(s) => write!(f, "Set({:?})", s),
            Value::AST(a) => write!(f, "AST({:?})", a),
            Value::Quote(q) => write!(f, "Quote({:?})", q),
            Value::Object(o) => write!(f, "Object({:?})", o),
            Value::Closure(c) => write!(f, "Closure({:?})", c),
            Value::Macro(m) => write!(f, "Macro({:?})", m),
            Value::NativeFunction(_) => write!(f, "NativeFunction(#<native>)"),
            Value::Capability(c) => write!(f, "Capability({:?})", c),
            Value::Channel(c) => write!(f, "Channel({:?})", c),
            Value::WithMeta { value, .. } => write!(f, "WithMeta({:?})", value),
        }
    }
}

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Value::Nil => write!(f, "nil"),
            Value::Bool(b) => write!(f, "{}", b),
            Value::Int(i) => write!(f, "{}", i),
            Value::Float(fl) => write!(f, "{}", fl),
            Value::Str(s) => write!(f, "\"{}\"", s),
            Value::Symbol(Symbol(s)) => write!(f, "{}", s),
            Value::Keyword(Keyword(k)) => write!(f, ":{}", k),
            Value::List(items) => {
                write!(f, "(")?;
                for (i, item) in items.iter().enumerate() {
                    if i > 0 { write!(f, " ")?; }
                    write!(f, "{}", item)?;
                }
                write!(f, ")")
            }
            Value::Vector(items) => {
                write!(f, "[")?;
                for (i, item) in items.iter().enumerate() {
                    if i > 0 { write!(f, " ")?; }
                    write!(f, "{}", item)?;
                }
                write!(f, "]")
            }
            Value::Map(m) => {
                write!(f, "{{")?;
                for (i, (k, v)) in m.iter().enumerate() {
                    if i > 0 { write!(f, ", ")?; }
                    write!(f, ":{} {}", k, v)?;
                }
                write!(f, "}}")
            }
            Value::Set(s) => {
                write!(f, "#{{")?;
                for (i, item) in s.iter().enumerate() {
                    if i > 0 { write!(f, " ")?; }
                    write!(f, "{}", item)?;
                }
                write!(f, "}}")
            }
            Value::AST(node) => write!(f, "#<AST:{}>", node.node_type),
            Value::Quote(v) => write!(f, "'{}", v),
            Value::Object(ObjectId(id)) => write!(f, "#<Object:{}>", id),
            Value::Closure(_) => write!(f, "#<Closure>"),
            Value::Macro(_) => write!(f, "#<Macro>"),
            Value::NativeFunction(_) => write!(f, "#<NativeFunction>"),
            Value::Capability(cap) => write!(f, "#<Capability:{}>", cap.name),
            Value::Channel(ch) => write!(f, "#<Channel:{}>", ch.id),
            Value::WithMeta { value, .. } => write!(f, "^{{}}{}", value),
        }
    }
}

// Helper functions for creating values
impl Value {
    /// Check if value is truthy (everything except nil and false)
    pub fn is_truthy(&self) -> bool {
        !matches!(self, Value::Nil | Value::Bool(false))
    }
    
    /// Convert value to boolean
    pub fn to_bool(&self) -> bool {
        self.is_truthy()
    }
    
    /// Check if value is a list-like collection
    pub fn is_list_like(&self) -> bool {
        matches!(self, Value::List(_) | Value::Vector(_))
    }
    
    /// Get the first element of a list-like value
    pub fn first(&self) -> Option<Value> {
        match self {
            Value::List(items) | Value::Vector(items) => items.first().cloned(),
            _ => None,
        }
    }
    
    /// Get the rest of a list-like value (everything after first)
    pub fn rest(&self) -> Option<Value> {
        match self {
            Value::List(items) | Value::Vector(items) => {
                if items.is_empty() {
                    Some(Value::List(vec![]))
                } else {
                    Some(Value::List(items[1..].to_vec()))
                }
            }
            _ => None,
        }
    }
    
    /// Create a symbol value
    pub fn symbol(s: &str) -> Self {
        Value::Symbol(Symbol(s.to_string()))
    }
    
    /// Create a keyword value
    pub fn keyword(k: &str) -> Self {
        Value::Keyword(Keyword(k.to_string()))
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    
    #[test]
    fn test_value_display() {
        assert_eq!(Value::Nil.to_string(), "nil");
        assert_eq!(Value::Bool(true).to_string(), "true");
        assert_eq!(Value::Int(42).to_string(), "42");
        assert_eq!(Value::symbol("foo").to_string(), "foo");
        assert_eq!(Value::keyword("bar").to_string(), ":bar");
        assert_eq!(
            Value::List(vec![Value::Int(1), Value::Int(2)]).to_string(),
            "(1 2)"
        );
    }
    
    #[test]
    fn test_environment() {
        let mut env = Environment::new();
        let sym = Symbol("x".to_string());
        env.bind(sym.clone(), Value::Int(42));
        assert_eq!(env.lookup(&sym), Some(Value::Int(42)));
    }
}