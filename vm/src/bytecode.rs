//! Bytecode VM for XR-Lang
//! 
//! A simple stack-based virtual machine that executes bytecode instructions.
//! This follows the tradition of many Lisp implementations that compile to bytecode.
//! Initially focusing on correctness over performance.

use crate::value::{Value, Symbol, Environment};
use crate::capability::CapabilityTable;
use std::rc::Rc;

/// Bytecode instructions for the stack-based VM
#[derive(Debug, Clone, PartialEq)]
pub enum OpCode {
    // Stack manipulation
    Push(Value),           // Push a value onto the stack
    Pop,                   // Pop and discard top of stack
    Dup,                   // Duplicate top of stack
    Swap,                  // Swap top two values on stack
    
    // Variable operations
    Load(Symbol),          // Load variable from environment
    Store(Symbol),         // Store top of stack to variable
    
    // Control flow
    Jump(usize),           // Unconditional jump to instruction
    JumpIf(usize),         // Jump if top of stack is truthy
    JumpIfNot(usize),      // Jump if top of stack is falsy
    Call(usize),           // Call function with N arguments
    Return,                // Return from function
    
    // Arithmetic operations (native)
    Add,
    Sub,
    Mul,
    Div,
    Mod,
    
    // Comparison operations
    Eq,
    Neq,
    Lt,
    Lte,
    Gt,
    Gte,
    
    // Logical operations
    And,
    Or,
    Not,
    
    // List operations
    Cons,                  // Construct list from top two values
    Car,                   // Get first element
    Cdr,                   // Get rest of list
    IsNull,                // Check if list is empty
    
    // Special forms
    Quote,                 // Quote the next value
    Eval,                  // Evaluate top of stack as code
    Apply,                 // Apply function to arguments
    
    // Macro operations
    MacroExpand,           // Expand macro
    
    // Meta operations
    GetMeta,               // Get metadata from value
    SetMeta,               // Set metadata on value
    
    // Debug operations
    Print,                 // Print top of stack
    Trace,                 // Enable/disable tracing
    
    // Capability operations
    CallCapability {       // Call a platform capability
        capability: String,
        method: String,
        arg_count: usize,
    },
}

/// Bytecode with source location information
#[derive(Debug, Clone)]
pub struct ByteCode {
    pub op: OpCode,
    pub line: usize,
    pub column: usize,
}

/// Virtual machine state
pub struct VM {
    /// The value stack
    stack: Vec<Value>,
    
    /// Program counter
    pc: usize,
    
    /// Current bytecode being executed
    code: Vec<ByteCode>,
    
    /// Environment for variable bindings
    env: Rc<Environment>,
    
    /// Call stack for function calls
    call_stack: Vec<CallFrame>,
    
    /// Trace execution flag
    trace: bool,
    
    /// Platform capability table
    capabilities: Option<Rc<CapabilityTable>>,
}

/// Represents a function call frame
#[derive(Debug, Clone)]
struct CallFrame {
    return_pc: usize,
    env: Rc<Environment>,
}

impl VM {
    /// Create a new VM with empty state
    pub fn new() -> Self {
        VM {
            stack: Vec::with_capacity(256),
            pc: 0,
            code: Vec::new(),
            env: Rc::new(Environment::new()),
            call_stack: Vec::with_capacity(64),
            trace: false,
            capabilities: None,
        }
    }
    
    /// Create a new VM with capability table
    pub fn with_capabilities(capabilities: Rc<CapabilityTable>) -> Self {
        let mut vm = Self::new();
        vm.capabilities = Some(capabilities);
        vm
    }
    
    /// Execute bytecode with given environment
    pub fn execute(&mut self, code: Vec<ByteCode>, env: Option<Rc<Environment>>) -> Result<Value, String> {
        self.code = code;
        self.pc = 0;
        self.stack.clear();
        self.call_stack.clear();
        
        if let Some(e) = env {
            self.env = e;
        } else {
            self.env = Rc::new(Environment::new());
        }
        
        while self.pc < self.code.len() {
            if self.trace {
                eprintln!("[{:04}] {:?} | Stack: {:?}", self.pc, self.code[self.pc], self.stack);
            }
            
            match self.code[self.pc].op.clone() {
                OpCode::Push(val) => {
                    self.stack.push(val);
                    self.pc += 1;
                }
                
                OpCode::Pop => {
                    self.stack.pop().ok_or("Stack underflow")?;
                    self.pc += 1;
                }
                
                OpCode::Dup => {
                    let val = self.stack.last().ok_or("Stack underflow")?.clone();
                    self.stack.push(val);
                    self.pc += 1;
                }
                
                OpCode::Swap => {
                    let len = self.stack.len();
                    if len < 2 {
                        return Err("Stack underflow".to_string());
                    }
                    self.stack.swap(len - 1, len - 2);
                    self.pc += 1;
                }
                
                OpCode::Load(ref sym) => {
                    let val = self.env.lookup(sym)
                        .ok_or_else(|| format!("Undefined variable: {}", sym.0))?;
                    self.stack.push(val);
                    self.pc += 1;
                }
                
                OpCode::Store(ref sym) => {
                    let val = self.stack.pop().ok_or("Stack underflow")?;
                    // For now, we'll need to clone the environment to modify it
                    // In a production system, we'd use more sophisticated environment management
                    let mut new_env = (*self.env).clone();
                    new_env.bind(sym.clone(), val);
                    self.env = Rc::new(new_env);
                    self.pc += 1;
                }
                
                OpCode::Jump(target) => {
                    self.pc = target;
                }
                
                OpCode::JumpIf(target) => {
                    let cond = self.stack.pop().ok_or("Stack underflow")?;
                    if cond.is_truthy() {
                        self.pc = target;
                    } else {
                        self.pc += 1;
                    }
                }
                
                OpCode::JumpIfNot(target) => {
                    let cond = self.stack.pop().ok_or("Stack underflow")?;
                    if !cond.is_truthy() {
                        self.pc = target;
                    } else {
                        self.pc += 1;
                    }
                }
                
                OpCode::Add => {
                    let b = self.stack.pop().ok_or("Stack underflow")?;
                    let a = self.stack.pop().ok_or("Stack underflow")?;
                    let result = match (a, b) {
                        (Value::Int(x), Value::Int(y)) => Value::Int(x + y),
                        (Value::Float(x), Value::Float(y)) => Value::Float(x + y),
                        (Value::Int(x), Value::Float(y)) => Value::Float(x as f64 + y),
                        (Value::Float(x), Value::Int(y)) => Value::Float(x + y as f64),
                        _ => return Err("Type error in addition".to_string()),
                    };
                    self.stack.push(result);
                    self.pc += 1;
                }
                
                OpCode::Sub => {
                    let b = self.stack.pop().ok_or("Stack underflow")?;
                    let a = self.stack.pop().ok_or("Stack underflow")?;
                    let result = match (a, b) {
                        (Value::Int(x), Value::Int(y)) => Value::Int(x - y),
                        (Value::Float(x), Value::Float(y)) => Value::Float(x - y),
                        (Value::Int(x), Value::Float(y)) => Value::Float(x as f64 - y),
                        (Value::Float(x), Value::Int(y)) => Value::Float(x - y as f64),
                        _ => return Err("Type error in subtraction".to_string()),
                    };
                    self.stack.push(result);
                    self.pc += 1;
                }
                
                OpCode::Mul => {
                    let b = self.stack.pop().ok_or("Stack underflow")?;
                    let a = self.stack.pop().ok_or("Stack underflow")?;
                    let result = match (a, b) {
                        (Value::Int(x), Value::Int(y)) => Value::Int(x * y),
                        (Value::Float(x), Value::Float(y)) => Value::Float(x * y),
                        (Value::Int(x), Value::Float(y)) => Value::Float(x as f64 * y),
                        (Value::Float(x), Value::Int(y)) => Value::Float(x * y as f64),
                        _ => return Err("Type error in multiplication".to_string()),
                    };
                    self.stack.push(result);
                    self.pc += 1;
                }
                
                OpCode::Div => {
                    let b = self.stack.pop().ok_or("Stack underflow")?;
                    let a = self.stack.pop().ok_or("Stack underflow")?;
                    let result = match (a, b) {
                        (Value::Int(x), Value::Int(y)) => {
                            if y == 0 { return Err("Division by zero".to_string()); }
                            Value::Int(x / y)
                        }
                        (Value::Float(x), Value::Float(y)) => {
                            if y == 0.0 { return Err("Division by zero".to_string()); }
                            Value::Float(x / y)
                        }
                        (Value::Int(x), Value::Float(y)) => {
                            if y == 0.0 { return Err("Division by zero".to_string()); }
                            Value::Float(x as f64 / y)
                        }
                        (Value::Float(x), Value::Int(y)) => {
                            if y == 0 { return Err("Division by zero".to_string()); }
                            Value::Float(x / y as f64)
                        }
                        _ => return Err("Type error in division".to_string()),
                    };
                    self.stack.push(result);
                    self.pc += 1;
                }
                
                OpCode::Eq => {
                    let b = self.stack.pop().ok_or("Stack underflow")?;
                    let a = self.stack.pop().ok_or("Stack underflow")?;
                    self.stack.push(Value::Bool(a == b));
                    self.pc += 1;
                }
                
                OpCode::Lt => {
                    let b = self.stack.pop().ok_or("Stack underflow")?;
                    let a = self.stack.pop().ok_or("Stack underflow")?;
                    let result = match (a, b) {
                        (Value::Int(x), Value::Int(y)) => Value::Bool(x < y),
                        (Value::Float(x), Value::Float(y)) => Value::Bool(x < y),
                        (Value::Int(x), Value::Float(y)) => Value::Bool((x as f64) < y),
                        (Value::Float(x), Value::Int(y)) => Value::Bool(x < (y as f64)),
                        _ => return Err("Type error in comparison".to_string()),
                    };
                    self.stack.push(result);
                    self.pc += 1;
                }
                
                OpCode::Not => {
                    let val = self.stack.pop().ok_or("Stack underflow")?;
                    self.stack.push(Value::Bool(!val.is_truthy()));
                    self.pc += 1;
                }
                
                OpCode::Print => {
                    let val = self.stack.last().ok_or("Stack underflow")?;
                    println!("{}", val);
                    self.pc += 1;
                }
                
                OpCode::Trace => {
                    self.trace = !self.trace;
                    self.pc += 1;
                }
                
                OpCode::CallCapability { capability, method, arg_count } => {
                    if self.capabilities.is_none() {
                        return Err("No capability table available".to_string());
                    }
                    
                    let mut args = Vec::with_capacity(arg_count);
                    for _ in 0..arg_count {
                        args.insert(0, self.stack.pop().ok_or("Stack underflow")?);
                    }
                    
                    let result = self.capabilities.as_ref().unwrap()
                        .call(&capability, &method, args)
                        .map_err(|e| format!("Capability error: {}", e))?;
                    
                    self.stack.push(result);
                    self.pc += 1;
                }
                
                OpCode::Cons => {
                    let b = self.stack.pop().ok_or("Stack underflow")?;
                    let a = self.stack.pop().ok_or("Stack underflow")?;
                    let result = match b {
                        Value::List(mut items) => {
                            items.insert(0, a);
                            Value::List(items)
                        }
                        _ => Value::List(vec![a, b]),
                    };
                    self.stack.push(result);
                    self.pc += 1;
                }
                
                OpCode::Car => {
                    let val = self.stack.pop().ok_or("Stack underflow")?;
                    let result = val.first().unwrap_or(Value::Nil);
                    self.stack.push(result);
                    self.pc += 1;
                }
                
                OpCode::Cdr => {
                    let val = self.stack.pop().ok_or("Stack underflow")?;
                    let result = val.rest().unwrap_or(Value::Nil);
                    self.stack.push(result);
                    self.pc += 1;
                }
                
                // TODO: Implement remaining opcodes
                _ => {
                    return Err(format!("Unimplemented opcode: {:?}", self.code[self.pc].op));
                }
            }
        }
        
        // Return the top of stack as the result
        self.stack.pop().ok_or_else(|| "Empty stack at end of execution".to_string())
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    
    #[test]
    fn test_arithmetic() {
        let mut vm = VM::new();
        let code = vec![
            ByteCode { op: OpCode::Push(Value::Int(5)), line: 1, column: 1 },
            ByteCode { op: OpCode::Push(Value::Int(3)), line: 1, column: 3 },
            ByteCode { op: OpCode::Add, line: 1, column: 5 },
        ];
        let result = vm.execute(code, None).unwrap();
        assert_eq!(result, Value::Int(8));
    }
    
    #[test]
    fn test_comparison() {
        let mut vm = VM::new();
        let code = vec![
            ByteCode { op: OpCode::Push(Value::Int(5)), line: 1, column: 1 },
            ByteCode { op: OpCode::Push(Value::Int(3)), line: 1, column: 3 },
            ByteCode { op: OpCode::Lt, line: 1, column: 5 },
        ];
        let result = vm.execute(code, None).unwrap();
        assert_eq!(result, Value::Bool(false));
    }
    
    #[test]
    fn test_list_operations() {
        let mut vm = VM::new();
        let code = vec![
            ByteCode { op: OpCode::Push(Value::Int(1)), line: 1, column: 1 },
            ByteCode { op: OpCode::Push(Value::List(vec![Value::Int(2), Value::Int(3)])), line: 1, column: 3 },
            ByteCode { op: OpCode::Cons, line: 1, column: 5 },
        ];
        let result = vm.execute(code, None).unwrap();
        assert_eq!(result, Value::List(vec![Value::Int(1), Value::Int(2), Value::Int(3)]));
    }
    
    #[test]
    fn test_capability_call() {
        use crate::capability::{create_platform_capabilities, DeviceType};
        
        let capabilities = Rc::new(create_platform_capabilities(DeviceType::Desktop));
        let mut vm = VM::with_capabilities(capabilities);
        
        let code = vec![
            ByteCode { 
                op: OpCode::Push(Value::Float(2.0)), 
                line: 1, column: 1 
            },
            ByteCode { 
                op: OpCode::CallCapability {
                    capability: "scene".to_string(),
                    method: "create_cube".to_string(),
                    arg_count: 1,
                }, 
                line: 1, column: 5 
            },
        ];
        
        let result = vm.execute(code, None).unwrap();
        if let Value::Map(map) = result {
            assert_eq!(map.get("type"), Some(&Value::Str("cube".to_string())));
            assert_eq!(map.get("size"), Some(&Value::Float(2.0)));
        } else {
            panic!("Expected Map result from capability call");
        }
    }
}