//! Compiler from XR-Lang to Bytecode
//! 
//! This compiler is written in XR-Lang itself (once bootstrapped),
//! demonstrating the language's maturity. It compiles S-expressions
//! to the bytecode that runs on our VM.

use crate::value::{Value, Symbol};
use crate::bytecode::{OpCode, ByteCode};
use std::collections::HashMap;

/// Compilation context tracking variables and labels
#[derive(Debug, Clone)]
pub struct CompileContext {
    /// Variable bindings to stack positions
    locals: Vec<Symbol>,
    /// Label positions for jumps
    labels: HashMap<String, usize>,
    /// Current line number for debug info
    line: usize,
    /// Current column for debug info
    column: usize,
}

impl CompileContext {
    pub fn new() -> Self {
        CompileContext {
            locals: Vec::new(),
            labels: HashMap::new(),
            line: 1,
            column: 1,
        }
    }

    /// Get the stack position of a local variable
    fn get_local(&self, sym: &Symbol) -> Option<usize> {
        self.locals.iter().position(|s| s == sym)
    }

    /// Push a new local variable
    fn push_local(&mut self, sym: Symbol) {
        self.locals.push(sym);
    }

    /// Pop local variables
    fn pop_locals(&mut self, count: usize) {
        for _ in 0..count {
            self.locals.pop();
        }
    }

    /// Create a bytecode instruction with source location
    fn emit(&self, op: OpCode) -> ByteCode {
        ByteCode {
            op,
            line: self.line,
            column: self.column,
        }
    }
}

/// The bytecode compiler
pub struct Compiler {
    context: CompileContext,
}

impl Compiler {
    pub fn new() -> Self {
        Compiler {
            context: CompileContext::new(),
        }
    }

    /// Compile an expression to bytecode
    pub fn compile(&mut self, expr: &Value) -> Result<Vec<ByteCode>, String> {
        self.compile_expr(expr)
    }

    /// Compile a single expression
    fn compile_expr(&mut self, expr: &Value) -> Result<Vec<ByteCode>, String> {
        match expr {
            // Literals compile to Push instructions
            Value::Nil => Ok(vec![self.context.emit(OpCode::Push(Value::Nil))]),
            Value::Bool(b) => Ok(vec![self.context.emit(OpCode::Push(Value::Bool(*b)))]),
            Value::Int(n) => Ok(vec![self.context.emit(OpCode::Push(Value::Int(*n)))]),
            Value::Float(f) => Ok(vec![self.context.emit(OpCode::Push(Value::Float(*f)))]),
            Value::Str(s) => Ok(vec![self.context.emit(OpCode::Push(Value::Str(s.clone())))]),
            
            // Variable reference compiles to Load
            Value::Symbol(sym) => {
                if let Some(pos) = self.context.get_local(sym) {
                    // Local variable - would use stack offset in real implementation
                    Ok(vec![self.context.emit(OpCode::Load(sym.clone()))])
                } else {
                    // Global variable
                    Ok(vec![self.context.emit(OpCode::Load(sym.clone()))])
                }
            }
            
            // List compilation (special forms and function calls)
            Value::List(items) if !items.is_empty() => {
                if let Value::Symbol(sym) = &items[0] {
                    match sym.0.as_str() {
                        "quote" => self.compile_quote(&items[1..]),
                        "if" => self.compile_if(&items[1..]),
                        "lambda" => self.compile_lambda(&items[1..]),
                        "let" => self.compile_let(&items[1..]),
                        "begin" | "do" => self.compile_begin(&items[1..]),
                        "define" => self.compile_define(&items[1..]),
                        "set!" => self.compile_set(&items[1..]),
                        "+" => self.compile_arithmetic(OpCode::Add, &items[1..]),
                        "-" => self.compile_arithmetic(OpCode::Sub, &items[1..]),
                        "*" => self.compile_arithmetic(OpCode::Mul, &items[1..]),
                        "/" => self.compile_arithmetic(OpCode::Div, &items[1..]),
                        "=" => self.compile_comparison(OpCode::Eq, &items[1..]),
                        "<" => self.compile_comparison(OpCode::Lt, &items[1..]),
                        ">" => self.compile_comparison(OpCode::Gt, &items[1..]),
                        "cons" => self.compile_list_op(OpCode::Cons, &items[1..]),
                        "car" => self.compile_list_op(OpCode::Car, &items[1..]),
                        "cdr" => self.compile_list_op(OpCode::Cdr, &items[1..]),
                        _ => self.compile_call(sym, &items[1..]),
                    }
                } else {
                    // Compile as function call with computed function
                    self.compile_apply(&items[0], &items[1..])
                }
            }
            
            Value::List(_) => {
                // Empty list
                Ok(vec![self.context.emit(OpCode::Push(Value::Nil))])
            }
            
            // Vectors compile their elements
            Value::Vector(items) => {
                let mut code = Vec::new();
                for item in items {
                    code.extend(self.compile_expr(item)?);
                }
                // Would need a MakeVector opcode in real implementation
                Ok(code)
            }
            
            _ => Err(format!("Cannot compile expression: {:?}", expr)),
        }
    }

    /// Compile quote special form
    fn compile_quote(&mut self, args: &[Value]) -> Result<Vec<ByteCode>, String> {
        if args.len() != 1 {
            return Err("quote expects exactly 1 argument".to_string());
        }
        Ok(vec![self.context.emit(OpCode::Push(args[0].clone()))])
    }

    /// Compile if special form
    fn compile_if(&mut self, args: &[Value]) -> Result<Vec<ByteCode>, String> {
        if args.len() < 2 || args.len() > 3 {
            return Err("if expects 2 or 3 arguments".to_string());
        }
        
        let mut code = Vec::new();
        
        // Compile condition
        code.extend(self.compile_expr(&args[0])?);
        
        // Compile then branch
        let then_code = self.compile_expr(&args[1])?;
        
        // Compile else branch (if present)
        let else_code = if args.len() == 3 {
            self.compile_expr(&args[2])?
        } else {
            vec![self.context.emit(OpCode::Push(Value::Nil))]
        };
        
        // Generate jump instructions
        // JumpIfNot to else branch
        code.push(self.context.emit(OpCode::JumpIfNot(then_code.len() + 1)));
        code.extend(then_code);
        // Jump over else branch
        code.push(self.context.emit(OpCode::Jump(else_code.len())));
        code.extend(else_code);
        
        Ok(code)
    }

    /// Compile lambda special form
    fn compile_lambda(&mut self, args: &[Value]) -> Result<Vec<ByteCode>, String> {
        if args.len() < 2 {
            return Err("lambda expects at least 2 arguments".to_string());
        }
        
        // Parse parameters
        let params = match &args[0] {
            Value::List(items) => {
                let mut params = Vec::new();
                for item in items {
                    match item {
                        Value::Symbol(sym) => params.push(sym.clone()),
                        _ => return Err("lambda parameters must be symbols".to_string()),
                    }
                }
                params
            }
            _ => return Err("lambda expects a parameter list".to_string()),
        };
        
        // Save current context
        let saved_locals = self.context.locals.clone();
        
        // Add parameters to local context
        for param in &params {
            self.context.push_local(param.clone());
        }
        
        // Compile body
        let mut body_code = Vec::new();
        for expr in &args[1..] {
            body_code.extend(self.compile_expr(expr)?);
        }
        body_code.push(self.context.emit(OpCode::Return));
        
        // Restore context
        self.context.locals = saved_locals;
        
        // In a real implementation, we'd create a closure object here
        // For now, we'll just push the compiled code as data
        Ok(vec![self.context.emit(OpCode::Push(Value::List(vec![
            Value::Symbol(Symbol("compiled-lambda".to_string())),
            Value::List(params.into_iter().map(Value::Symbol).collect()),
            // Would store bytecode here in real implementation
        ])))])
    }

    /// Compile let special form
    fn compile_let(&mut self, args: &[Value]) -> Result<Vec<ByteCode>, String> {
        if args.len() < 2 {
            return Err("let expects at least 2 arguments".to_string());
        }
        
        let mut code = Vec::new();
        let mut local_count = 0;
        
        // Compile bindings
        match &args[0] {
            Value::List(bindings) => {
                for binding in bindings {
                    match binding {
                        Value::List(pair) if pair.len() == 2 => {
                            match &pair[0] {
                                Value::Symbol(sym) => {
                                    // Compile value expression
                                    code.extend(self.compile_expr(&pair[1])?);
                                    // Add to locals
                                    self.context.push_local(sym.clone());
                                    local_count += 1;
                                }
                                _ => return Err("let binding must start with symbol".to_string()),
                            }
                        }
                        _ => return Err("let bindings must be pairs".to_string()),
                    }
                }
            }
            _ => return Err("let expects a list of bindings".to_string()),
        }
        
        // Compile body
        for expr in &args[1..] {
            code.extend(self.compile_expr(expr)?);
        }
        
        // Clean up locals
        self.context.pop_locals(local_count);
        for _ in 0..local_count {
            code.push(self.context.emit(OpCode::Pop));
        }
        
        Ok(code)
    }

    /// Compile begin/do special form
    fn compile_begin(&mut self, args: &[Value]) -> Result<Vec<ByteCode>, String> {
        if args.is_empty() {
            return Ok(vec![self.context.emit(OpCode::Push(Value::Nil))]);
        }
        
        let mut code = Vec::new();
        
        // Compile all expressions, popping intermediate results
        for (i, expr) in args.iter().enumerate() {
            code.extend(self.compile_expr(expr)?);
            if i < args.len() - 1 {
                code.push(self.context.emit(OpCode::Pop));
            }
        }
        
        Ok(code)
    }

    /// Compile define special form
    fn compile_define(&mut self, args: &[Value]) -> Result<Vec<ByteCode>, String> {
        if args.len() != 2 {
            return Err("define expects exactly 2 arguments".to_string());
        }
        
        match &args[0] {
            Value::Symbol(sym) => {
                let mut code = self.compile_expr(&args[1])?;
                code.push(self.context.emit(OpCode::Store(sym.clone())));
                Ok(code)
            }
            _ => Err("define expects a symbol as first argument".to_string()),
        }
    }

    /// Compile set! special form
    fn compile_set(&mut self, args: &[Value]) -> Result<Vec<ByteCode>, String> {
        if args.len() != 2 {
            return Err("set! expects exactly 2 arguments".to_string());
        }
        
        match &args[0] {
            Value::Symbol(sym) => {
                let mut code = self.compile_expr(&args[1])?;
                code.push(self.context.emit(OpCode::Store(sym.clone())));
                Ok(code)
            }
            _ => Err("set! expects a symbol as first argument".to_string()),
        }
    }

    /// Compile arithmetic operations
    fn compile_arithmetic(&mut self, op: OpCode, args: &[Value]) -> Result<Vec<ByteCode>, String> {
        if args.len() < 2 {
            return Err(format!("Arithmetic operation expects at least 2 arguments"));
        }
        
        let mut code = Vec::new();
        
        // Compile first argument
        code.extend(self.compile_expr(&args[0])?);
        
        // Compile remaining arguments with operation
        for arg in &args[1..] {
            code.extend(self.compile_expr(arg)?);
            code.push(self.context.emit(op.clone()));
        }
        
        Ok(code)
    }

    /// Compile comparison operations
    fn compile_comparison(&mut self, op: OpCode, args: &[Value]) -> Result<Vec<ByteCode>, String> {
        if args.len() != 2 {
            return Err("Comparison expects exactly 2 arguments".to_string());
        }
        
        let mut code = Vec::new();
        code.extend(self.compile_expr(&args[0])?);
        code.extend(self.compile_expr(&args[1])?);
        code.push(self.context.emit(op));
        
        Ok(code)
    }

    /// Compile list operations
    fn compile_list_op(&mut self, op: OpCode, args: &[Value]) -> Result<Vec<ByteCode>, String> {
        let expected = match op {
            OpCode::Cons => 2,
            OpCode::Car | OpCode::Cdr => 1,
            _ => return Err("Invalid list operation".to_string()),
        };
        
        if args.len() != expected {
            return Err(format!("List operation expects {} arguments", expected));
        }
        
        let mut code = Vec::new();
        for arg in args {
            code.extend(self.compile_expr(arg)?);
        }
        code.push(self.context.emit(op));
        
        Ok(code)
    }

    /// Compile function call
    fn compile_call(&mut self, func: &Symbol, args: &[Value]) -> Result<Vec<ByteCode>, String> {
        let mut code = Vec::new();
        
        // Push arguments
        for arg in args {
            code.extend(self.compile_expr(arg)?);
        }
        
        // Push function
        code.push(self.context.emit(OpCode::Load(func.clone())));
        
        // Call with argument count
        code.push(self.context.emit(OpCode::Call(args.len())));
        
        Ok(code)
    }

    /// Compile computed function application
    fn compile_apply(&mut self, func: &Value, args: &[Value]) -> Result<Vec<ByteCode>, String> {
        let mut code = Vec::new();
        
        // Push arguments
        for arg in args {
            code.extend(self.compile_expr(arg)?);
        }
        
        // Push function
        code.extend(self.compile_expr(func)?);
        
        // Call with argument count
        code.push(self.context.emit(OpCode::Call(args.len())));
        
        Ok(code)
    }
}

/// Compile a complete program
pub fn compile_program(exprs: &[Value]) -> Result<Vec<ByteCode>, String> {
    let mut compiler = Compiler::new();
    let mut code = Vec::new();
    
    for expr in exprs {
        code.extend(compiler.compile(expr)?);
    }
    
    // Add final return if not present
    if code.is_empty() || !matches!(code.last().unwrap().op, OpCode::Return) {
        code.push(ByteCode {
            op: OpCode::Return,
            line: 1,
            column: 1,
        });
    }
    
    Ok(code)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::parser::parse_one;

    #[test]
    fn test_compile_literal() {
        let mut compiler = Compiler::new();
        
        let code = compiler.compile(&Value::Int(42)).unwrap();
        assert_eq!(code.len(), 1);
        assert_eq!(code[0].op, OpCode::Push(Value::Int(42)));
    }

    #[test]
    fn test_compile_arithmetic() {
        let mut compiler = Compiler::new();
        
        let expr = parse_one("(+ 1 2 3)").unwrap();
        let code = compiler.compile(&expr).unwrap();
        
        // Should compile to: Push(1) Push(2) Add Push(3) Add
        assert_eq!(code.len(), 5);
        assert_eq!(code[0].op, OpCode::Push(Value::Int(1)));
        assert_eq!(code[1].op, OpCode::Push(Value::Int(2)));
        assert_eq!(code[2].op, OpCode::Add);
        assert_eq!(code[3].op, OpCode::Push(Value::Int(3)));
        assert_eq!(code[4].op, OpCode::Add);
    }

    #[test]
    fn test_compile_if() {
        let mut compiler = Compiler::new();
        
        let expr = parse_one("(if #t 1 2)").unwrap();
        let code = compiler.compile(&expr).unwrap();
        
        // Should have condition, jump, then branch, jump, else branch
        assert!(code.len() >= 5);
        assert_eq!(code[0].op, OpCode::Push(Value::Bool(true)));
    }

    #[test]
    fn test_compile_let() {
        let mut compiler = Compiler::new();
        
        let expr = parse_one("(let ((x 3)) x)").unwrap();
        let code = compiler.compile(&expr).unwrap();
        
        // Should push value, load it, then pop
        assert!(code.len() >= 3);
    }

    #[test]
    fn test_compile_program() {
        let exprs = vec![
            parse_one("(define x 10)").unwrap(),
            parse_one("(+ x 5)").unwrap(),
        ];
        
        let code = compile_program(&exprs).unwrap();
        
        // Should have both expressions compiled plus Return
        assert!(code.len() > 2);
        assert_eq!(code.last().unwrap().op, OpCode::Return);
    }
}