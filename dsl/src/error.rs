//! Enhanced error reporting with precise location tracking
//! Combines Common Lisp-style REPL debugging with Rust-style clear error messages

use std::fmt;

/// Position in source code
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Position {
    pub line: usize,
    pub column: usize,
    pub byte_offset: usize,
}

impl Position {
    pub fn new(line: usize, column: usize, byte_offset: usize) -> Self {
        Self { line, column, byte_offset }
    }
}

impl fmt::Display for Position {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}:{}", self.line, self.column)
    }
}

/// A span in the source code
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Span {
    pub start: Position,
    pub end: Position,
}

impl Span {
    pub fn new(start: Position, end: Position) -> Self {
        Self { start, end }
    }
}

impl fmt::Display for Span {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if self.start.line == self.end.line {
            write!(f, "{}:{}-{}", self.start.line, self.start.column, self.end.column)
        } else {
            write!(f, "{}:{}-{}:{}", self.start.line, self.start.column, self.end.line, self.end.column)
        }
    }
}

/// DSL Error with location information
#[derive(Debug)]
pub struct DslError {
    pub kind: ErrorKind,
    pub span: Option<Span>,
    pub message: String,
    pub note: Option<String>,
    pub help: Option<String>,
    pub backtrace: Vec<String>,
}

#[derive(Debug, Clone)]
pub enum ErrorKind {
    // Lexer errors
    UnexpectedChar,
    UnterminatedString,
    InvalidNumber,
    
    // Parser errors
    UnexpectedToken,
    UnexpectedEof,
    UnknownForm,
    InvalidSyntax,
    
    // Semantic errors
    UndefinedVariable,
    UndefinedFunction,
    UndefinedBehavior,
    TypeMismatch,
    ArityMismatch,
    
    // Macro expansion errors
    MacroExpansionFailed,
    InvalidMacroForm,
    
    // Runtime errors
    DivisionByZero,
    IndexOutOfBounds,
    InvalidOperation,
}

impl ErrorKind {
    pub fn as_str(&self) -> &'static str {
        match self {
            ErrorKind::UnexpectedChar => "unexpected character",
            ErrorKind::UnterminatedString => "unterminated string literal",
            ErrorKind::InvalidNumber => "invalid number literal",
            ErrorKind::UnexpectedToken => "unexpected token",
            ErrorKind::UnexpectedEof => "unexpected end of file",
            ErrorKind::UnknownForm => "unknown form",
            ErrorKind::InvalidSyntax => "invalid syntax",
            ErrorKind::UndefinedVariable => "undefined variable",
            ErrorKind::UndefinedFunction => "undefined function",
            ErrorKind::UndefinedBehavior => "undefined behavior",
            ErrorKind::TypeMismatch => "type mismatch",
            ErrorKind::ArityMismatch => "arity mismatch",
            ErrorKind::MacroExpansionFailed => "macro expansion failed",
            ErrorKind::InvalidMacroForm => "invalid macro form",
            ErrorKind::DivisionByZero => "division by zero",
            ErrorKind::IndexOutOfBounds => "index out of bounds",
            ErrorKind::InvalidOperation => "invalid operation",
        }
    }
}

impl DslError {
    pub fn new(kind: ErrorKind, message: String) -> Self {
        Self {
            kind,
            span: None,
            message,
            note: None,
            help: None,
            backtrace: Vec::new(),
        }
    }
    
    pub fn with_span(mut self, span: Span) -> Self {
        self.span = Some(span);
        self
    }
    
    pub fn with_note(mut self, note: String) -> Self {
        self.note = Some(note);
        self
    }
    
    pub fn with_help(mut self, help: String) -> Self {
        self.help = Some(help);
        self
    }
    
    pub fn with_backtrace(mut self, backtrace: Vec<String>) -> Self {
        self.backtrace = backtrace;
        self
    }
    
    pub fn span(&self) -> Option<&Span> {
        self.span.as_ref()
    }
    
    /// Format error in Common Lisp REPL style with Rust-like clarity
    pub fn format_error(&self, source: &str, filename: &str) -> String {
        let mut output = String::new();
        
        // Header in Common Lisp style
        output.push_str(&format!("\n;; {}\n", "=".repeat(76)));
        output.push_str(&format!(";; ERROR: {}\n", self.kind.as_str().to_uppercase()));
        
        // Location with Rust-style formatting
        if let Some(span) = self.span {
            output.push_str(&format!(";;   --> {}:{}\n", filename, span.start));
        } else {
            output.push_str(&format!(";;   --> {}:?:?\n", filename));
        }
        
        output.push_str(";;    |\n");
        
        // Show the relevant source lines with context
        if let Some(span) = self.span {
            let lines: Vec<&str> = source.lines().collect();
            let start_line = span.start.line.saturating_sub(1);
            let end_line = (span.end.line).min(lines.len());
            
            // Show context before
            if start_line > 0 && start_line - 1 < lines.len() {
                output.push_str(&format!(";;{:4} | {}\n", start_line, lines[start_line - 1]));
            }
            
            // Show the error lines
            for line_idx in start_line..end_line {
                if line_idx < lines.len() {
                    let line_num = line_idx + 1;
                    let line = lines[line_idx];
                    output.push_str(&format!(";;{:4} | {}\n", line_num, line));
                    
                    // Add error indicator
                    if line_idx == start_line {
                        let padding = span.start.column.saturating_sub(1);
                        let width = if span.start.line == span.end.line {
                            span.end.column - span.start.column
                        } else {
                            line.len() - padding
                        };
                        output.push_str(&format!(";;    | {}{} {}\n", 
                            " ".repeat(padding),
                            "^".repeat(width.max(1)),
                            self.message));
                    }
                }
            }
            
            // Show context after
            if end_line < lines.len() {
                output.push_str(&format!(";;{:4} | {}\n", end_line + 1, lines[end_line]));
            }
        } else {
            // No span, just show the message
            output.push_str(&format!(";;    = {}\n", self.message));
        }
        
        output.push_str(";;    |\n");
        
        // Add note if present (Rust style)
        if let Some(note) = &self.note {
            output.push_str(&format!(";;    = note: {}\n", note));
        }
        
        // Add help if present (Rust style)
        if let Some(help) = &self.help {
            output.push_str(&format!(";;    = help: {}\n", help));
        }
        
        // Add backtrace if present (Common Lisp style)
        if !self.backtrace.is_empty() {
            output.push_str(";;\n");
            output.push_str(";; Backtrace:\n");
            for (i, frame) in self.backtrace.iter().enumerate() {
                output.push_str(&format!(";;   {}: {}\n", i, frame));
            }
        }
        
        output.push_str(&format!(";; {}\n", "=".repeat(76)));
        
        output
    }
}

impl fmt::Display for DslError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}: {}", self.kind.as_str(), self.message)
    }
}

impl std::error::Error for DslError {}

/// Result type for DSL operations
pub type DslResult<T> = Result<T, DslError>;

/// Helper to create arity mismatch errors
pub fn arity_error(name: &str, expected: usize, got: usize, span: Option<Span>) -> DslError {
    let mut err = DslError::new(
        ErrorKind::ArityMismatch,
        format!("{} expects {} argument{}, but got {}", 
            name, 
            expected, 
            if expected == 1 { "" } else { "s" },
            got)
    );
    
    if expected == 2 && got > 2 {
        err = err.with_help(format!(
            "use nested expressions for multiple operations: ({} a ({} b c))",
            name, name
        ));
    }
    
    if let Some(span) = span {
        err = err.with_span(span);
    }
    
    err
}

/// Helper to create undefined variable errors
pub fn undefined_var_error(name: &str, span: Option<Span>) -> DslError {
    let mut err = DslError::new(
        ErrorKind::UndefinedVariable,
        format!("variable '{}' is not defined", name)
    );
    
    err = err.with_help(format!(
        "did you mean to define it with 'let' or as a function parameter?"
    ));
    
    if let Some(span) = span {
        err = err.with_span(span);
    }
    
    err
}

/// Helper to create type mismatch errors
pub fn type_error(expected: &str, got: &str, span: Option<Span>) -> DslError {
    let mut err = DslError::new(
        ErrorKind::TypeMismatch,
        format!("expected {}, got {}", expected, got)
    );
    
    if let Some(span) = span {
        err = err.with_span(span);
    }
    
    err
}