//! Professional REPL-style error and debug formatting
//! Combines Common Lisp REPL aesthetics with Rust's clarity

use crate::error::{DslError, ErrorKind, Position, Span};
use std::fmt::Write;

pub struct ReplFormatter {
    use_color: bool,
    context_lines: usize,
}

impl ReplFormatter {
    pub fn new() -> Self {
        Self {
            use_color: true,
            context_lines: 2,
        }
    }
    
    pub fn format_error(&self, error: &DslError, source: &str, filename: &str) -> String {
        let mut output = String::new();
        
        // Professional header
        writeln!(output, "\n{}", self.separator('─', 80)).unwrap();
        
        // Error type in Common Lisp style
        writeln!(output, "{}", self.error_header(&error.kind)).unwrap();
        
        // Location information with Rust precision
        if let Some(span) = error.span {
            writeln!(output, "  in: {}:{}", 
                self.dim(filename), 
                self.position_str(span.start)).unwrap();
        } else {
            writeln!(output, "  in: {}", self.dim(filename)).unwrap();
        }
        
        writeln!(output).unwrap();
        
        // Source context with line numbers
        if let Some(span) = error.span {
            self.format_source_context(&mut output, source, span);
        }
        
        // Error message
        writeln!(output, "  {}: {}", 
            self.bold("error"), 
            self.red(&error.message)).unwrap();
        
        // Additional information
        if let Some(note) = &error.note {
            writeln!(output, "  {}: {}", 
                self.bold("note"), 
                self.cyan(note)).unwrap();
        }
        
        if let Some(help) = &error.help {
            writeln!(output, "  {}: {}", 
                self.bold("help"), 
                self.green(help)).unwrap();
        }
        
        // Backtrace in Common Lisp style
        if !error.backtrace.is_empty() {
            writeln!(output).unwrap();
            writeln!(output, "  {}:", self.bold("Backtrace")).unwrap();
            for (i, frame) in error.backtrace.iter().enumerate() {
                writeln!(output, "    {}: {}", 
                    self.dim(&format!("[{}]", i)), 
                    frame).unwrap();
            }
        }
        
        writeln!(output, "{}", self.separator('─', 80)).unwrap();
        
        output
    }
    
    fn format_source_context(&self, output: &mut String, source: &str, span: Span) {
        let lines: Vec<&str> = source.lines().collect();
        let start_line = span.start.line.saturating_sub(1);
        let end_line = span.end.line.min(lines.len());
        
        // Calculate the width needed for line numbers
        let max_line = end_line + self.context_lines;
        let line_width = max_line.to_string().len();
        
        // Show context before
        let context_start = start_line.saturating_sub(self.context_lines);
        for line_idx in context_start..start_line {
            if line_idx < lines.len() {
                writeln!(output, "  {}{} │ {}", 
                    self.dim(&format!("{:>width$}", line_idx + 1, width = line_width)),
                    self.dim(" "),
                    self.dim(lines[line_idx])).unwrap();
            }
        }
        
        // Show the error lines
        for line_idx in start_line..end_line {
            if line_idx < lines.len() {
                let line_num = line_idx + 1;
                let line = lines[line_idx];
                
                // Highlight the error line
                writeln!(output, "  {}{} │ {}", 
                    self.yellow(&format!("{:>width$}", line_num, width = line_width)),
                    self.red(">"),
                    line).unwrap();
                
                // Add error indicator
                if line_idx == start_line {
                    let padding = span.start.column.saturating_sub(1);
                    let width = if span.start.line == span.end.line {
                        (span.end.column - span.start.column).max(1)
                    } else {
                        line.len() - padding
                    };
                    
                    writeln!(output, "  {} {} {}{}", 
                        " ".repeat(line_width),
                        self.dim("│"),
                        " ".repeat(padding),
                        self.red(&"^".repeat(width))).unwrap();
                }
            }
        }
        
        // Show context after
        for line_idx in end_line..end_line.saturating_add(self.context_lines) {
            if line_idx < lines.len() {
                writeln!(output, "  {}{} │ {}", 
                    self.dim(&format!("{:>width$}", line_idx + 1, width = line_width)),
                    self.dim(" "),
                    self.dim(lines[line_idx])).unwrap();
            }
        }
        
        writeln!(output).unwrap();
    }
    
    fn error_header(&self, kind: &ErrorKind) -> String {
        match kind {
            ErrorKind::UnexpectedChar => format!("{} {}", self.red(";;"), self.bold("READER-ERROR")),
            ErrorKind::UnterminatedString => format!("{} {}", self.red(";;"), self.bold("READER-ERROR")),
            ErrorKind::InvalidNumber => format!("{} {}", self.red(";;"), self.bold("READER-ERROR")),
            ErrorKind::UnexpectedToken => format!("{} {}", self.red(";;"), self.bold("PARSE-ERROR")),
            ErrorKind::UnexpectedEof => format!("{} {}", self.red(";;"), self.bold("UNEXPECTED-EOF")),
            ErrorKind::UnknownForm => format!("{} {}", self.red(";;"), self.bold("UNKNOWN-FORM")),
            ErrorKind::InvalidSyntax => format!("{} {}", self.red(";;"), self.bold("SYNTAX-ERROR")),
            ErrorKind::UndefinedVariable => format!("{} {}", self.red(";;"), self.bold("UNBOUND-VARIABLE")),
            ErrorKind::UndefinedFunction => format!("{} {}", self.red(";;"), self.bold("UNDEFINED-FUNCTION")),
            ErrorKind::UndefinedBehavior => format!("{} {}", self.red(";;"), self.bold("UNDEFINED-BEHAVIOR")),
            ErrorKind::TypeMismatch => format!("{} {}", self.red(";;"), self.bold("TYPE-ERROR")),
            ErrorKind::ArityMismatch => format!("{} {}", self.red(";;"), self.bold("ARITY-ERROR")),
            ErrorKind::MacroExpansionFailed => format!("{} {}", self.red(";;"), self.bold("MACRO-EXPANSION-ERROR")),
            ErrorKind::InvalidMacroForm => format!("{} {}", self.red(";;"), self.bold("INVALID-MACRO-FORM")),
            ErrorKind::DivisionByZero => format!("{} {}", self.red(";;"), self.bold("DIVISION-BY-ZERO")),
            ErrorKind::IndexOutOfBounds => format!("{} {}", self.red(";;"), self.bold("INDEX-OUT-OF-BOUNDS")),
            ErrorKind::InvalidOperation => format!("{} {}", self.red(";;"), self.bold("INVALID-OPERATION")),
        }
    }
    
    fn position_str(&self, pos: Position) -> String {
        format!("{}:{}", 
            self.bold(&pos.line.to_string()), 
            self.bold(&pos.column.to_string()))
    }
    
    fn separator(&self, ch: char, width: usize) -> String {
        self.dim(&ch.to_string().repeat(width))
    }
    
    // ANSI color helpers
    fn red(&self, s: &str) -> String {
        if self.use_color {
            format!("\x1b[31m{}\x1b[0m", s)
        } else {
            s.to_string()
        }
    }
    
    fn green(&self, s: &str) -> String {
        if self.use_color {
            format!("\x1b[32m{}\x1b[0m", s)
        } else {
            s.to_string()
        }
    }
    
    fn yellow(&self, s: &str) -> String {
        if self.use_color {
            format!("\x1b[33m{}\x1b[0m", s)
        } else {
            s.to_string()
        }
    }
    
    fn cyan(&self, s: &str) -> String {
        if self.use_color {
            format!("\x1b[36m{}\x1b[0m", s)
        } else {
            s.to_string()
        }
    }
    
    fn bold(&self, s: &str) -> String {
        if self.use_color {
            format!("\x1b[1m{}\x1b[0m", s)
        } else {
            s.to_string()
        }
    }
    
    fn dim(&self, s: &str) -> String {
        if self.use_color {
            format!("\x1b[2m{}\x1b[0m", s)
        } else {
            s.to_string()
        }
    }
}

/// Format a warning in a professional style
pub fn format_warning(message: &str, span: Option<Span>, source: &str, filename: &str) -> String {
    let formatter = ReplFormatter::new();
    let mut output = String::new();
    
    writeln!(output, "\n{}", formatter.separator('─', 80)).unwrap();
    writeln!(output, "{} {}", formatter.yellow(";;"), formatter.bold("WARNING")).unwrap();
    
    if let Some(span) = span {
        writeln!(output, "  in: {}:{}", 
            formatter.dim(filename), 
            formatter.position_str(span.start)).unwrap();
        writeln!(output).unwrap();
        formatter.format_source_context(&mut output, source, span);
    }
    
    writeln!(output, "  {}: {}", 
        formatter.bold("warning"), 
        formatter.yellow(message)).unwrap();
    
    writeln!(output, "{}", formatter.separator('─', 80)).unwrap();
    
    output
}

/// Format debug output in REPL style
pub fn format_debug_output(label: &str, value: &str) -> String {
    let formatter = ReplFormatter::new();
    format!("{} {} => {}", 
        formatter.dim(";;"),
        formatter.cyan(label),
        value)
}

/// Format a successful evaluation result
pub fn format_result(value: &str) -> String {
    let formatter = ReplFormatter::new();
    format!("{} => {}", 
        formatter.green(";;"),
        value)
}