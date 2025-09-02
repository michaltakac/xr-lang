//! Rust-style colored logging without emojis

pub struct Logger {
    use_color: bool,
}

impl Logger {
    pub fn new() -> Self {
        Self {
            use_color: std::env::var("NO_COLOR").is_err(),
        }
    }
    
    // Log levels with Rust-style formatting
    pub fn info(&self, message: &str) {
        println!("{}", self.format_info(message));
    }
    
    pub fn success(&self, message: &str) {
        println!("{}", self.format_success(message));
    }
    
    pub fn warning(&self, message: &str) {
        eprintln!("{}", self.format_warning(message));
    }
    
    pub fn error(&self, message: &str) {
        eprintln!("{}", self.format_error(message));
    }
    
    pub fn debug(&self, message: &str) {
        if std::env::var("DSL_DEBUG").is_ok() {
            eprintln!("{}", self.format_debug(message));
        }
    }
    
    pub fn trace(&self, message: &str) {
        if std::env::var("DSL_TRACE").is_ok() {
            eprintln!("{}", self.format_trace(message));
        }
    }
    
    // Formatting methods
    fn format_info(&self, message: &str) -> String {
        format!("{} {}", self.cyan("INFO"), message)
    }
    
    fn format_success(&self, message: &str) -> String {
        format!("{} {}", self.green("OK"), message)
    }
    
    fn format_warning(&self, message: &str) -> String {
        format!("{} {}", self.yellow("WARN"), message)
    }
    
    fn format_error(&self, message: &str) -> String {
        format!("{} {}", self.red("ERROR"), message)
    }
    
    fn format_debug(&self, message: &str) -> String {
        format!("{} {}", self.dim("DEBUG"), self.dim(message))
    }
    
    fn format_trace(&self, message: &str) -> String {
        format!("{} {}", self.dim("TRACE"), self.dim(message))
    }
    
    // Special formats for different types of messages
    pub fn init(&self, message: &str) {
        println!("{} {}", self.bold(&self.cyan("INIT")), message);
    }
    
    pub fn parse(&self, file: &str, size: usize) {
        println!("{} {} ({} bytes)", 
            self.cyan("PARSE"), 
            file, 
            self.dim(&size.to_string()));
    }
    
    pub fn compile(&self, message: &str) {
        println!("{} {}", self.cyan("COMPILE"), message);
    }
    
    pub fn reload(&self, file: &str) {
        println!("{} {}", self.yellow("RELOAD"), file);
    }
    
    pub fn watch(&self, path: &str) {
        println!("{} {}", self.cyan("WATCH"), path);
    }
    
    // Color helpers
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

// Global logger instance
lazy_static::lazy_static! {
    pub static ref LOG: Logger = Logger::new();
}

// Convenience macros
#[macro_export]
macro_rules! log_info {
    ($($arg:tt)*) => {
        $crate::logger::LOG.info(&format!($($arg)*))
    };
}

#[macro_export]
macro_rules! log_success {
    ($($arg:tt)*) => {
        $crate::logger::LOG.success(&format!($($arg)*))
    };
}

#[macro_export]
macro_rules! log_warning {
    ($($arg:tt)*) => {
        $crate::logger::LOG.warning(&format!($($arg)*))
    };
}

#[macro_export]
macro_rules! log_error {
    ($($arg:tt)*) => {
        $crate::logger::LOG.error(&format!($($arg)*))
    };
}

#[macro_export]
macro_rules! log_debug {
    ($($arg:tt)*) => {
        $crate::logger::LOG.debug(&format!($($arg)*))
    };
}