//! Debug configuration for controlling verbose output

/// Global debug flag - set to false to disable verbose debug output
pub const DEBUG_BEHAVIORS: bool = false;
pub const DEBUG_SCENE_LOADING: bool = false;
pub const DEBUG_HOT_RELOAD: bool = true;  // Keep this for important hot-reload messages

/// Macro for conditional debug printing
#[macro_export]
macro_rules! debug_print {
    ($flag:expr, $($arg:tt)*) => {
        if $flag {
            println!($($arg)*);
        }
    };
}