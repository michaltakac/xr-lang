//! Default embedded font data for text rendering

/// Returns the default embedded font bytes
/// Using a simple bitmap font for now
pub fn get_default_font() -> Vec<u8> {
    // This is a placeholder - in production you would embed a real TTF font
    // For now, we'll generate a simple bitmap font programmatically
    vec![0; 1024] // Placeholder data
}