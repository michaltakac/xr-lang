//! Debug utilities for font atlas

use std::fs::File;
use std::io::Write;

/// Save font atlas as a PPM image for debugging
pub fn save_atlas_as_ppm(atlas_data: &[u8], width: u32, height: u32, filename: &str) -> std::io::Result<()> {
    let mut file = File::create(filename)?;
    
    // PPM header
    writeln!(file, "P3")?;
    writeln!(file, "{} {}", width, height)?;
    writeln!(file, "255")?;
    
    // Write pixel data
    for y in 0..height {
        for x in 0..width {
            let idx = ((y * width + x) * 4) as usize;
            if idx + 3 < atlas_data.len() {
                let r = atlas_data[idx];
                let g = atlas_data[idx + 1];
                let b = atlas_data[idx + 2];
                write!(file, "{} {} {} ", r, g, b)?;
            }
        }
        writeln!(file)?;
    }
    
    log::info!("Saved font atlas debug image to {}", filename);
    Ok(())
}