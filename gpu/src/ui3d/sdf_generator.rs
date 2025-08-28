//! Signed Distance Field generator for high-quality text rendering

/// Generate a signed distance field from a binary image
/// 
/// # Arguments
/// * `input` - Binary image data (0 or 255 values)
/// * `width` - Width of the input image
/// * `height` - Height of the input image
/// * `spread` - Maximum distance to calculate (in pixels)
/// 
/// # Returns
/// SDF data where each pixel contains the distance to the nearest edge
pub fn generate_sdf(input: &[u8], width: usize, height: usize, spread: f32) -> Vec<u8> {
    let mut output = vec![128u8; width * height];
    let spread_sq = spread * spread;
    
    // For each pixel in the output
    for y in 0..height {
        for x in 0..width {
            let idx = y * width + x;
            let is_inside = input[idx * 4 + 3] > 127; // Use alpha channel
            
            let mut min_dist_sq = spread_sq;
            
            // Search in a square region around the pixel
            let search_radius = spread.ceil() as i32;
            for dy in -search_radius..=search_radius {
                for dx in -search_radius..=search_radius {
                    let nx = x as i32 + dx;
                    let ny = y as i32 + dy;
                    
                    if nx >= 0 && nx < width as i32 && ny >= 0 && ny < height as i32 {
                        let nidx = (ny as usize) * width + (nx as usize);
                        let neighbor_inside = input[nidx * 4 + 3] > 127;
                        
                        // If we're looking for an edge (inside != neighbor)
                        if is_inside != neighbor_inside {
                            let dist_sq = (dx * dx + dy * dy) as f32;
                            min_dist_sq = min_dist_sq.min(dist_sq);
                        }
                    }
                }
            }
            
            // Convert distance to 0-255 range
            let dist = min_dist_sq.sqrt();
            let normalized = if is_inside {
                0.5 + 0.5 * (dist / spread).min(1.0)
            } else {
                0.5 - 0.5 * (dist / spread).min(1.0)
            };
            
            output[idx] = (normalized * 255.0) as u8;
        }
    }
    
    output
}

/// Fast approximate SDF using jump flooding algorithm
pub fn generate_sdf_fast(input: &[u8], width: usize, height: usize, spread: i32) -> Vec<u8> {
    // Create a distance grid
    let mut dist_grid: Vec<(i32, i32)> = vec![(i32::MAX, i32::MAX); width * height];
    let mut is_inside_grid: Vec<bool> = vec![false; width * height];
    
    // Initialize with edge pixels
    for y in 0..height {
        for x in 0..width {
            let idx = y * width + x;
            let is_inside = input[idx * 4 + 3] > 200;  // Higher threshold for better edge detection
            is_inside_grid[idx] = is_inside;
            
            // Check if this pixel is on an edge
            let mut is_edge = false;
            for dy in -1i32..=1 {
                for dx in -1i32..=1 {
                    if dx == 0 && dy == 0 { continue; }
                    
                    let nx = x as i32 + dx;
                    let ny = y as i32 + dy;
                    
                    if nx >= 0 && nx < width as i32 && ny >= 0 && ny < height as i32 {
                        let nidx = (ny as usize) * width + (nx as usize);
                        let neighbor_inside = input[nidx * 4 + 3] > 200;
                        if is_inside != neighbor_inside {
                            is_edge = true;
                            break;
                        }
                    }
                }
                if is_edge { break; }
            }
            
            if is_edge {
                dist_grid[idx] = (x as i32, y as i32);
            }
        }
    }
    
    // Jump flooding passes - limit for performance
    let max_dim = width.max(height);
    let passes = ((max_dim as f32).log2().ceil() as u32).min(8); // Limit passes for performance
    for pass in 0..passes {
        let step = 1 << (passes - 1 - pass);
        
        for y in 0..height {
            for x in 0..width {
                let idx = y * width + x;
                let current_seed = dist_grid[idx];
                
                // Check neighbors at current step distance
                for dy in -1i32..=1 {
                    for dx in -1i32..=1 {
                        let nx = x as i32 + dx * step as i32;
                        let ny = y as i32 + dy * step as i32;
                        
                        if nx >= 0 && nx < width as i32 && ny >= 0 && ny < height as i32 {
                            let nidx = (ny as usize) * width + (nx as usize);
                            let neighbor_seed = dist_grid[nidx];
                            
                            if neighbor_seed.0 != i32::MAX {
                                let dist_to_neighbor = 
                                    ((x as i32 - neighbor_seed.0).pow(2) + 
                                     (y as i32 - neighbor_seed.1).pow(2)) as f32;
                                let dist_to_current = if current_seed.0 != i32::MAX {
                                    ((x as i32 - current_seed.0).pow(2) + 
                                     (y as i32 - current_seed.1).pow(2)) as f32
                                } else {
                                    f32::MAX
                                };
                                
                                if dist_to_neighbor < dist_to_current {
                                    dist_grid[idx] = neighbor_seed;
                                }
                            }
                        }
                    }
                }
            }
        }
    }
    
    // Convert to SDF values with improved normalization
    let mut output = vec![128u8; width * height];
    let spread_f = spread as f32;
    
    for y in 0..height {
        for x in 0..width {
            let idx = y * width + x;
            let is_inside = is_inside_grid[idx];
            let seed = dist_grid[idx];
            
            if seed.0 != i32::MAX {
                let dx = (x as i32 - seed.0) as f32;
                let dy = (y as i32 - seed.1) as f32;
                let dist = (dx * dx + dy * dy).sqrt();
                
                // Improved normalization for sharper edges
                let normalized_dist = (dist / spread_f).min(1.0);
                
                let sdf_value = if is_inside {
                    // Inside: 0.5 to 1.0
                    0.5 + 0.5 * normalized_dist.powf(0.75)  // Power curve for sharper falloff
                } else {
                    // Outside: 0.0 to 0.5
                    0.5 - 0.5 * normalized_dist.powf(0.75)
                };
                
                output[idx] = (sdf_value.clamp(0.0, 1.0) * 255.0) as u8;
            } else {
                // For pixels far from any edge, set to fully inside or outside
                output[idx] = if is_inside { 255 } else { 0 };
            }
        }
    }
    
    output
}

/// Generate SDF for a single glyph with higher quality
pub fn generate_glyph_sdf(
    input: &[u8], 
    width: usize, 
    height: usize, 
    padding: usize
) -> (Vec<u8>, usize, usize) {
    // Add padding for better SDF quality
    let padded_width = width + padding * 2;
    let padded_height = height + padding * 2;
    
    // Create padded input
    let mut padded_input = vec![0u8; padded_width * padded_height * 4];
    for y in 0..height {
        for x in 0..width {
            let src_idx = (y * width + x) * 4;
            let dst_idx = ((y + padding) * padded_width + (x + padding)) * 4;
            
            padded_input[dst_idx] = input[src_idx];
            padded_input[dst_idx + 1] = input[src_idx + 1];
            padded_input[dst_idx + 2] = input[src_idx + 2];
            padded_input[dst_idx + 3] = input[src_idx + 3];
        }
    }
    
    // Generate SDF with spread based on padding
    let sdf = generate_sdf_fast(&padded_input, padded_width, padded_height, padding as i32);
    
    (sdf, padded_width, padded_height)
}