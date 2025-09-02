//! Color parsing utilities for various formats

use crate::ast::Expr;

/// Parse color from various formats
pub fn parse_color(exprs: &[Expr]) -> anyhow::Result<[f32; 4]> {
    if exprs.is_empty() {
        anyhow::bail!("Color specification cannot be empty");
    }
    
    // Check first element to determine format
    match &exprs[0] {
        Expr::Sym(s) => {
            // Could be a hex string or color format identifier
            if s.starts_with('#') {
                // HEX color
                parse_hex_color(s)
            } else if s == "hex" && exprs.len() == 2 {
                // (color hex "#FF0000")
                if let Expr::Sym(hex_str) = &exprs[1] {
                    parse_hex_color(hex_str)
                } else {
                    anyhow::bail!("hex color must be a string")
                }
            } else if s == "hsl" && exprs.len() == 4 {
                // (color hsl h s l)
                parse_hsl_color(&exprs[1..])
            } else if s == "hsla" && exprs.len() == 5 {
                // (color hsla h s l a)
                parse_hsla_color(&exprs[1..])
            } else if s == "rgba" && exprs.len() == 5 {
                // (color rgba r g b a)
                parse_rgba_color(&exprs[1..])
            } else if s == "rgb" && exprs.len() == 4 {
                // (color rgb r g b)
                parse_rgb_color(&exprs[1..])
            } else {
                // Try as direct hex string without prefix
                parse_hex_color(s)
            }
        }
        Expr::F32(_) | Expr::I32(_) => {
            // Direct RGB or RGBA values
            if exprs.len() == 3 {
                parse_rgb_color(exprs)
            } else if exprs.len() == 4 {
                parse_rgba_color(exprs)
            } else {
                anyhow::bail!("Direct color values must be RGB (3 values) or RGBA (4 values)")
            }
        }
        _ => anyhow::bail!("Invalid color format")
    }
}

/// Parse hex color string (#RGB, #RGBA, #RRGGBB, #RRGGBBAA)
fn parse_hex_color(hex: &str) -> anyhow::Result<[f32; 4]> {
    let hex = hex.trim_start_matches('#');
    
    let (r, g, b, a) = match hex.len() {
        3 => {
            // #RGB
            let r = u8::from_str_radix(&hex[0..1].repeat(2), 16)?;
            let g = u8::from_str_radix(&hex[1..2].repeat(2), 16)?;
            let b = u8::from_str_radix(&hex[2..3].repeat(2), 16)?;
            (r, g, b, 255)
        }
        4 => {
            // #RGBA
            let r = u8::from_str_radix(&hex[0..1].repeat(2), 16)?;
            let g = u8::from_str_radix(&hex[1..2].repeat(2), 16)?;
            let b = u8::from_str_radix(&hex[2..3].repeat(2), 16)?;
            let a = u8::from_str_radix(&hex[3..4].repeat(2), 16)?;
            (r, g, b, a)
        }
        6 => {
            // #RRGGBB
            let r = u8::from_str_radix(&hex[0..2], 16)?;
            let g = u8::from_str_radix(&hex[2..4], 16)?;
            let b = u8::from_str_radix(&hex[4..6], 16)?;
            (r, g, b, 255)
        }
        8 => {
            // #RRGGBBAA
            let r = u8::from_str_radix(&hex[0..2], 16)?;
            let g = u8::from_str_radix(&hex[2..4], 16)?;
            let b = u8::from_str_radix(&hex[4..6], 16)?;
            let a = u8::from_str_radix(&hex[6..8], 16)?;
            (r, g, b, a)
        }
        _ => anyhow::bail!("Invalid hex color format. Use #RGB, #RGBA, #RRGGBB, or #RRGGBBAA")
    };
    
    Ok([
        r as f32 / 255.0,
        g as f32 / 255.0,
        b as f32 / 255.0,
        a as f32 / 255.0,
    ])
}

/// Parse RGB color (values 0-1 or 0-255)
fn parse_rgb_color(exprs: &[Expr]) -> anyhow::Result<[f32; 4]> {
    if exprs.len() != 3 {
        anyhow::bail!("RGB color needs exactly 3 values");
    }
    
    let r = parse_color_component(&exprs[0])?;
    let g = parse_color_component(&exprs[1])?;
    let b = parse_color_component(&exprs[2])?;
    
    Ok([r, g, b, 1.0])
}

/// Parse RGBA color (values 0-1 or 0-255 for RGB, 0-1 for alpha)
fn parse_rgba_color(exprs: &[Expr]) -> anyhow::Result<[f32; 4]> {
    if exprs.len() != 4 {
        anyhow::bail!("RGBA color needs exactly 4 values");
    }
    
    let r = parse_color_component(&exprs[0])?;
    let g = parse_color_component(&exprs[1])?;
    let b = parse_color_component(&exprs[2])?;
    let a = parse_alpha_component(&exprs[3])?;
    
    Ok([r, g, b, a])
}

/// Parse HSL color (h: 0-360, s: 0-100, l: 0-100)
fn parse_hsl_color(exprs: &[Expr]) -> anyhow::Result<[f32; 4]> {
    if exprs.len() != 3 {
        anyhow::bail!("HSL color needs exactly 3 values");
    }
    
    let h = parse_float_expr(&exprs[0])?;
    let s = parse_float_expr(&exprs[1])?;
    let l = parse_float_expr(&exprs[2])?;
    
    let rgb = hsl_to_rgb(h, s, l);
    Ok([rgb.0, rgb.1, rgb.2, 1.0])
}

/// Parse HSLA color (h: 0-360, s: 0-100, l: 0-100, a: 0-1)
fn parse_hsla_color(exprs: &[Expr]) -> anyhow::Result<[f32; 4]> {
    if exprs.len() != 4 {
        anyhow::bail!("HSLA color needs exactly 4 values");
    }
    
    let h = parse_float_expr(&exprs[0])?;
    let s = parse_float_expr(&exprs[1])?;
    let l = parse_float_expr(&exprs[2])?;
    let a = parse_alpha_component(&exprs[3])?;
    
    let rgb = hsl_to_rgb(h, s, l);
    Ok([rgb.0, rgb.1, rgb.2, a])
}

/// Parse a color component (handles both 0-1 and 0-255 ranges)
fn parse_color_component(expr: &Expr) -> anyhow::Result<f32> {
    let value = parse_float_expr(expr)?;
    
    // If value is > 1, assume it's in 0-255 range
    if value > 1.0 {
        Ok(value / 255.0)
    } else {
        Ok(value)
    }
}

/// Parse an alpha component (always 0-1)
fn parse_alpha_component(expr: &Expr) -> anyhow::Result<f32> {
    let value = parse_float_expr(expr)?;
    
    if value < 0.0 || value > 1.0 {
        anyhow::bail!("Alpha value must be between 0 and 1");
    }
    
    Ok(value)
}

/// Parse float from expression (evaluating arithmetic if needed)
fn parse_float_expr(expr: &Expr) -> anyhow::Result<f32> {
    match expr {
        Expr::F32(f) => Ok(*f),
        Expr::I32(i) => Ok(*i as f32),
        Expr::List(parts) if !parts.is_empty() => {
            // Handle arithmetic expressions
            if let Expr::Sym(op) = &parts[0] {
                evaluate_arithmetic_expr(op, &parts[1..])
            } else {
                anyhow::bail!("Expected a number, got {:?}", expr)
            }
        }
        _ => anyhow::bail!("Expected a number, got {:?}", expr),
    }
}

/// Evaluate simple arithmetic expressions for color values
fn evaluate_arithmetic_expr(op: &str, args: &[Expr]) -> anyhow::Result<f32> {
    if args.is_empty() {
        anyhow::bail!("{} requires at least one argument", op);
    }
    
    // Recursively evaluate all arguments
    let mut values = Vec::new();
    for arg in args {
        values.push(parse_float_expr(arg)?);
    }
    
    // Compute the result based on the operation
    let result = match op {
        "+" => values.iter().sum::<f32>(),
        "-" => {
            if values.len() == 1 {
                -values[0]
            } else {
                let mut result = values[0];
                for val in &values[1..] {
                    result -= val;
                }
                result
            }
        },
        "*" => values.iter().product::<f32>(),
        "/" => {
            if values.len() < 2 {
                anyhow::bail!("/ requires at least 2 arguments");
            }
            let mut result = values[0];
            for val in &values[1..] {
                if *val == 0.0 {
                    anyhow::bail!("Division by zero");
                }
                result /= val;
            }
            result
        },
        "mod" => {
            if values.len() != 2 {
                anyhow::bail!("mod requires exactly 2 arguments");
            }
            values[0] % values[1]
        },
        _ => anyhow::bail!("Unknown arithmetic operator: {}", op),
    };
    
    Ok(result)
}

/// Convert HSL to RGB
/// h: 0-360 degrees
/// s: 0-100 percent
/// l: 0-100 percent
/// Returns RGB values in 0-1 range
fn hsl_to_rgb(h: f32, s: f32, l: f32) -> (f32, f32, f32) {
    let h = h % 360.0;
    let s = s.clamp(0.0, 100.0) / 100.0;
    let l = l.clamp(0.0, 100.0) / 100.0;
    
    if s == 0.0 {
        // Achromatic (gray)
        return (l, l, l);
    }
    
    let q = if l < 0.5 {
        l * (1.0 + s)
    } else {
        l + s - l * s
    };
    
    let p = 2.0 * l - q;
    
    let r = hue_to_rgb(p, q, h / 360.0 + 1.0 / 3.0);
    let g = hue_to_rgb(p, q, h / 360.0);
    let b = hue_to_rgb(p, q, h / 360.0 - 1.0 / 3.0);
    
    (r, g, b)
}

/// Helper function for HSL to RGB conversion
fn hue_to_rgb(p: f32, q: f32, mut t: f32) -> f32 {
    if t < 0.0 {
        t += 1.0;
    }
    if t > 1.0 {
        t -= 1.0;
    }
    
    if t < 1.0 / 6.0 {
        p + (q - p) * 6.0 * t
    } else if t < 1.0 / 2.0 {
        q
    } else if t < 2.0 / 3.0 {
        p + (q - p) * (2.0 / 3.0 - t) * 6.0
    } else {
        p
    }
}