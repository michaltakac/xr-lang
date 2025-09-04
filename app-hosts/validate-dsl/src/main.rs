//! DSL Validation Tool
//! Validates XR-DSL files for syntax and semantic correctness

use std::env;
use std::fs;
use std::path::Path;
use std::process;
use dsl::parser::parse;
use dsl::lower::lower;
use colored::*;

fn main() {
    let args: Vec<String> = env::args().collect();
    
    if args.len() < 2 {
        eprintln!("Usage: {} <file.xrdsl> [--verbose]", args[0]);
        process::exit(1);
    }
    
    let file_path = &args[1];
    let verbose = args.len() > 2 && args[2] == "--verbose";
    
    // Check if file exists
    if !Path::new(file_path).exists() {
        eprintln!("{} File not found: {}", "❌".red(), file_path);
        process::exit(1);
    }
    
    // Read file content
    let content = match fs::read_to_string(file_path) {
        Ok(content) => content,
        Err(e) => {
            eprintln!("{} Failed to read file: {}", "❌".red(), e);
            process::exit(1);
        }
    };
    
    // Parse the DSL
    println!("Validating: {}", file_path.cyan());
    println!("{}", "=".repeat(50));
    
    let ast = match parse(&content) {
        Ok(ast) => {
            println!("{} Syntax validation passed", "✅".green());
            if verbose {
                println!("\nAST Structure:");
                println!("{:#?}", ast);
            }
            ast
        }
        Err(e) => {
            eprintln!("{} Syntax error: {}", "❌".red(), e);
            process::exit(1);
        }
    };
    
    // Lower to scene representation
    match lower(&ast) {
        Ok((scene_data, behaviors)) => {
            println!("{} Semantic validation passed", "✅".green());
            
            if verbose {
                println!("\nScene Data:");
                println!("  Camera: {:?}", scene_data.camera);
                println!("  Objects: {} items", scene_data.objects.len());
                for obj in &scene_data.objects {
                    println!("    - {} ({}) at {:?}", 
                        obj.id.as_ref().unwrap_or(&"unnamed".to_string()),
                        obj.mesh_type,
                        obj.position
                    );
                }
                
                if !behaviors.is_empty() {
                    println!("\nBehaviors:");
                    for behavior in &behaviors {
                        println!("  - {}", behavior.name);
                    }
                }
            } else {
                // Summary output
                println!("\nSummary:");
                println!("  📷 Camera configured");
                println!("  🎲 {} objects", scene_data.objects.len());
                println!("  ⚡ {} behaviors", behaviors.len());
            }
            
            println!("\n{} Validation complete!", "🎉".green());
        }
        Err(e) => {
            eprintln!("{} Semantic error: {}", "❌".red(), e);
            process::exit(1);
        }
    }
}