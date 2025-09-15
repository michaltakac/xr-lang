//! XR-Lang Image Builder
//! Compiles XR-Lang source files into portable bytecode images

use anyhow::{Context, Result};
use clap::Parser;
use std::fs;
use std::path::{Path, PathBuf};
use std::collections::HashMap;
use vm::{
    image::{Platform, XRLangImage},
    parser::parse,
    compiler::Compiler,
    Value,
};

#[derive(Parser, Debug)]
#[clap(name = "xr-lang-build-image")]
#[clap(about = "Build XR-Lang portable bytecode images")]
struct Args {
    /// Input directory or file
    #[clap(short, long)]
    input: PathBuf,

    /// Output image file
    #[clap(short, long, default_value = "xr-lang.img")]
    output: PathBuf,

    /// Target platform
    #[clap(short, long, default_value = "universal")]
    platform: String,

    /// Enable verbose output
    #[clap(short, long)]
    verbose: bool,
}

fn main() -> Result<()> {
    env_logger::init();
    
    let args = Args::parse();

    if args.verbose {
        eprintln!("Building XR-Lang image...");
        eprintln!("Input: {:?}", args.input);
        eprintln!("Output: {:?}", args.output);
        eprintln!("Platform: {}", args.platform);
    }

    // Parse platform
    let platform = match args.platform.as_str() {
        "universal" => Platform::Universal,
        "quest" | "meta" => Platform::MetaQuest,
        "vision" | "visionpro" => Platform::AppleVisionPro,
        "web" | "webxr" => Platform::WebXR,
        "desktop" => Platform::Desktop,
        _ => {
            eprintln!("Unknown platform: {}", args.platform);
            Platform::Universal
        }
    };

    // Collect source files
    let source_files = collect_source_files(&args.input)?;
    if args.verbose {
        eprintln!("Found {} source files", source_files.len());
    }

    // Parse and compile all source files
    let mut globals = HashMap::new();
    let mut compiler = Compiler::new();

    for file_path in &source_files {
        if args.verbose {
            eprintln!("Processing: {:?}", file_path);
        }

        let source = fs::read_to_string(file_path)
            .with_context(|| format!("Failed to read {:?}", file_path))?;

        // Parse S-expressions
        let ast = parse(&source)
            .map_err(|e| anyhow::anyhow!("Parse error: {}", e))
            .with_context(|| format!("Failed to parse {:?}", file_path))?;

        // Store parsed values as globals
        let file_name = file_path.file_stem()
            .and_then(|s| s.to_str())
            .unwrap_or("unknown")
            .to_string();
        
        globals.insert(file_name, ast);
    }

    // Create metadata
    let mut metadata = HashMap::new();
    metadata.insert("platform".to_string(), Value::Str(format!("{:?}", platform)));
    metadata.insert("build_time".to_string(), Value::Str(chrono::Utc::now().to_rfc3339()));
    if args.verbose {
        metadata.insert("source".to_string(), Value::Str(format!("{:?}", args.input)));
    }

    // Create image
    let image = XRLangImage {
        version: 1.to_string(),
        platform,
        created_at: chrono::Utc::now(),
        globals,
        metadata,
        capabilities_required: vec![],
        snapshot_data: None,
    };

    // Serialize and write image
    let image_data = image.serialize()
        .map_err(|e| anyhow::anyhow!("Serialization error: {}", e))
        .context("Failed to serialize image")?;

    fs::write(&args.output, &image_data)
        .with_context(|| format!("Failed to write image to {:?}", args.output))?;

    if args.verbose {
        eprintln!("Successfully built image: {:?}", args.output);
        eprintln!("Image size: {} bytes", image_data.len());
        eprintln!("Platform: {:?}", platform);
    }

    Ok(())
}

fn collect_source_files(path: &Path) -> Result<Vec<PathBuf>> {
    let mut files = Vec::new();

    if path.is_file() {
        if path.extension() == Some(std::ffi::OsStr::new("xrl")) {
            files.push(path.to_path_buf());
        }
    } else if path.is_dir() {
        for entry in fs::read_dir(path)? {
            let entry = entry?;
            let path = entry.path();
            if path.extension() == Some(std::ffi::OsStr::new("xrl")) {
                files.push(path);
            }
        }
    }

    if files.is_empty() {
        anyhow::bail!("No .xrl source files found in {:?}", path);
    }

    files.sort(); // Ensure consistent ordering
    Ok(files)
}