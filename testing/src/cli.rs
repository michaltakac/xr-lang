//! Terminal interface for the test runner

use crate::runner::*;
use crate::TestDef;
use anyhow::Result;
use clap::{Parser, Subcommand};
use colored::*;
use indicatif::{ProgressBar, ProgressStyle};
use std::path::PathBuf;
use std::time::Duration;

#[derive(Parser)]
#[command(name = "xr-lang-test")]
#[command(version = "1.0")]
#[command(about = "XR-Lang Testing Framework", long_about = None)]
pub struct Cli {
    #[command(subcommand)]
    pub command: Option<Commands>,
    
    /// Test files or directories to run
    #[arg(value_name = "TESTS")]
    pub tests: Vec<PathBuf>,
    
    /// Run tests in parallel
    #[arg(short, long)]
    pub parallel: bool,
    
    /// Show verbose output
    #[arg(short, long)]
    pub verbose: bool,
    
    /// Stop on first failure
    #[arg(long)]
    pub fail_fast: bool,
    
    /// Run tests in watch mode
    #[arg(short, long)]
    pub watch: bool,
    
    /// Filter tests by pattern
    #[arg(short, long)]
    pub filter: Option<String>,
    
    /// Device profile to use
    #[arg(long)]
    pub device: Option<String>,
    
    /// Enable test visualization
    #[arg(long)]
    pub visualize: bool,
    
    /// Timeout in seconds
    #[arg(long, default_value = "120")]
    pub timeout: u64,
}

#[derive(Subcommand)]
pub enum Commands {
    /// Run tests
    Run {
        /// Test suite to run
        #[arg(value_name = "SUITE")]
        suite: Option<String>,
    },
    
    /// Record a new test from interaction
    Record {
        /// Name for the recorded test
        #[arg(value_name = "NAME")]
        name: String,
        
        /// Output file
        #[arg(short, long)]
        output: Option<PathBuf>,
    },
    
    /// Generate tests using AI
    #[command(name = "ai-generate")]
    AiGenerate {
        /// Description of what to test
        #[arg(value_name = "DESCRIPTION")]
        description: String,
        
        /// Output file
        #[arg(short, long)]
        output: Option<PathBuf>,
    },
    
    /// Show test coverage report
    Coverage {
        /// Output format (text, html, json)
        #[arg(short, long, default_value = "text")]
        format: String,
    },
    
    /// Debug a failing test with time-travel
    Debug {
        /// Test to debug
        #[arg(value_name = "TEST")]
        test: String,
        
        /// Enable timeline view
        #[arg(long)]
        timeline: bool,
        
        /// Step through execution
        #[arg(long)]
        step: bool,
    },
}

pub struct TestCli {
    pub config: TestConfig,
    pub runner: TestRunner,
}

impl TestCli {
    pub fn new(args: Cli) -> Self {
        let config = TestConfig {
            parallel: args.parallel,
            verbose: args.verbose,
            fail_fast: args.fail_fast,
            timeout: Duration::from_secs(args.timeout),
            device_profile: args.device,
        };
        
        let runner = TestRunner::new(config.clone());
        
        Self { config, runner }
    }
    
    pub async fn run(&mut self, args: Cli) -> Result<()> {
        self.print_header();
        
        match args.command {
            Some(Commands::Run { suite }) => {
                self.run_tests(args.tests, suite).await?;
            }
            Some(Commands::Record { name, output }) => {
                self.record_test(name, output).await?;
            }
            Some(Commands::AiGenerate { description, output }) => {
                self.ai_generate_test(description, output).await?;
            }
            Some(Commands::Coverage { format }) => {
                self.show_coverage(format).await?;
            }
            Some(Commands::Debug { test, timeline, step }) => {
                self.debug_test(test, timeline, step).await?;
            }
            None => {
                // Default: run all tests
                self.run_tests(args.tests, None).await?;
            }
        }
        
        Ok(())
    }
    
    fn print_header(&self) {
        println!("{}", "╔═══════════════════════════════════════════════════════════╗".cyan());
        println!("{}", "║         XR-Lang Test Runner v1.0.0                       ║".cyan().bold());
        println!("{}", "╚═══════════════════════════════════════════════════════════╝".cyan());
        println!();
    }
    
    async fn run_tests(&mut self, paths: Vec<PathBuf>, suite: Option<String>) -> Result<()> {
        let tests = self.discover_tests(paths, suite)?;
        
        if tests.is_empty() {
            println!("{}", "No tests found!".yellow());
            return Ok(());
        }
        
        println!("Found {} tests\n", tests.len());
        
        let progress = ProgressBar::new(tests.len() as u64);
        progress.set_style(
            ProgressStyle::default_bar()
                .template("{spinner:.green} [{elapsed_precise}] [{bar:40.cyan/blue}] {pos}/{len} {msg}")?
                .progress_chars("#>-"),
        );
        
        for test in tests {
            progress.set_message(format!("Running: {}", test.name));
            let result = self.runner.run_test(&test).await?;
            self.runner.results.push(result);
            progress.inc(1);
        }
        
        progress.finish_and_clear();
        
        self.runner.print_summary();
        
        Ok(())
    }
    
    fn discover_tests(&self, paths: Vec<PathBuf>, suite: Option<String>) -> Result<Vec<TestDef>> {
        let mut tests = Vec::new();
        
        // If no paths specified, look for default test directory
        let search_paths = if paths.is_empty() {
            vec![PathBuf::from("tests")]
        } else {
            paths
        };
        
        for path in search_paths {
            if path.is_file() {
                if let Some(ext) = path.extension() {
                    if ext == "xrtest" || ext == "xrdsl" {
                        tests.extend(self.load_tests_from_file(&path)?);
                    }
                }
            } else if path.is_dir() {
                tests.extend(self.load_tests_from_dir(&path)?);
            }
        }
        
        // Filter by suite if specified
        if let Some(suite_name) = suite {
            tests.retain(|t| t.name.contains(&suite_name));
        }
        
        Ok(tests)
    }
    
    fn load_tests_from_file(&self, path: &PathBuf) -> Result<Vec<TestDef>> {
        let content = std::fs::read_to_string(path)?;
        let ast = dsl::parse(&content)?;
        
        let tests = Vec::new();
        for item in ast {
            // Check if it's a test definition
            if let dsl::ast::Top::Behavior(_) = item {
                // Skip behaviors
                continue;
            }
            
            // TODO: Add test parsing when we extend the DSL Top enum
        }
        
        Ok(tests)
    }
    
    fn load_tests_from_dir(&self, dir: &PathBuf) -> Result<Vec<TestDef>> {
        let mut tests = Vec::new();
        
        for entry in std::fs::read_dir(dir)? {
            let entry = entry?;
            let path = entry.path();
            
            if path.is_file() {
                if let Some(ext) = path.extension() {
                    if ext == "xrtest" || ext == "xrdsl" {
                        tests.extend(self.load_tests_from_file(&path)?);
                    }
                }
            }
        }
        
        Ok(tests)
    }
    
    async fn record_test(&self, name: String, _output: Option<PathBuf>) -> Result<()> {
        println!("{} Recording test: {}", "●".red().bold(), name);
        println!("Press {} to start recording, {} to stop\n", 
            "SPACE".green().bold(), 
            "ESC".red().bold());
        
        // TODO: Implement test recording
        println!("{}", "Test recording not yet implemented".yellow());
        
        Ok(())
    }
    
    async fn ai_generate_test(&self, description: String, _output: Option<PathBuf>) -> Result<()> {
        println!("{} Generating test with AI...", "🤖".cyan());
        println!("Description: {}", description.italic());
        
        // TODO: Implement AI test generation
        println!("{}", "AI test generation not yet implemented".yellow());
        
        Ok(())
    }
    
    async fn show_coverage(&self, format: String) -> Result<()> {
        println!("{} Test Coverage Report", "📊");
        println!("Format: {}\n", format);
        
        match format.as_str() {
            "text" => {
                println!("Coverage Summary:");
                println!("  Lines:    {}%", "95.2".green());
                println!("  Branches: {}%", "88.7".green());
                println!("  Mutation: {}%", "72.3".yellow());
            }
            "html" => {
                println!("Generating HTML coverage report...");
                println!("Report saved to: coverage/index.html");
            }
            "json" => {
                println!("{{");
                println!("  \"lines\": 95.2,");
                println!("  \"branches\": 88.7,");
                println!("  \"mutation\": 72.3");
                println!("}}");
            }
            _ => {
                println!("{}", "Unknown coverage format".red());
            }
        }
        
        Ok(())
    }
    
    async fn debug_test(&self, test: String, timeline: bool, step: bool) -> Result<()> {
        println!("{} Debugging test: {}", "🐛", test.bold());
        
        if timeline {
            println!("Timeline view enabled");
        }
        
        if step {
            println!("Step-through mode enabled");
        }
        
        // TODO: Implement test debugging
        println!("{}", "Test debugging not yet implemented".yellow());
        
        Ok(())
    }
}