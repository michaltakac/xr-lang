use std::fs;
use std::rc::Rc;
use vm::evaluator::Evaluator;
use vm::parser::Parser;

fn main() {
    // Read the test file
    let source = fs::read_to_string("examples/camera_test.xrl")
        .expect("Failed to read test file");
    
    // Parse and evaluate
    let mut parser = Parser::new(&source);
    let mut evaluator = Evaluator::new();
    
    loop {
        match parser.parse() {
            Ok(exprs) => {
                for expr in exprs {
                    match evaluator.eval(&expr, Rc::clone(&evaluator.global_env)) {
                        Ok(result) => {
                            // Only print non-nil results (println returns nil)
                            if !matches!(result, vm::value::Value::Nil) {
                                println!("Result: {}", result);
                            }
                        }
                        Err(e) => {
                            eprintln!("Evaluation error: {}", e);
                        }
                    }
                }
            }
            Err(e) if e == "EOF" => break,
            Err(e) => {
                eprintln!("Parse error: {}", e);
                break;
            }
        }
    }
    
    println!("\nCamera test completed!");
}