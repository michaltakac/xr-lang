# XR-Lang Examples

This directory contains examples demonstrating XR-Lang's capabilities after Stage A and Stage B implementation.

## File Extension Change
- Old: `.xrdsl` (DSL format for GPU-based implementation)
- New: `.xrl` (XR-Lang S-expression format for VM implementation)

## Working Examples

### `stage_a_homoiconicity.xrl`
Demonstrates the code-as-data principle with quote and eval, showing how code can be manipulated as data structures.
- Quote and eval
- Building code dynamically
- Manipulating code as lists

### `stage_b_macros.xrl`
Shows basic macro expansion with defmacro.
- Simple macro definitions
- Macro expansion examples
- Conditional macros

## Comprehensive Examples

### `kitchen_sink.xrl`
A complete showcase of all Stage A and Stage B features in one file, including:
- Homoiconic values and code-as-data
- Hygienic macros with quasiquote
- Metacircular evaluation
- Closures and higher-order functions
- Selective persistence policies
- Scene construction with behaviors

### `simple_demo.xrl`
A simpler working example focusing on core language features:
- Basic data types
- Functions and lambdas
- List operations
- Conditionals and bindings

### `test_vm.xrl`
Basic VM functionality tests that can be evaluated directly.

## Running Examples

Currently, these examples demonstrate the XR-Lang syntax and can be parsed successfully. 
Full evaluation support is in development.

```bash
# Build the VM
cargo build -p vm

# Test parsing of examples
cargo test -p vm

# Parse and validate syntax (examples of what will work)
# Note: Full interpreter binary coming in Stage C

# Examples that parse successfully:
# - examples/test_vm.xrl (17 expressions)
# - examples/simple_demo.xrl (14 expressions)  
# - examples/kitchen_sink.xrl (34 expressions)
# - examples/stage_a_homoiconicity.xrl (11 expressions)
# - examples/stage_b_macros.xrl (7 expressions)
```

## Feature Status

✅ **Fully Implemented:**
- S-expression parsing with EDN-like syntax
- Quote and eval for homoiconicity
- Lambda functions with closures
- Let bindings and local scope
- Arithmetic and comparison operators
- List operations (cons, car, cdr)
- Conditional evaluation (if)
- Sequential execution (begin)
- Define for global bindings
- Type predicates
- Hygienic macros with gensym
- Metacircular evaluator
- Bytecode compiler
- Selective persistence policies

🚧 **Coming in Stage C:**
- Self-modification APIs
- Bootstrap compiler in XR-Lang
- AI integration for code generation
- 3D scene graph integration
- Hot-reload with GPU rendering