# True Liveness in XR-Lang

XR-Lang now supports **true liveness** - the ability to modify running code without restarting your application or losing state. This is inspired by Lisp and Smalltalk environments where you can redefine functions, modify data structures, and evolve your program while it runs.

## What is True Liveness?

Unlike hot-reload (which restarts your application), true liveness:
- **Preserves all runtime state** - variables, objects, execution contexts
- **Updates code in-place** - function redefinitions take effect immediately
- **Maintains execution flow** - no interruption to running computations
- **Supports incremental changes** - only changed definitions are updated

## How It Works

The implementation includes:

1. **Live Image System** (`vm/src/live_image.rs`)
   - Captures entire VM state in a persistent image
   - Tracks all definitions and their versions
   - Manages execution contexts and continuations

2. **Runtime Code Mutation** (`vm/src/evaluator.rs`)
   - Detects redefinitions and applies them live
   - Callbacks notify about changes
   - Preserves environment bindings

3. **Live XRL Runner** (`app-hosts/desktop/src/live_xrl_runner.rs`)
   - Incremental parsing to find changed definitions
   - Applies only the changes without full reload
   - Maintains scene state across updates

## Usage

### Running with Live Mode

```bash
# Run an XRL file with live mode enabled (default)
cargo run -- examples/live_demo.xrl
```

### Live Editing Workflow

1. **Start your program**
   ```bash
   cargo run -- examples/live_demo.xrl
   ```

2. **Edit the XRL file** while it's running
   - Change function definitions
   - Modify constants
   - Add new functions
   - Update object properties

3. **Save the file** - changes apply immediately!
   - No restart needed
   - State is preserved
   - Only changed code is updated

### Example: Live Demo

The `examples/live_demo.xrl` demonstrates live editing:

```lisp
;; Define rotation speed - CHANGE THIS WHILE RUNNING!
(define spin-speed 45)  ; Try changing to 90, 180, etc.

;; Animation counter - preserves state across reloads!
(define counter 0)
(define increment-counter
  (lambda ()
    (set! counter (+ counter 1))
    counter))
```

While the program runs:
1. Change `spin-speed` from 45 to 90 - cubes spin faster immediately
2. The `counter` value is preserved - it doesn't reset to 0
3. Modify the `make-cube` function - existing cubes update their behavior

## Live Update Types

### Function Redefinition
```lisp
;; Original
(define calculate (lambda (x) (* x 2)))

;; Live update - takes effect immediately
(define calculate (lambda (x) (* x 3)))
```

### Constant Updates
```lisp
;; Original
(define speed 1.0)

;; Live update - all references see new value
(define speed 2.0)
```

### Structure Changes (Future)
```lisp
;; Add fields to existing objects
;; Remove unused fields
;; Migrate existing instances automatically
```

## Implementation Details

### Live Update Detection

The system detects changes by:
1. Parsing both old and new source
2. Extracting all definitions
3. Comparing definitions to find changes
4. Applying only the changed definitions

### State Preservation

The VM maintains:
- Global environment bindings
- Object instances
- Execution contexts
- Call stacks and continuations

### Performance

Live updates are efficient:
- Incremental parsing (only parse changes)
- Selective evaluation (only eval changed definitions)
- No full scene rebuild (only update affected objects)

## Limitations

Current limitations:
- Macro changes require dependent code to be re-evaluated
- Structure changes for complex objects are simplified
- Some native functions cannot be redefined

## Future Enhancements

Planned improvements:
- Visual feedback for live changes
- Time-travel debugging with snapshots
- Live collaboration (shared image)
- Inspector for runtime state
- REPL integration

## Comparison with Hot-Reload

| Feature | Hot-Reload | True Liveness |
|---------|------------|---------------|
| Code updates | ✅ | ✅ |
| State preserved | ❌ | ✅ |
| Execution continues | ❌ | ✅ |
| Incremental updates | ❌ | ✅ |
| Debug state maintained | ❌ | ✅ |
| Connection state kept | ❌ | ✅ |

## Try It Yourself

1. Run the live demo:
   ```bash
   cargo run -- examples/live_demo.xrl
   ```

2. Open `examples/live_demo.xrl` in your editor

3. Change values and functions while watching the 3D scene

4. Experience true liveness - no restarts, no lost state!

This is the power of Lisp-style development brought to XR programming!