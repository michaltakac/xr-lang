# Meta-Level Architecture in XR-Lang

XR-Lang implements a complete meta-level architecture inspired by Smalltalk-80 and CLOS (Common Lisp Object System), enabling true "live" programming with fine-grained code entity updates and runtime introspection.

## The Power of Meta

As described in the Smalltalk Blue Book (Chapter 16) and "The Art of the Metaobject Protocol", a meta-level interface provides programmatic access to both:

1. **Static Entities** - Classes, methods, functions that can be modified at runtime
2. **Dynamic Entities** - Stack frames, processes, execution contexts that can be inspected and modified while running

## Key Differentiators from Hot-Reload

Traditional hot-reload:
- Reloads entire modules or classes
- Loses all runtime state
- Breaks connections and contexts
- Requires re-navigation to test state

XR-Lang's meta-level liveness:
- Updates individual methods/functions
- Preserves all runtime state
- Maintains execution contexts
- Allows debugging with live modification

## Meta-Level Capabilities

### 1. Static Entity Modification

```lisp
;; Redefine a single function - takes effect immediately
(define calculate 
  (lambda (x) (* x 2)))  ; Original

;; Later, while running:
(define calculate 
  (lambda (x) (* x 3)))  ; Live update - no restart!

;; All existing calls now use the new definition
;; State is preserved, execution continues
```

### 2. Runtime Introspection

```lisp
;; Inspect any live object
(inspect my-object)

;; Get methods of an object
(methods-of my-cube)

;; Get class/type information
(class-of my-sphere)

;; View current stack trace
(stack-trace)
```

### 3. Interactive Debugging

The debugger allows you to:
- **Pause execution** at any point
- **Inspect live objects** - see actual runtime values
- **Modify variables** in stack frames
- **Create new code** while debugging
- **Restart frames** - re-run from a point with changes

```lisp
;; Set a breakpoint
(break 'my-function)

;; When hit, you can:
;; - Inspect all local variables
;; - Modify values
;; - Define new functions
;; - Continue with changes applied
```

### 4. Object Identity Swapping (become:)

Inspired by Smalltalk's `become:` operation:

```lisp
;; Swap identity of two objects
(become old-object new-object)

;; All references to old-object now point to new-object
;; Useful for object migration during class changes
```

## The Swimming with Fish Analogy

As described in the blog post "Swimming with Fish", working in a live environment is like:

> "Instead of looking at fish in an aquarium (traditional development), you're swimming with them. You can touch them, follow them, see how they react. You're part of the environment, not an outside observer."

In XR-Lang:
- You're **inside** your running program
- You can **touch** live objects
- You can **modify** behavior while observing effects
- You **experience** the program, not just view it

## Implementation Architecture

### Meta Interface (`vm/src/meta.rs`)

```rust
pub struct MetaInterface {
    image: Rc<RefCell<LiveImage>>,      // Live system image
    methods: HashMap<Symbol, MethodInfo>, // Method dictionary
    classes: HashMap<Symbol, ClassInfo>,  // Class hierarchy
    processes: Vec<Process>,              // Running computations
    debugger: Option<Debugger>,          // Interactive debugger
}
```

### Method Information

Each method maintains:
- Source code
- Compiled bytecode
- Local variables
- Captured variables
- Metadata

### Process Management

Processes represent running computations:
- Execution stack
- Local environment
- Continuation (what to do next)
- Priority and state

### Object Inspector

The inspector provides:
- Instance variable access
- Method listing
- Object graph visualization
- Live value modification

## Practical Examples

### Example 1: Fix Bug While Running

```lisp
;; Running code with a bug
(define calculate-price
  (lambda (quantity price)
    (* quantity price)))  ; Oops, forgot tax!

;; User reports wrong total
;; Fix it live:
(define calculate-price
  (lambda (quantity price)
    (* (* quantity price) 1.08)))  ; Add 8% tax

;; All future calculations now include tax
;; No restart, no lost shopping cart state!
```

### Example 2: Explore Unknown Object

```lisp
;; Received an object from API
(define response (fetch-api "/data"))

;; What is this object?
(inspect response)
;; => Shows all fields, values, methods

;; Try out methods interactively
(methods-of response)
;; => '(get-field set-field to-json validate)

;; Test a method
((get-field response) 'user-id)
;; => 12345

;; Now you understand the object!
```

### Example 3: Debug with Context

```lisp
(define process-item
  (lambda (item)
    (let ((result (complex-calculation item)))
      (if (valid? result)
          (save result)
          (error "Invalid result")))))

;; Set breakpoint
(break 'process-item)

;; When breakpoint hits:
;; - See actual 'item' value
;; - Inspect 'result'
;; - Modify 'valid?' if needed
;; - Continue or restart with fixes
```

## Design Principles

Following Smalltalk's design principles:

1. **Everything is an object** - Including classes and methods
2. **Objects communicate via messages** - Late binding enables live updates
3. **Reflection is fundamental** - Not an add-on but core to the system
4. **Simple, uniform mechanisms** - Same tools work at all levels

## Comparison with Other Systems

| System | Live Updates | State Preserved | Debug & Modify | Meta API |
|--------|-------------|-----------------|----------------|----------|
| XR-Lang | ✅ Individual | ✅ Full | ✅ Yes | ✅ Complete |
| Smalltalk | ✅ Individual | ✅ Full | ✅ Yes | ✅ Complete |
| Common Lisp | ✅ Individual | ✅ Full | ✅ Yes | ✅ CLOS |
| Clojure (cloxp) | ✅ Individual | ✅ Full | ✅ Yes | ✅ Partial |
| JavaScript (Lively) | ✅ Individual | ✅ Full | ✅ Yes | ✅ Partial |
| Python (reload) | ❌ Module-level | ❌ Lost | ❌ No | ❌ Limited |
| JavaScript (HMR) | ❌ Module-level | ⚠️ Partial | ❌ No | ❌ None |

## Benefits for XR Development

In XR/3D development, true liveness is especially valuable:

1. **Spatial Debugging** - Inspect 3D objects in their actual positions
2. **Live Tweaking** - Adjust positions, rotations, scales without restart
3. **State Preservation** - Keep camera position, user interactions
4. **Rapid Iteration** - See changes immediately in 3D space
5. **Context Maintenance** - Don't lose VR headset tracking or hand positions

## Try It Yourself

```bash
# Run the live demo
cargo run -- examples/live_demo.xrl

# While running:
# 1. Edit spin-speed value
# 2. Save file
# 3. See cubes spin at new speed immediately
# 4. Counter value is preserved!

# Open another terminal and connect to REPL:
# (Coming soon - REPL integration)
```

## References

1. [Smalltalk-80 Blue Book, Chapter 16](http://stephane.ducasse.free.fr/FreeBooks/BlueBook/Bluebook.pdf)
2. [The Art of the Metaobject Protocol](https://mitpress.mit.edu/books/art-metaobject-protocol)
3. [Design Principles Behind Smalltalk](https://www.cs.virginia.edu/~evans/cs655/readings/smalltalk.html)
4. [Swimming with Fish](http://simberon.blogspot.nl/2013/01/swimming-with-fish.html)
5. [Lively Kernel](http://lively-next.org/)
6. [cloxp for Clojure](http://cloxp.github.io/)

## Future Enhancements

- **REPL Integration** - Connect to running program
- **Time-travel Debugging** - Step backwards through execution
- **Collaborative Editing** - Multiple developers in same live image
- **Visual Inspector** - 3D visualization of object graphs
- **Method Versioning** - Track all changes with rollback
- **Become: for 3D** - Transform objects between types live

This is not just hot-reload. This is true liveness - the full power of Smalltalk and Lisp environments for modern XR development!