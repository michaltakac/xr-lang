# XR-DSL: Domain-Specific Language for GPU-Accelerated XR Experiences

A programming DSL with Lisp/Smalltalk-like interactivity and self-introspection features, targeting GPUs through WebGPU with Rust, designed for building augmented reality environments using OpenXR API.

## Features

- **Homoiconic DSL**: Code as data with macro support, enabling powerful metaprogramming
- **Live Introspection**: Smalltalk-style object inspection and modification at runtime
- **GPU-First**: High-performance compute and graphics via WebGPU/wgpu
- **JIT Compilation**: Cranelift-based JIT for fast execution with hot-swapping
- **XR Native**: First-class OpenXR support for Quest 3, XREAL, and other devices
- **AI-Assisted**: Voice-driven code generation with Claude/Ollama integration
- **Computer Vision**: Real-world object selection and anchoring
- **Hot Reloading**: Live code updates in XR environment

## Architecture

### Core Components

- **`dsl/`** - Language frontend: parser, AST, macros
- **`ir/`** - Typed SSA intermediate representation
- **`jit/`** - Cranelift-based JIT compiler with hot-swapping
- **`vm/`** - Object space, heap management, image persistence
- **`gpu/`** - WebGPU integration with shader/pipeline caching
- **`xr/`** - OpenXR session management, anchors, input
- **`ai/`** - Voice recognition and LLM integration
- **`cv/`** - Computer vision for object selection

### Application Hosts

- **`app-hosts/desktop/`** - Development REPL and testing
- **`app-hosts/quest/`** - Native Quest 3 application

## Quick Start

### Prerequisites

```bash
# Rust toolchain
rustup target add aarch64-linux-android
cargo install cargo-ndk cargo-apk

# Android development (for Quest)
export ANDROID_HOME="$HOME/Android/Sdk"
export ANDROID_NDK_HOME="$ANDROID_HOME/ndk/26.1.10909125"

# Quest development
# Enable Developer Mode and ADB over Wi-Fi on your Quest 3
```

### Desktop Development

```bash
# Run desktop host for development
cargo run -p desktop

# Edit examples/hello_xr.xrdsl and see live updates
```

### Quest 3 Deployment

```bash
cd app-hosts/quest

# Build and install on Quest
cargo apk build --target aarch64-linux-android --release
adb connect <quest-ip>
cargo apk install --target aarch64-linux-android --release

# Launch
adb shell am start -n com.example.xrdsl/android.app.NativeActivity
```

## DSL Examples

### Basic Behavior with State

```lisp
(defbehavior pulse
  (state (t 0.0))
  (update (dt)
    (set! t (+ t dt))
    (let ((intensity (+ 0.5 (* 0.5 (sin t)))))
      (gpu.panel.color intensity 0.8 1.0 1.0))))
```

### XR Interaction

```lisp
(defbehavior interactive-menu
  (state (selected #f))
  (on_select ()
    (set! selected #t)
    (spawn-menu :items ["Info" "Pin" "Link"] :around :selected-object))
  (update (dt)
    (when selected
      (draw-outline :color [0 1 0 1] :thickness 2px))))
```

### AI-Generated Content

```lisp
(ask-ai
  :prompt "Create a 3D radial menu around the selected object"
  :context :selected-object
  :tools [:dsl-macro :wgsl-builder])
```

## Development Phases

### Phase 1: Core Language (Current)
- [x] Workspace setup
- [ ] S-expression parser and AST
- [ ] SSA IR with type system
- [ ] Basic Cranelift JIT pipeline

### Phase 2: Graphics Foundation
- [ ] WebGPU integration
- [ ] Shader compilation and caching
- [ ] Desktop rendering host
- [ ] Hot-reload pipeline

### Phase 3: XR Integration
- [ ] OpenXR session management
- [ ] Quest 3 native application
- [ ] Spatial anchoring
- [ ] Basic XR UI panels

### Phase 4: Interaction & AI
- [ ] Hand tracking and selection
- [ ] Computer vision integration
- [ ] Voice recognition
- [ ] LLM-based code generation

### Phase 5: Advanced Features
- [ ] Passthrough and MR
- [ ] Advanced shaders and compute
- [ ] Multi-device support (XREAL, etc.)
- [ ] Persistent world state

## Contributing

This project is in early development. See individual crate README files for specific contribution guidelines.

## License

Licensed under either of Apache License, Version 2.0 or MIT license at your option.