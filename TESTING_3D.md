# Testing XR-DSL 3D Rendering

This guide shows how to test the 3D rendering capabilities of XR-DSL using both standalone desktop applications and WebGPU in the browser.

## 🖥️ Desktop Testing

### Prerequisites

```bash
# Install Rust if not already installed
curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh

# Make sure you have the required system dependencies
# On macOS: Xcode Command Line Tools
# On Linux: mesa-dev, libx11-dev, libwayland-dev
# On Windows: Visual Studio Build Tools
```

### Running the Desktop 3D Demo

```bash
# From the project root
cd /Users/michaltakac/projects/questspace/xr-lang

# Run the desktop 3D renderer
cargo run -p desktop

# Or build and run in release mode for better performance
cargo run -p desktop --release
```

**What you'll see:**
- A window with animated spinning cubes
- Real-time 3D rendering using WebGPU/wgpu
- Orbit camera that rotates around the scene
- Hot-reload: edit `examples/spinning_cubes.xrdsl` to see changes live

### Desktop Controls
- The scene auto-rotates the camera
- Edit DSL files in `examples/` and they'll hot-reload automatically
- Press window close to exit

## 🌐 Browser WebGPU Testing

### Building for Web

```bash
# Navigate to web app directory
cd app-hosts/web

# Run the build script (installs wasm-pack if needed)
./build.sh

# Start the development server
./serve.py
```

**What happens:**
1. Compiles Rust to WebAssembly using wasm-pack
2. Creates a local HTTP server with proper CORS headers
3. Opens your browser automatically to `http://localhost:8000`

### Browser Features
- **Real-time WebGPU rendering** of 3D scenes
- **DSL Code Editor** - edit and reload DSL code live
- **Example scenes** - click buttons to load different examples
- **Responsive design** - works on desktop and mobile browsers

### Browser Requirements
- **Chrome/Edge 94+** with WebGPU enabled
- **Firefox Nightly** with WebGPU experimental features
- **Safari Technology Preview** (partial support)

To enable WebGPU in Chrome: `chrome://flags/#enable-webgpu`

## 📝 DSL Examples

### Simple Spinning Cube
```lisp
; Basic spinning behavior
(defbehavior spin
  (state (speed 2.0))
  (update (dt)
    (set! rotation.y (+ rotation.y (* speed dt)))))

(defscene3d simple
  (object cube1 cube
    (position 0 0 0)
    (behavior spin)))
```

### Multiple Objects Scene
```lisp
; Multiple cubes with different properties
(defbehavior spin
  (state (speed 1.0))
  (update (dt)
    (set! rotation.y (+ rotation.y (* speed dt)))))

(defscene3d demo
  (object cube1 cube
    (position -3 0 0)
    (scale 1 1 1)
    (behavior spin))
  (object cube2 cube
    (position 0 0 0)
    (scale 1.5 1.5 1.5)
    (behavior spin))
  (object cube3 cube
    (position 3 0 0)
    (scale 0.8 0.8 0.8)
    (behavior spin))
  (object ground plane
    (position 0 -2 0)
    (scale 10 1 10)))
```

### Complex Animations
```lisp
; Advanced behaviors with multiple state variables
(defbehavior float-spin
  (state (t 0.0) (speed 2.0))
  (update (dt)
    (set! t (+ t dt))
    (set! rotation.y (* t speed))
    (set! position.y (+ 1.0 (* 0.5 (sin t))))))

(defbehavior pulse
  (state (t 0.0))
  (update (dt)
    (set! t (+ t dt))
    (let ((s (+ 1.0 (* 0.3 (sin (* t 3.0))))))
      (set-scale s s s))))

(defscene3d complex
  (object floater cube
    (position -2 0 0)
    (behavior float-spin))
  (object pulser cube
    (position 2 0 0)
    (behavior pulse)))
```

## 🔧 Development Features

### Hot Reload
- **Desktop**: Edit `.xrdsl` files in `examples/` directory
- **Web**: Use the built-in code editor
- Changes apply immediately without restart

### 3D Math System
The DSL includes a complete 3D math library:
- **Vec3**: 3D vectors with standard operations
- **Mat4**: 4x4 matrices for transformations
- **Transform**: Position, rotation, scale combinations
- **Camera**: Perspective projection and view matrices

### Shader System
- **WGSL shaders** with hot-reload capability
- **Pipeline caching** for fast shader recompilation
- **Uniform buffers** for efficient GPU data transfer
- **Depth testing** for proper 3D rendering

## 🐛 Troubleshooting

### Desktop Issues
```bash
# If you get compilation errors, try:
cargo clean
cargo build

# For graphics driver issues on Linux:
sudo apt install mesa-vulkan-drivers

# For Windows, ensure you have Visual Studio Build Tools
```

### Web Issues
```bash
# If wasm-pack fails to install:
curl https://rustwasm.github.io/wasm-pack/installer/init.sh -sSf | sh

# If the server won't start:
python3 -m http.server 8000

# Then manually navigate to http://localhost:8000
```

### WebGPU Not Available
If WebGPU isn't available in your browser:
1. Update to the latest Chrome/Edge/Firefox
2. Enable WebGPU in browser flags
3. Try the fallback WebGL backend (if implemented)

## 🚀 Performance Tips

### Desktop
- Use `--release` builds for better performance
- On integrated graphics, expect 30-60 FPS
- On discrete GPUs, expect 60+ FPS

### Web
- WebGPU performance is close to native
- WebGL fallback is slower but more compatible
- Mobile browsers may have limited WebGPU support

## 📊 Expected Results

When everything works correctly, you should see:
- ✅ Smooth 3D rendering at 60 FPS
- ✅ Animated rotating cubes with colored lighting
- ✅ Orbiting camera showing the scene from multiple angles
- ✅ Real-time DSL code updates
- ✅ Proper depth testing and 3D perspective

The system demonstrates the core XR-DSL capabilities:
- **Homoiconic syntax** - code as data
- **Live programming** - hot-reload without restart
- **GPU-first architecture** - efficient 3D rendering
- **Cross-platform** - same code on desktop and web
- **Extensible** - easy to add new 3D primitives and behaviors

This foundation supports the full XR vision with spatial anchoring, hand tracking, and AI-assisted code generation in immersive environments.