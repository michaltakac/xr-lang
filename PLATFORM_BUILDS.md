# Platform Build Guide for XR-Lang

## Overview

XR-Lang supports multiple platforms including Desktop (macOS, Linux, Windows), Meta Quest 3 (Android), and Web (WebAssembly). This guide explains how to build for each platform.

## Platform Support

| Platform | Status | Target | Use Case |
|----------|--------|--------|----------|
| **Desktop** | ✅ Ready | Native | Development & Testing |
| **Meta Quest 3** | ✅ Ready | Android (aarch64) | VR/AR Deployment |
| **Web** | 🚧 In Progress | WebAssembly | Browser-based XR |

## Quick Start

### Desktop Build (Default)

```bash
# Build for desktop (macOS/Linux/Windows)
cargo build --package desktop

# Or use the build script
./build.sh

# Run with example
cargo run -p desktop -- examples/spinning_cubes.xrdsl
```

### Meta Quest 3 Build

```bash
# Use the build script for Quest
./build.sh --platform quest

# Or manually build for Android
cargo build --target aarch64-linux-android --package quest
```

## Detailed Platform Instructions

### 1. Desktop Development (macOS/Linux/Windows)

The default configuration is optimized for desktop development:

```bash
# Debug build
cargo build --package desktop

# Release build (optimized)
cargo build --release --package desktop

# Run directly
cargo run -p desktop -- examples/spinning_cubes.xrdsl
```

**Requirements:**
- Rust 1.70+
- No additional dependencies

### 2. Meta Quest 3 (Android)

Building for Quest requires Android NDK and specific Rust targets.

#### Prerequisites

1. **Install Android NDK:**
   ```bash
   # macOS (using Homebrew)
   brew install android-ndk
   
   # Or download from Android Studio
   # https://developer.android.com/ndk/downloads
   ```

2. **Set Environment Variables:**
   ```bash
   # Add to ~/.zshrc or ~/.bashrc
   export ANDROID_NDK_HOME=~/Library/Android/sdk/ndk/26.1.10909125
   export PATH=$ANDROID_NDK_HOME/toolchains/llvm/prebuilt/darwin-x86_64/bin:$PATH
   ```

3. **Install Rust Target:**
   ```bash
   rustup target add aarch64-linux-android
   ```

4. **Install cargo-apk (optional, for APK generation):**
   ```bash
   cargo install cargo-apk
   ```

#### Building for Quest

```bash
# Method 1: Using build script
./build.sh --platform quest --release

# Method 2: Direct cargo build
cargo build --target aarch64-linux-android --package quest --release

# Method 3: Build APK (requires cargo-apk)
cargo apk build --package quest --release
```

#### Deploying to Quest

1. **Enable Developer Mode** on your Quest device
2. **Connect via USB** or **ADB over WiFi**
3. **Install and run:**
   ```bash
   # Install APK
   adb install target/apk/xrdsl.apk
   
   # Run the app
   adb shell am start -n com.example.xrdsl/.MainActivity
   
   # View logs
   adb logcat -s xrdsl:V
   ```

### 3. WebAssembly (Browser)

Build for web browsers with WebXR support:

```bash
# Install wasm-pack if needed
cargo install wasm-pack

# Build for web
./build.sh --platform web

# Or manually
cd app-hosts/web
wasm-pack build --release
```

**Serving the Web App:**
```bash
cd app-hosts/web
python3 -m http.server 8080
# Open http://localhost:8080
```

## Platform-Specific Features

### Desktop Features
- Hot-reload support
- File system access
- Native window management
- Full debugging capabilities

### Quest Features
- OpenXR integration
- Hand tracking
- Passthrough support
- Spatial anchors
- Performance optimizations for mobile GPU

### Web Features
- WebXR support
- WebGPU rendering
- Browser-based editor
- No installation required

## Conditional Compilation

The project uses conditional compilation to handle platform differences:

```rust
#[cfg(target_os = "android")]
use android_specific::*;

#[cfg(not(target_os = "android"))]
use desktop_specific::*;
```

## Build Configuration

The workspace is configured to exclude Android-specific dependencies from default builds:

```toml
# Cargo.toml
[workspace]
default-members = [
    "dsl", "gpu", "vm", # ... other crates
    "app-hosts/desktop",
    # "app-hosts/quest" is excluded by default
]
```

To build everything including Quest:
```bash
cargo build --workspace
```

## Troubleshooting

### Common Issues

1. **Android NDK not found**
   - Solution: Set `ANDROID_NDK_HOME` environment variable
   - Verify: `echo $ANDROID_NDK_HOME`

2. **Target not installed**
   - Solution: `rustup target add aarch64-linux-android`
   - List targets: `rustup target list`

3. **cargo-apk not found**
   - Solution: `cargo install cargo-apk`

4. **Build fails with ndk-sys error on macOS**
   - This is fixed by excluding quest from default builds
   - Use `cargo build --package desktop` instead of `cargo build`

5. **Quest app crashes on launch**
   - Check logs: `adb logcat -s xrdsl:V`
   - Ensure OpenXR permissions are granted
   - Verify Meta Quest Developer Mode is enabled

### Platform-Specific Environment Variables

```bash
# Android/Quest
export ANDROID_NDK_HOME=/path/to/ndk
export ANDROID_SDK_ROOT=/path/to/sdk

# WebAssembly
export WASM_PACK_PATH=$(which wasm-pack)

# Desktop (optional)
export RUST_LOG=debug
export RUST_BACKTRACE=1
```

## CI/CD Considerations

For GitHub Actions or other CI systems:

```yaml
# .github/workflows/build.yml
strategy:
  matrix:
    include:
      - os: ubuntu-latest
        target: x86_64-unknown-linux-gnu
      - os: macos-latest
        target: x86_64-apple-darwin
      - os: windows-latest
        target: x86_64-pc-windows-msvc
      # Android build requires special setup
      - os: ubuntu-latest
        target: aarch64-linux-android
        android: true
```

## Performance Notes

- **Desktop**: Full debug symbols, hot-reload enabled
- **Quest**: Release mode recommended, optimized for mobile GPU
- **Web**: WASM size optimization important, use `wee_alloc`

## Next Steps

1. For Quest development, see [Quest Development Guide](docs/quest-development.md)
2. For WebXR features, see [WebXR Integration](docs/webxr.md)
3. For performance optimization, see [Performance Guide](docs/performance.md)

## Support

- Desktop issues: Most common platform, well-tested
- Quest issues: Check Meta Developer Forums
- Web issues: Verify WebGPU/WebXR browser support