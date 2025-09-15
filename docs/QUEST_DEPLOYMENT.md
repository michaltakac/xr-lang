# XR-Lang Quest 3 Deployment Guide

This guide explains how to develop XR-Lang applications on your MacBook Pro M1 and deploy them to Meta Quest 3 with hot-reload support.

## Prerequisites

### On MacBook Pro M1

1. **Rust toolchain**
   ```bash
   curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh
   rustup target add aarch64-linux-android
   ```

2. **Android SDK & NDK**
   ```bash
   # Install via Android Studio or command line tools
   brew install --cask android-studio
   # Or minimal installation:
   brew install android-sdk android-ndk
   ```

3. **Set environment variables** (add to ~/.zshrc)
   ```bash
   export ANDROID_HOME=$HOME/Library/Android/sdk
   export ANDROID_NDK_HOME=$ANDROID_HOME/ndk/25.2.9519653  # Adjust version
   export PATH=$PATH:$ANDROID_HOME/platform-tools
   ```

4. **Install cargo-apk**
   ```bash
   cargo install cargo-apk
   ```

5. **Install fswatch for hot-reload** (optional but recommended)
   ```bash
   brew install fswatch
   ```

### On Meta Quest 3

1. **Enable Developer Mode**
   - Open Meta Quest mobile app
   - Go to Devices → Your Quest 3
   - Settings → Developer Mode → Toggle ON

2. **Enable USB Debugging**
   - In Quest headset: Settings → System → Developer
   - Enable USB Connection Dialog
   - Connect Quest to Mac via USB-C cable
   - Accept "Allow USB Debugging" prompt in headset

3. **Enable Wi-Fi for wireless development** (optional)
   ```bash
   # While connected via USB:
   adb tcpip 5555
   # Find Quest IP: Settings → Wi-Fi → Connected Network
   adb connect 192.168.1.XXX:5555
   ```

## Building and Deploying

### Initial Setup

1. **Build the APK**
   ```bash
   cd /Users/michaltakac/projects/questspace/xr-lang
   ./scripts/quest-build.sh
   ```

2. **Deploy to Quest**
   ```bash
   ./scripts/quest-build.sh --deploy
   ```

3. **For release builds**
   ```bash
   ./scripts/quest-build.sh --release --deploy
   ```

## Hot-Reload Development Workflow

This is the killer feature - edit code on your Mac and see changes instantly in the Quest headset!

### Step 1: Launch the App on Quest

Make sure XR-Lang is running on your Quest after deployment.

### Step 2: Start Hot-Reload Client on Mac

```bash
# Automatic IP detection (Quest must be connected via ADB)
./scripts/quest-hot-reload.sh

# Or specify Quest IP manually
./scripts/quest-hot-reload.sh 192.168.1.123
```

The script will:
- Watch for changes in `examples/*.xrl` files
- Compile changes to XR-Lang bytecode image
- Send updates to Quest over network (port 9090)

### Step 3: Edit and Save

Edit any `.xrl` file in the examples directory:
```bash
vim examples/quest-demo.xrl
```

When you save, changes appear in the headset within seconds!

## Example Scene

The included `examples/quest-demo.xrl` demonstrates:
- 3D primitives (cubes, spheres, planes)
- Interactive behaviors (rotation, color changes)
- Controller input handling
- Hand tracking support
- Passthrough mode toggle
- Performance monitoring
- Hot-reload preservation of runtime state

## Features Available in Quest

### Implemented
- ✅ OpenXR integration
- ✅ Stereoscopic rendering (90Hz)
- ✅ Controller tracking
- ✅ Hand tracking (when available)
- ✅ Network hot-reload
- ✅ Passthrough mode
- ✅ Capability-based platform abstraction
- ✅ Runtime state preservation

### XR-Lang Features on Quest
- ✅ Stage A: Homoiconic value system
- ✅ Stage B: Macro system & metacircular evaluator
- ✅ Event-sourced persistence
- ✅ Time-travel debugging (in-headset!)
- ✅ Live code synchronization
- ✅ Scene DSL with behaviors
- ✅ All math functions and operators

## Development Tips

### Debugging

1. **View logs from Quest**
   ```bash
   adb logcat -s XR-Lang:V rust:V
   ```

2. **Clear logs**
   ```bash
   adb logcat -c
   ```

3. **Screenshot from headset**
   ```bash
   adb exec-out screencap -p > quest-screenshot.png
   ```

### Performance

- Quest 3 runs at 90Hz by default
- Use `(meta preserve-runtime)` to keep object states during hot-reload
- Monitor FPS with the built-in performance overlay
- Enable fixed foveated rendering for complex scenes

### Wireless Development

For untethered development:
1. Connect Quest to same Wi-Fi as Mac
2. Use `adb connect` as shown above
3. Hot-reload works over Wi-Fi!

## Troubleshooting

### "Device not found"
- Check USB connection
- Ensure Developer Mode is enabled
- Accept USB debugging prompt in headset
- Try `adb kill-server && adb start-server`

### Hot-reload not working
- Check Quest IP address
- Ensure port 9090 is not blocked
- Verify app is running on Quest
- Check network connectivity

### Build errors
- Verify Android NDK is installed
- Check ANDROID_NDK_HOME environment variable
- Ensure correct Rust target: `rustup target add aarch64-linux-android`

### App crashes on Quest
- Check logs: `adb logcat -s XR-Lang:E`
- Ensure OpenXR permissions in AndroidManifest
- Verify Vulkan support

## Network Requirements

The hot-reload feature requires network communication between your Mac and Quest:
- **Port**: 9090 (TCP)
- **Direction**: Mac → Quest
- Both devices should be on the same network
- Firewall must allow outgoing connections on Mac
- Quest automatically accepts incoming connections when app is running

## Next Steps

1. Try modifying `examples/quest-demo.xrl` while wearing the headset
2. Create your own scenes using the XR-Lang DSL
3. Experiment with hand tracking and controller input
4. Use time-travel debugging to understand behavior changes
5. Build complex procedural scenes with the macro system

Happy XR coding! 🥽✨