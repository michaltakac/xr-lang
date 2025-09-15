# 🚀 XR-Lang Quest 3 Quick Start

Get your XR-Lang app running on Meta Quest 3 with hot-reload in 5 minutes!

## Prerequisites Checklist

✅ Meta Quest 3 with Developer Mode enabled  
✅ MacBook Pro M1 with USB-C cable  
✅ Rust installed (`curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh`)  
✅ Android SDK/NDK installed  

## Step 1: Setup (One-Time)

```bash
# Clone and enter project
cd /Users/michaltakac/projects/questspace/xr-lang

# Install required tools
rustup target add aarch64-linux-android
cargo install cargo-apk

# Set Android environment (add to ~/.zshrc)
export ANDROID_HOME=$HOME/Library/Android/sdk
export ANDROID_NDK_HOME=$ANDROID_HOME/ndk/25.2.9519653
export PATH=$PATH:$ANDROID_HOME/platform-tools
```

## Step 2: Connect Quest 3

1. **Enable Developer Mode** on Quest (via Meta app on phone)
2. **Connect Quest to Mac** via USB-C
3. **Accept "Allow USB Debugging"** prompt in headset
4. **Verify connection:**
   ```bash
   adb devices
   # Should show your Quest device
   ```

## Step 3: Build & Deploy

```bash
# Build and deploy to Quest
./scripts/quest-build.sh --deploy

# The app will auto-launch on your Quest!
```

## Step 4: Enable Hot-Reload Magic ✨

**On your Mac:**
```bash
# Start hot-reload watcher
./scripts/quest-hot-reload.sh

# It will auto-detect your Quest's IP
# Or specify manually: ./scripts/quest-hot-reload.sh 192.168.1.123
```

## Step 5: Live Code!

Edit the example scene:
```bash
# Open in your favorite editor
vim examples/quest-demo.xrl

# Make changes like:
# - Change colors: (material :color "#ff0000")
# - Adjust positions: (position 0 2 -3)
# - Add new objects
# - Modify behaviors
```

**Save the file → See changes instantly in VR!** 🎮

## What's Happening?

When you save a `.xrl` file:
1. 📝 File watcher detects the change
2. 🔨 Compiles to XR-Lang bytecode image
3. 📡 Sends to Quest over network (port 9090)
4. 🔄 Quest hot-reloads without restart
5. 👀 You see changes immediately!

## Key Features in Quest

- **90Hz stereoscopic rendering**
- **Controller tracking & input**
- **Hand tracking support**
- **Passthrough mode** (Press 'P')
- **Runtime state preservation** during hot-reload
- **Performance monitoring overlay**
- **All XR-Lang features**: macros, time-travel debugging, etc.

## Example Controls

In `quest-demo.xrl`:
- **Controller trigger**: Changes cube color
- **Hand pinch**: Spawns particles
- **'P' key**: Toggle passthrough
- Objects marked with `(meta preserve-runtime)` keep state during reload

## Troubleshooting

### Can't find Quest?
```bash
adb kill-server
adb start-server
adb devices
```

### Hot-reload not working?
- Check Quest and Mac are on same Wi-Fi
- Verify app is running on Quest
- Check Quest IP: `adb shell ip addr show wlan0`

### Build errors?
```bash
# Verify Android NDK
echo $ANDROID_NDK_HOME
# Should show path to NDK
```

## Next Steps

1. **Try modifying** `examples/quest-demo.xrl` - change colors, add objects
2. **Create your own scene** - copy quest-demo.xrl and experiment
3. **Use hand tracking** - pinch gestures work out of the box
4. **Enable passthrough** - see your room with virtual objects

## Pro Tips

- 🔋 Keep Quest plugged in during development
- 📶 Use 5GHz Wi-Fi for faster hot-reload
- 🎯 Use `(meta preserve-runtime)` to keep object states
- 📊 Monitor FPS with the built-in overlay
- 🔍 View logs: `adb logcat -s XR-Lang:V`

---

**Ready to build amazing XR experiences with live coding? Put on your Quest and start creating!** 🥽✨