#!/bin/bash

# XR-Lang Quest 3 Build & Deploy Script
# Usage: ./quest-build.sh [--release] [--deploy]

set -e

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
NC='\033[0m' # No Color

echo -e "${GREEN}XR-Lang Quest 3 Builder${NC}"
echo "========================"

# Parse arguments
BUILD_MODE="debug"
DEPLOY=false

for arg in "$@"; do
    case $arg in
        --release)
            BUILD_MODE="release"
            shift
            ;;
        --deploy)
            DEPLOY=true
            shift
            ;;
        *)
            ;;
    esac
done

# Check for required tools
echo -e "\n${YELLOW}Checking environment...${NC}"

if ! command -v cargo &> /dev/null; then
    echo -e "${RED}Error: cargo not found. Please install Rust.${NC}"
    exit 1
fi

if ! command -v adb &> /dev/null; then
    echo -e "${RED}Error: adb not found. Please install Android SDK tools.${NC}"
    exit 1
fi

# Check for cargo-apk
if ! cargo apk --version &> /dev/null 2>&1; then
    echo -e "${YELLOW}Installing cargo-apk...${NC}"
    cargo install cargo-apk
fi

# Install Android targets if not present
if ! rustup target list --installed | grep -q "aarch64-linux-android"; then
    echo -e "${YELLOW}Installing aarch64-linux-android target...${NC}"
    rustup target add aarch64-linux-android
fi

# Set up Android NDK environment variables if not set
if [ -z "$ANDROID_NDK_HOME" ] && [ -z "$NDK_HOME" ]; then
    # Try common locations
    if [ -d "$HOME/Android/sdk/ndk" ]; then
        export ANDROID_NDK_HOME="$HOME/Android/sdk/ndk/$(ls -1 $HOME/Android/sdk/ndk | head -n1)"
    elif [ -d "$HOME/Library/Android/sdk/ndk" ]; then
        export ANDROID_NDK_HOME="$HOME/Library/Android/sdk/ndk/$(ls -1 $HOME/Library/Android/sdk/ndk | head -n1)"
    else
        echo -e "${RED}Error: Android NDK not found. Please set ANDROID_NDK_HOME.${NC}"
        exit 1
    fi
fi

# Build the APK
echo -e "\n${YELLOW}Building APK (${BUILD_MODE} mode)...${NC}"

# Try cargo-apk first, fall back to cargo-ndk if it fails
if cargo apk --help &> /dev/null 2>&1; then
    echo -e "${YELLOW}Using cargo-apk...${NC}"
    if [ "$BUILD_MODE" == "release" ]; then
        cargo apk build --package quest --release --target aarch64-linux-android
        APK_PATH="target/release/apk/xrdsl.apk"
    else
        cargo apk build --package quest --target aarch64-linux-android
        APK_PATH="target/debug/apk/xrdsl.apk"
    fi
else
    echo -e "${YELLOW}Using cargo-ndk (cargo-apk not working)...${NC}"
    
    # Build with cargo-ndk
    export ANDROID_NDK_HOME="$HOME/Library/Android/sdk/ndk/29.0.14033849"
    
    if [ "$BUILD_MODE" == "release" ]; then
        cargo ndk --target aarch64-linux-android --platform 26 -- build --package quest --release
        LIB_PATH="target/aarch64-linux-android/release/libquest.so"
    else
        cargo ndk --target aarch64-linux-android --platform 26 -- build --package quest
        LIB_PATH="target/aarch64-linux-android/debug/libquest.so"
    fi
    
    # For now, we'll just build the library
    # TODO: Package into APK manually
    echo -e "${YELLOW}Note: Built library at $LIB_PATH${NC}"
    echo -e "${YELLOW}APK packaging with cargo-ndk not yet implemented${NC}"
    echo -e "${YELLOW}Please use cargo-apk or Android Studio for full APK${NC}"
    exit 0
fi

if [ ! -f "$APK_PATH" ]; then
    echo -e "${RED}Error: APK not found at $APK_PATH${NC}"
    exit 1
fi

echo -e "${GREEN}✓ APK built successfully: $APK_PATH${NC}"

# Deploy if requested
if [ "$DEPLOY" = true ]; then
    echo -e "\n${YELLOW}Checking for Quest device...${NC}"
    
    # Check if Quest is connected
    if ! adb devices | grep -q "device$"; then
        echo -e "${RED}Error: No Quest device found. Please connect your Quest and enable Developer Mode.${NC}"
        echo -e "${YELLOW}Make sure to:${NC}"
        echo "  1. Enable Developer Mode on your Quest"
        echo "  2. Connect via USB or enable wireless ADB"
        echo "  3. Accept the RSA key fingerprint on the headset"
        exit 1
    fi
    
    DEVICE_INFO=$(adb shell getprop ro.product.model)
    echo -e "${GREEN}✓ Found device: $DEVICE_INFO${NC}"
    
    # Uninstall old version if exists
    echo -e "\n${YELLOW}Uninstalling old version...${NC}"
    adb uninstall com.example.xrdsl 2>/dev/null || true
    
    # Install new APK
    echo -e "${YELLOW}Installing APK...${NC}"
    adb install -r "$APK_PATH"
    
    if [ $? -eq 0 ]; then
        echo -e "${GREEN}✓ APK installed successfully!${NC}"
        
        # Launch the app
        echo -e "\n${YELLOW}Launching XR-Lang on Quest...${NC}"
        adb shell am start -n com.example.xrdsl/android.app.NativeActivity
        
        # Show logs
        echo -e "\n${YELLOW}Showing logs (Ctrl+C to stop)...${NC}"
        adb logcat -s XR-Lang:V rust:V
    else
        echo -e "${RED}Error: Failed to install APK${NC}"
        exit 1
    fi
else
    echo -e "\n${YELLOW}APK ready at: $APK_PATH${NC}"
    echo -e "To deploy, run: ${GREEN}./quest-build.sh --deploy${NC}"
fi