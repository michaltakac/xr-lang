#!/bin/bash
# Build script for XR-Lang with platform-specific support

set -e

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

# Default values
PLATFORM="desktop"
BUILD_TYPE="debug"
TARGET=""

# Parse arguments
while [[ $# -gt 0 ]]; do
    case $1 in
        --platform)
            PLATFORM="$2"
            shift 2
            ;;
        --release)
            BUILD_TYPE="release"
            shift
            ;;
        --target)
            TARGET="$2"
            shift 2
            ;;
        --help)
            echo "Usage: ./build.sh [options]"
            echo ""
            echo "Options:"
            echo "  --platform <platform>  Build for specific platform (desktop, quest, web)"
            echo "  --release             Build in release mode"
            echo "  --target <target>     Specify target triple"
            echo "  --help                Show this help message"
            echo ""
            echo "Examples:"
            echo "  ./build.sh                    # Build for desktop (default)"
            echo "  ./build.sh --platform quest   # Build for Meta Quest"
            echo "  ./build.sh --platform web     # Build for WebAssembly"
            echo "  ./build.sh --release          # Build optimized release"
            exit 0
            ;;
        *)
            echo -e "${RED}Unknown option: $1${NC}"
            exit 1
            ;;
    esac
done

# Build based on platform
case $PLATFORM in
    desktop)
        echo -e "${BLUE}Building for Desktop (macOS/Linux/Windows)...${NC}"
        if [ "$BUILD_TYPE" = "release" ]; then
            cargo build --release --package desktop
            echo -e "${GREEN}✅ Desktop build complete: target/release/xr-dsl-desktop${NC}"
        else
            cargo build --package desktop
            echo -e "${GREEN}✅ Desktop build complete: target/debug/xr-dsl-desktop${NC}"
        fi
        ;;
        
    quest)
        echo -e "${BLUE}Building for Meta Quest (Android)...${NC}"
        
        # Check if Android NDK is installed
        if [ -z "$ANDROID_NDK_HOME" ]; then
            echo -e "${YELLOW}Warning: ANDROID_NDK_HOME not set${NC}"
            echo "Please install Android NDK and set ANDROID_NDK_HOME environment variable"
            echo ""
            echo "Installation instructions:"
            echo "1. Install Android Studio or Android SDK"
            echo "2. Install NDK through SDK Manager"
            echo "3. Set ANDROID_NDK_HOME to NDK location"
            echo "   export ANDROID_NDK_HOME=~/Library/Android/sdk/ndk/<version>"
            exit 1
        fi
        
        # Set up Android target
        TARGET="aarch64-linux-android"
        
        # Check if target is installed
        if ! rustup target list | grep -q "$TARGET (installed)"; then
            echo -e "${YELLOW}Installing Rust target for Android...${NC}"
            rustup target add $TARGET
        fi
        
        # Build for Android
        if [ "$BUILD_TYPE" = "release" ]; then
            cargo build --release --target $TARGET --package quest
            echo -e "${GREEN}✅ Quest build complete: target/$TARGET/release/libquest.so${NC}"
        else
            cargo build --target $TARGET --package quest
            echo -e "${GREEN}✅ Quest build complete: target/$TARGET/debug/libquest.so${NC}"
        fi
        
        echo -e "${YELLOW}Note: To create APK, use cargo-apk:${NC}"
        echo "  cargo install cargo-apk"
        echo "  cargo apk build --package quest"
        ;;
        
    web)
        echo -e "${BLUE}Building for WebAssembly...${NC}"
        
        # Check if wasm target is installed
        if ! rustup target list | grep -q "wasm32-unknown-unknown (installed)"; then
            echo -e "${YELLOW}Installing Rust target for WebAssembly...${NC}"
            rustup target add wasm32-unknown-unknown
        fi
        
        # Check if wasm-pack is installed
        if ! command -v wasm-pack &> /dev/null; then
            echo -e "${YELLOW}wasm-pack not found. Install with:${NC}"
            echo "  cargo install wasm-pack"
            exit 1
        fi
        
        # Build for Web
        cd app-hosts/web
        if [ "$BUILD_TYPE" = "release" ]; then
            wasm-pack build --release
        else
            wasm-pack build --dev
        fi
        cd ../..
        
        echo -e "${GREEN}✅ Web build complete: app-hosts/web/pkg/${NC}"
        ;;
        
    *)
        echo -e "${RED}Unknown platform: $PLATFORM${NC}"
        echo "Valid platforms: desktop, quest, web"
        exit 1
        ;;
esac

echo ""
echo -e "${GREEN}Build successful!${NC}"

# Provide run instructions
case $PLATFORM in
    desktop)
        echo ""
        echo "To run the desktop app:"
        echo "  cargo run -p desktop -- examples/spinning_cubes.xrdsl"
        ;;
    quest)
        echo ""
        echo "To deploy to Quest:"
        echo "  1. Build APK: cargo apk build --package quest"
        echo "  2. Install: adb install target/apk/xrdsl.apk"
        echo "  3. Run: adb shell am start -n com.example.xrdsl/.MainActivity"
        ;;
    web)
        echo ""
        echo "To run the web app:"
        echo "  1. cd app-hosts/web"
        echo "  2. python3 -m http.server 8080"
        echo "  3. Open http://localhost:8080"
        ;;
esac