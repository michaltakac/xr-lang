#!/bin/bash

# Quest 3 Deployment Script
# Deploys XR-Lang APK to connected Quest device

set -e

# Colors
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
RED='\033[0;31m'
NC='\033[0m'

echo -e "${GREEN}XR-Lang Quest 3 Deployer${NC}"
echo "========================"

# Check for adb
if ! command -v adb &> /dev/null; then
    echo -e "${RED}Error: adb not found. Please install Android SDK tools.${NC}"
    exit 1
fi

# Check for APK
APK_PATH="target/debug/apk/xrdsl.apk"
if [ ! -f "$APK_PATH" ]; then
    echo -e "${RED}Error: APK not found at $APK_PATH${NC}"
    echo -e "${YELLOW}Run './scripts/quest-build.sh' first${NC}"
    exit 1
fi

# Check for Quest device
echo -e "\n${YELLOW}Checking for Quest device...${NC}"
if ! adb devices | grep -q "device$"; then
    echo -e "${RED}Error: No Quest device found.${NC}"
    echo -e "${YELLOW}Please:${NC}"
    echo "  1. Enable Developer Mode on your Quest"
    echo "  2. Connect via USB-C cable"
    echo "  3. Accept the RSA key on the headset"
    exit 1
fi

DEVICE_INFO=$(adb shell getprop ro.product.model 2>/dev/null || echo "Unknown")
echo -e "${GREEN}✓ Found device: $DEVICE_INFO${NC}"

# Get Quest IP for wireless debugging
QUEST_IP=$(adb shell ip addr show wlan0 2>/dev/null | grep "inet " | awk '{print $2}' | cut -d/ -f1 || echo "")
if [ -n "$QUEST_IP" ]; then
    echo -e "${GREEN}✓ Quest IP: $QUEST_IP${NC}"
    echo -e "${YELLOW}Hot-reload will be available at: $QUEST_IP:9090${NC}"
fi

# Uninstall old version
echo -e "\n${YELLOW}Uninstalling old version...${NC}"
adb uninstall com.example.xrdsl 2>/dev/null || true

# Install APK
echo -e "${YELLOW}Installing APK ($(du -h $APK_PATH | cut -f1))...${NC}"
adb install -r "$APK_PATH"

if [ $? -eq 0 ]; then
    echo -e "${GREEN}✓ APK installed successfully!${NC}"
    
    # Launch the app
    echo -e "\n${YELLOW}Launching XR-Lang on Quest...${NC}"
    adb shell am start -n com.example.xrdsl/android.app.NativeActivity
    
    echo -e "\n${GREEN}========================================${NC}"
    echo -e "${GREEN}XR-Lang is now running on your Quest 3!${NC}"
    echo -e "${GREEN}========================================${NC}"
    
    if [ -n "$QUEST_IP" ]; then
        echo -e "\n${YELLOW}To hot-reload code:${NC}"
        echo -e "  ./scripts/quest-hot-reload.sh $QUEST_IP <file.xrl>"
    fi
    
    echo -e "\n${YELLOW}To view logs:${NC}"
    echo -e "  adb logcat -s XR-Lang:V rust:V"
    
    # Ask if user wants to see logs
    echo -e "\n${YELLOW}View logs now? (y/n)${NC}"
    read -n 1 -r
    echo
    if [[ $REPLY =~ ^[Yy]$ ]]; then
        echo -e "${YELLOW}Showing logs (Ctrl+C to stop)...${NC}"
        adb logcat -c  # Clear old logs
        adb logcat -s XR-Lang:V rust:V OpenXR:V
    fi
else
    echo -e "${RED}Error: Failed to install APK${NC}"
    exit 1
fi