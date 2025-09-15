#!/bin/bash

# XR-Lang Quest 3 Hot-Reload Client
# Watches for file changes and pushes updates to Quest device

set -e

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

echo -e "${GREEN}XR-Lang Quest Hot-Reload Client${NC}"
echo "================================="

# Default Quest IP (can be overridden with argument)
QUEST_IP="${1:-}"

# Function to find Quest IP automatically via ADB
find_quest_ip() {
    if command -v adb &> /dev/null && adb devices | grep -q "device$"; then
        # Try to get IP from Quest via ADB
        IP=$(adb shell ip addr show wlan0 2>/dev/null | grep 'inet ' | awk '{print $2}' | cut -d/ -f1)
        if [ ! -z "$IP" ]; then
            echo "$IP"
            return 0
        fi
    fi
    return 1
}

# If no IP provided, try to find it
if [ -z "$QUEST_IP" ]; then
    echo -e "${YELLOW}Searching for Quest device...${NC}"
    
    if QUEST_IP=$(find_quest_ip); then
        echo -e "${GREEN}✓ Found Quest at: $QUEST_IP${NC}"
    else
        echo -e "${RED}Error: Could not find Quest IP automatically.${NC}"
        echo -e "${YELLOW}Please provide Quest IP as argument:${NC}"
        echo "  ./quest-hot-reload.sh 192.168.1.XXX"
        echo ""
        echo "To find your Quest IP:"
        echo "  1. On Quest: Settings → Wi-Fi → Click connected network"
        echo "  2. Or via ADB: adb shell ip addr show wlan0"
        exit 1
    fi
fi

# Port for hot-reload server (must match Quest app)
PORT=9090

# Check if Quest is reachable
echo -e "\n${YELLOW}Testing connection to Quest at $QUEST_IP:$PORT...${NC}"
if nc -z -w2 "$QUEST_IP" "$PORT" 2>/dev/null; then
    echo -e "${GREEN}✓ Quest is reachable${NC}"
else
    echo -e "${RED}Warning: Cannot reach Quest at $QUEST_IP:$PORT${NC}"
    echo "Make sure the XR-Lang app is running on your Quest"
fi

# Function to compile and send image to Quest
send_update() {
    local file="$1"
    echo -e "\n${BLUE}[$(date '+%H:%M:%S')] File changed: $file${NC}"
    
    # Compile XR-Lang image
    echo -e "${YELLOW}Compiling XR-Lang image...${NC}"
    
    # Build the image using the VM's image builder
    cargo run -p vm --bin build-image -- \
        --input examples/ \
        --output target/xr-lang.img \
        --platform quest \
        2>/dev/null
    
    if [ $? -eq 0 ]; then
        echo -e "${GREEN}✓ Image compiled successfully${NC}"
        
        # Send to Quest via TCP
        echo -e "${YELLOW}Sending to Quest...${NC}"
        
        if nc "$QUEST_IP" "$PORT" < target/xr-lang.img 2>/dev/null; then
            echo -e "${GREEN}✓ Hot-reload sent successfully!${NC}"
        else
            echo -e "${RED}Error: Failed to send to Quest${NC}"
        fi
    else
        echo -e "${RED}Error: Compilation failed${NC}"
    fi
}

# Watch for changes in examples directory
echo -e "\n${GREEN}Watching for changes in examples/*.xrl ...${NC}"
echo -e "${YELLOW}Press Ctrl+C to stop${NC}\n"

# Use fswatch if available (macOS), otherwise fall back to simple loop
if command -v fswatch &> /dev/null; then
    # macOS with fswatch
    fswatch -o examples/*.xrl | while read num; do
        send_update "examples/*.xrl"
    done
else
    # Fallback: simple polling loop
    echo -e "${YELLOW}Note: Install fswatch for better performance: brew install fswatch${NC}\n"
    
    # Store initial checksums
    declare -A checksums
    for file in examples/*.xrl; do
        if [ -f "$file" ]; then
            checksums["$file"]=$(shasum "$file" | cut -d' ' -f1)
        fi
    done
    
    # Poll for changes
    while true; do
        for file in examples/*.xrl; do
            if [ -f "$file" ]; then
                current=$(shasum "$file" | cut -d' ' -f1)
                if [ "${checksums[$file]}" != "$current" ]; then
                    checksums["$file"]="$current"
                    send_update "$file"
                fi
            fi
        done
        sleep 1
    done
fi