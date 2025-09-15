#!/bin/bash

# Quest 3 Hot-Reload Client Script
# Sends XR-Lang code to Quest for hot-reload

set -e

# Colors
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
NC='\033[0m'

echo -e "${GREEN}XR-Lang Quest Hot-Reload Client${NC}"
echo "================================"

# Parse arguments
if [ $# -lt 1 ]; then
    # Try to auto-detect Quest IP
    if command -v adb &> /dev/null; then
        QUEST_IP=$(adb shell ip addr show wlan0 2>/dev/null | grep "inet " | awk '{print $2}' | cut -d/ -f1 || echo "")
    fi
    
    if [ -z "$QUEST_IP" ]; then
        echo -e "${RED}Usage: $0 <quest-ip> [file.xrl]${NC}"
        echo -e "${YELLOW}Example: $0 192.168.1.100 examples/quest-demo.xrl${NC}"
        exit 1
    fi
    
    echo -e "${YELLOW}Auto-detected Quest IP: $QUEST_IP${NC}"
else
    QUEST_IP="$1"
    shift
fi

# Port for hot-reload
PORT=9090

# Function to send file
send_file() {
    local file=$1
    
    if [ ! -f "$file" ]; then
        echo -e "${RED}Error: File not found: $file${NC}"
        return 1
    fi
    
    echo -e "${YELLOW}Sending $(basename $file) to $QUEST_IP:$PORT...${NC}"
    
    # Try different methods to send
    if command -v nc &> /dev/null; then
        # Using netcat
        nc -w 2 "$QUEST_IP" "$PORT" < "$file" 2>/dev/null
        RESULT=$?
    elif command -v telnet &> /dev/null; then
        # Using telnet
        (cat "$file"; sleep 1) | telnet "$QUEST_IP" "$PORT" 2>/dev/null
        RESULT=$?
    elif command -v python3 &> /dev/null; then
        # Using Python
        python3 -c "
import socket
import sys
with open('$file', 'rb') as f:
    data = f.read()
s = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
s.settimeout(2)
try:
    s.connect(('$QUEST_IP', $PORT))
    s.sendall(data)
    s.close()
    sys.exit(0)
except:
    sys.exit(1)
"
        RESULT=$?
    else
        echo -e "${RED}Error: No suitable tool found (nc, telnet, or python3)${NC}"
        return 1
    fi
    
    if [ $RESULT -eq 0 ]; then
        echo -e "${GREEN}✓ Hot-reload sent successfully!${NC}"
        return 0
    else
        echo -e "${RED}✗ Failed to connect to Quest at $QUEST_IP:$PORT${NC}"
        echo -e "${YELLOW}Make sure:${NC}"
        echo "  1. XR-Lang app is running on Quest"
        echo "  2. Quest and Mac are on same network"
        echo "  3. Port $PORT is not blocked"
        return 1
    fi
}

# If file specified, send it
if [ $# -gt 0 ]; then
    send_file "$1"
    exit $?
fi

# Interactive mode - watch directory
WATCH_DIR="${WATCH_DIR:-examples}"
echo -e "\n${YELLOW}Watching directory: $WATCH_DIR${NC}"
echo -e "${YELLOW}Modify any .xrl file to hot-reload${NC}"

# Function to find most recently modified .xrl file
find_latest_xrl() {
    find "$WATCH_DIR" -name "*.xrl" -type f -exec stat -f "%m %N" {} \; 2>/dev/null | sort -rn | head -1 | cut -d' ' -f2-
}

# Watch for changes
if command -v fswatch &> /dev/null; then
    # macOS with fswatch
    echo -e "${GREEN}Watching for changes (fswatch)...${NC}\n"
    
    fswatch -o "$WATCH_DIR" --exclude="\.git" -e ".*" -i "\\.xrl$" | while read event; do
        LATEST=$(find_latest_xrl)
        if [ -n "$LATEST" ]; then
            echo -e "\n${YELLOW}Change detected!${NC}"
            send_file "$LATEST"
            echo
        fi
    done
    
elif command -v inotifywait &> /dev/null; then
    # Linux with inotify
    echo -e "${GREEN}Watching for changes (inotify)...${NC}\n"
    
    while true; do
        FILE=$(inotifywait -r -e modify,create --format '%w%f' "$WATCH_DIR" 2>/dev/null | grep "\.xrl$" || true)
        if [ -n "$FILE" ] && [ -f "$FILE" ]; then
            echo -e "\n${YELLOW}Change detected!${NC}"
            send_file "$FILE"
            echo
        fi
    done
    
else
    # Manual mode
    echo -e "${YELLOW}File watcher not found. Manual mode.${NC}"
    echo -e "${GREEN}Commands:${NC}"
    echo "  <Enter> - Send latest .xrl file"
    echo "  q       - Quit"
    echo
    
    while true; do
        read -r -p "> " cmd
        
        if [ "$cmd" = "q" ]; then
            break
        fi
        
        LATEST=$(find_latest_xrl)
        if [ -n "$LATEST" ]; then
            send_file "$LATEST"
        else
            echo -e "${RED}No .xrl files found in $WATCH_DIR${NC}"
        fi
    done
fi