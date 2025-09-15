#!/bin/bash

# Set Android environment variables for XR-Lang Quest development
export ANDROID_HOME=$HOME/Library/Android/sdk
export ANDROID_NDK_HOME=$ANDROID_HOME/ndk/29.0.14033849
export PATH=$PATH:$ANDROID_HOME/platform-tools:$ANDROID_HOME/tools

echo "Android environment configured:"
echo "  ANDROID_HOME=$ANDROID_HOME"
echo "  ANDROID_NDK_HOME=$ANDROID_NDK_HOME"
echo ""
echo "To make this permanent, add these lines to your ~/.zshrc:"
echo ""
echo "export ANDROID_HOME=\$HOME/Library/Android/sdk"
echo "export ANDROID_NDK_HOME=\$ANDROID_HOME/ndk/29.0.14033849"
echo "export PATH=\$PATH:\$ANDROID_HOME/platform-tools:\$ANDROID_HOME/tools"