#!/bin/bash

# Setup script for Claude Code Web environment
# Installs: beads, Flutter, Android SDK and accepts licenses
# Only runs in CLAUDE_CODE_REMOTE environment

# Exit early if not in Claude Code Remote environment
if [ "$CLAUDE_CODE_REMOTE" != "true" ]; then
  exit 0
fi

set -e

echo "🔧 Claude Code Web Setup Script"
echo "================================"
echo "✓ Running in Claude Code Remote environment"

# Function to check if command exists
command_exists() {
    command -v "$1" >/dev/null 2>&1
}

# 1. Install beads (bd)
echo ""
echo "📦 Installing beads..."
if command_exists bd; then
    echo "✓ beads already installed ($(bd --version))"
else
    echo "  Downloading beads..."
    cd /tmp
    curl -L -o beads.tar.gz https://github.com/steveyegge/beads/releases/download/v0.17.7/beads_0.17.7_linux_amd64.tar.gz
    tar -xzf beads.tar.gz
    sudo mv bd /usr/local/bin/bd
    sudo chmod +x /usr/local/bin/bd
    rm beads.tar.gz
    echo "✓ beads installed ($(bd --version))"
fi

# 2. Install Flutter
echo ""
echo "📦 Installing Flutter..."
# Check if Flutter exists in PATH or in /opt/flutter
if command_exists flutter || [ -d "/opt/flutter" ]; then
    # Add to PATH if not already there
    export PATH="$PATH:/opt/flutter/bin"
    echo "✓ Flutter already installed ($(flutter --version 2>/dev/null | head -n 1 || echo 'Flutter SDK at /opt/flutter'))"
else
    echo "  Downloading Flutter SDK..."
    cd /tmp
    curl -L -o flutter.tar.xz https://storage.googleapis.com/flutter_infra_release/releases/stable/linux/flutter_linux_3.35.7-stable.tar.xz
    echo "  Extracting Flutter..."
    cd /opt
    sudo tar xf /tmp/flutter.tar.xz
    rm /tmp/flutter.tar.xz

    # Fix git ownership
    git config --global --add safe.directory /opt/flutter

    # Add to PATH for this session
    export PATH="$PATH:/opt/flutter/bin"

    # Add to shell profiles if not already present
    if ! grep -q "/opt/flutter/bin" ~/.bashrc; then
        echo 'export PATH="$PATH:/opt/flutter/bin"' >> ~/.bashrc
    fi
    if ! grep -q "/opt/flutter/bin" ~/.profile; then
        echo 'export PATH="$PATH:/opt/flutter/bin"' >> ~/.profile
    fi

    echo "✓ Flutter installed ($(flutter --version | head -n 1))"
fi

# Ensure Flutter is in PATH for this session
export PATH="$PATH:/opt/flutter/bin"

# 3. Install Android SDK
echo ""
echo "📦 Installing Android SDK..."
export ANDROID_HOME=/opt/android-sdk

if [ -d "$ANDROID_HOME/cmdline-tools/latest" ]; then
    echo "✓ Android SDK already installed"
else
    echo "  Creating Android SDK directory..."
    sudo mkdir -p $ANDROID_HOME/cmdline-tools

    echo "  Downloading Android command-line tools..."
    cd $ANDROID_HOME/cmdline-tools
    sudo curl -L -o cmdline-tools.zip https://dl.google.com/android/repository/commandlinetools-linux-11076708_latest.zip

    echo "  Extracting command-line tools..."
    sudo unzip -q cmdline-tools.zip
    sudo mv cmdline-tools latest
    sudo rm cmdline-tools.zip

    echo "✓ Android command-line tools installed"
fi

# 4. Install Android platform-tools
echo ""
echo "📦 Installing Android platform-tools..."
if [ -d "$ANDROID_HOME/platform-tools" ]; then
    echo "✓ Platform-tools already installed"
else
    echo "  Downloading platform-tools..."
    cd $ANDROID_HOME
    sudo curl -L -o platform-tools-latest-linux.zip https://dl.google.com/android/repository/platform-tools-latest-linux.zip

    echo "  Extracting platform-tools..."
    sudo unzip -q platform-tools-latest-linux.zip
    sudo rm platform-tools-latest-linux.zip

    echo "✓ Platform-tools installed"
fi

# Add Android SDK to PATH
export PATH=$PATH:$ANDROID_HOME/cmdline-tools/latest/bin:$ANDROID_HOME/platform-tools

# Add to shell profiles if not already present
if ! grep -q "ANDROID_HOME" ~/.bashrc; then
    echo 'export ANDROID_HOME=/opt/android-sdk' >> ~/.bashrc
    echo 'export PATH=$PATH:$ANDROID_HOME/cmdline-tools/latest/bin:$ANDROID_HOME/platform-tools' >> ~/.bashrc
fi

# 5. Accept Android SDK licenses
echo ""
echo "📦 Accepting Android SDK licenses..."
echo "y" | $ANDROID_HOME/cmdline-tools/latest/bin/sdkmanager --sdk_root=$ANDROID_HOME --licenses >/dev/null 2>&1 || true
echo "✓ Android SDK licenses accepted"

# 6. Configure Flutter to use Android SDK
echo ""
echo "📦 Configuring Flutter..."
flutter config --android-sdk $ANDROID_HOME >/dev/null 2>&1
echo "✓ Flutter configured to use Android SDK"

# 7. Run flutter doctor to verify setup
echo ""
echo "🏥 Running flutter doctor..."
flutter doctor

echo ""
echo "✅ Setup complete!"
echo ""
echo "📝 Summary:"
echo "  - beads (bd): $(bd --version)"
echo "  - Flutter: $(flutter --version | head -n 1)"
echo "  - Android SDK: $ANDROID_HOME"
echo ""
echo "🚀 Environment ready for development!"
