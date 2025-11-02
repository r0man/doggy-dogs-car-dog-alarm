# Scripts

This directory contains utility scripts for the Doggy Dogs Car Dog Alarm project.

## install-claude-code-web

Installation script for Spritely development environment (Guile → Goblins → Hoot).

### What it does

This script automatically installs the complete Spritely stack for building the Doggy Dogs Dog World game:

1. **GNU Guile** - Scheme interpreter and development environment
   - Installs Guile 3.0+ (required for Goblins and Hoot)
   - Supports multiple installation methods (Guix, apt, Homebrew, source)

2. **Spritely Goblins** - Distributed object programming library
   - Actor-based programming model
   - Required for building distributed game features
   - Repository: https://codeberg.org/spritely/goblins

3. **Spritely Hoot** - Scheme to WebAssembly compiler
   - Compiles Scheme code to WASM
   - Enables running Scheme in web browsers
   - Includes `guild compile-wasm` command
   - Repository: https://codeberg.org/spritely/hoot

### When to use

Run this script to set up the Spritely development environment for working on Doggy Dogs Dog World features:

```bash
./scripts/install-claude-code-web
```

### Features

- **Idempotent**: Safe to run multiple times - skips already installed components
- **Multi-platform**: Supports Linux and macOS
- **Multiple installation methods**:
  - Linux: Guix, apt, dnf, yum, pacman, zypper, or from source
  - macOS: Homebrew, Guix, or from source
- **Automatic verification**: Tests each component after installation
- **Clear progress messages**: Shows what's being installed and why

### Requirements

**Linux:**
- Any of: Guix, apt, dnf, yum, pacman, or zypper
- Build tools (if building from source): git, autoconf, automake, pkg-config, make, gcc
- sudo access (for system-wide installation)

**macOS:**
- Homebrew (recommended) or Guix
- Xcode Command Line Tools (for building from source)

### Installation methods

The script tries installation methods in order of preference:

**Guile:**
1. Guix package manager
2. System package manager (apt/dnf/yum/pacman/zypper/brew)
3. Build from source (GNU FTP)

**Goblins:**
1. Guix package manager
2. Homebrew (macOS only, via aconchillo/guile tap)
3. Build from source (Codeberg)

**Hoot:**
1. Guix package manager
2. Homebrew (macOS only, via aconchillo/guile tap)
3. Build from source (Codeberg)

### Verification

After installation completes, verify with these commands:

```bash
# Test Guile
guile --version

# Test Goblins
guile -c '(use-modules (goblins))'

# Test Hoot
guild compile-wasm --version

# Compile example Scheme to WASM
echo '42' > /tmp/test.scm
guild compile-wasm --run /tmp/test.scm
```

### Troubleshooting

**Guile not found after installation:**
- Restart your shell: `exec bash`
- Or source your profile: `source ~/.bashrc` (Linux) or `source ~/.profile` (macOS)

**Goblins module not loading:**
- Check that Guile is version 3.0+: `guile --version`
- Ensure guile-fibers, guile-gnutls, and guile-websocket are installed
- Try: `guix install guile-fibers guile-gnutls guile-websocket`

**guild compile-wasm not found:**
- Check PATH includes `/usr/local/bin`: `echo $PATH`
- Try: `export PATH="$PATH:/usr/local/bin"`
- Verify Hoot installation: `find /usr/local -name "*hoot*"`

### Related

This script is separate from `setup-claude-code-web.sh`, which installs Flutter, Android SDK, and beads for the mobile app development.

---

## setup-claude-code-web.sh

Automated setup script for Claude Code Web environment (Flutter/Android).

### What it does

This script automatically installs and configures the complete development environment:

1. **Beads (bd)** - Issue tracker CLI tool
   - Downloads and installs beads v0.17.7
   - Installs to `/usr/local/bin/bd`

2. **Flutter SDK** - Mobile app development framework
   - Downloads Flutter 3.35.0 stable
   - Installs to `/opt/flutter`
   - Adds to PATH in `.bashrc` and `.profile`

3. **Android SDK** - Android development tools
   - Downloads Android command-line tools
   - Installs to `/opt/android-sdk`
   - Includes platform-tools (adb, fastboot)
   - Automatically accepts all SDK licenses

4. **Flutter Configuration**
   - Configures Flutter to use Android SDK
   - Runs `flutter doctor` to verify setup

### When it runs

- **Automatically**: When starting a Claude Code Web session (via `.claude/settings.json` SessionStart hook)
- **Manually**: Run `./scripts/setup-claude-code-web.sh` from project root
- **Environment**: Only runs when `CLAUDE_CODE_REMOTE=true`

### Features

- **Idempotent**: Safe to run multiple times - skips already installed components
- **Fast**: Only installs what's missing
- **Automated**: No manual intervention required
- **Verified**: Runs `flutter doctor` to confirm everything works

### Requirements

- Ubuntu/Debian Linux
- sudo access
- Internet connection
- `CLAUDE_CODE_REMOTE=true` (automatically set in Claude Code Web)

### Hook Configuration

The script is automatically triggered by the SessionStart hook defined in `.claude/settings.json`:

```json
{
  "hooks": {
    "SessionStart": [
      {
        "matcher": "startup",
        "hooks": [
          {
            "type": "command",
            "command": "./scripts/setup-claude-code-web.sh"
          }
        ]
      }
    ]
  }
}
```

**Note:** The hook runs from the project root directory, so the path is relative to where `.claude/settings.json` is located.

### Manual Installation

If you need to run the setup manually:

```bash
# From project root
./scripts/setup-claude-code-web.sh
```

### Verification

After setup completes, verify installation:

```bash
bd --version          # Should show: bd version 0.17.7
flutter --version     # Should show: Flutter 3.35.0
flutter doctor        # Should show Flutter and Android toolchain ready
```

### Troubleshooting

**Script doesn't run:**
- Verify `.claude/settings.json` exists and is properly formatted
- Check script has execute permissions: `chmod +x scripts/setup-claude-code-web.sh`
- Ensure hook configuration points to correct script path

**Flutter not in PATH:**
- Restart your shell session: `exec bash`
- Or source your profile: `source ~/.bashrc`

**Android SDK issues:**
- Verify `/opt/android-sdk` directory exists
- Check licenses: `sdkmanager --licenses`

### Files Modified

The script modifies these files:
- `~/.bashrc` - Adds Flutter and Android SDK to PATH
- `~/.profile` - Adds Flutter to PATH
- `~/.gitconfig` - Adds safe directory for Flutter git repo

### Installed Locations

- Beads: `/usr/local/bin/bd`
- Flutter: `/opt/flutter`
- Android SDK: `/opt/android-sdk`
