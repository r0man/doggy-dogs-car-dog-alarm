# Scripts

This directory contains utility scripts for the Doggy Dogs Car Dog Alarm project.

## setup-claude-code-web.sh

Automated setup script for Claude Code Web environment.

### What it does

This script automatically installs and configures the complete development environment:

1. **Beads (bd)** - Issue tracker CLI tool
   - Downloads and installs beads v0.17.7
   - Installs to `/usr/local/bin/bd`

2. **Flutter SDK** - Mobile app development framework
   - Downloads Flutter 3.24.5 stable
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
flutter --version     # Should show: Flutter 3.24.5
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
