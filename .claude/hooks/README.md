# Claude Code Hooks

This directory contains hooks that run at specific points in the Claude Code session lifecycle.

## Available Hooks

### start-session.sh

Runs when a Claude Code session starts.

**Current behavior:**
- Executes `scripts/setup-claude-code-web.sh`
- Only runs in Claude Code Web environment (`CLAUDE_CODE_REMOTE`)
- Installs development dependencies (beads, Flutter, Android SDK)
- Idempotent: Safe to run on every session start

**When it runs:**
- Every time you start a new Claude Code session
- Automatically triggered by Claude Code

**What it does:**
1. Checks if running in Claude Code Remote environment
2. Installs missing tools (beads, Flutter, Android SDK)
3. Configures development environment
4. Runs `flutter doctor` to verify setup

## Hook Configuration

Hooks are executable shell scripts that must:
- Be located in `.claude/hooks/`
- Have executable permissions (`chmod +x`)
- Exit with status 0 on success

## Environment Variables

Hooks have access to these environment variables:

- `CLAUDE_CODE_REMOTE` - Set to "true" in Claude Code Web
- `PWD` - Current working directory (project root)
- Standard environment variables

## Customization

To add your own startup tasks:

1. Edit `.claude/hooks/start-session.sh`
2. Add your commands after the setup script call
3. Ensure script exits with status 0

Example:

```bash
#!/bin/bash

# Run standard setup
"$SCRIPT_DIR/scripts/setup-claude-code-web.sh"

# Your custom startup tasks
echo "Running custom startup tasks..."
# Add your commands here
```

## Debugging

To debug hook execution:

1. Add `set -x` at the top of the hook for verbose output
2. Check hook output in Claude Code session logs
3. Test manually: `./claude/hooks/start-session.sh`

## Documentation

For more information about Claude Code hooks, see:
- Claude Code documentation
- Project scripts/README.md for setup details
