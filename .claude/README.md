# Claude Code Configuration

This directory contains configuration for Claude Code.

## settings.json

Contains hook configuration for automated environment setup.

### SessionStart Hook

The `SessionStart` hook runs when a Claude Code session starts, executing:
```bash
scripts/setup-claude-code-web.sh
```

This ensures the development environment is automatically configured with:
- beads (issue tracker CLI)
- Flutter SDK
- Android SDK and tools

### Hook Configuration

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

### Environment Variables

- `$CLAUDE_CODE_REMOTE` - Set to "true" in Claude Code Web environment

### How It Works

1. Claude Code session starts
2. Hook system reads `settings.json`
3. Executes the setup script with `$CLAUDE_PROJECT_DIR` resolved
4. Script checks if `CLAUDE_CODE_REMOTE=true`
5. If true, installs/verifies development dependencies
6. If false, exits immediately (skips on local environments)

### Documentation

For more information about Claude Code hooks:
- [Hooks Guide](https://docs.claude.com/en/docs/claude-code/hooks-guide.md)
- [Hooks Reference](https://docs.claude.com/en/docs/claude-code/hooks.md)
- [Claude Code on the Web](https://docs.claude.com/en/docs/claude-code/claude-code-on-the-web.md)

For details about the setup script, see `scripts/README.md`.
