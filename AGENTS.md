# AI Agent Guidelines

This document provides guidelines for AI agents working on the Doggy Dogs Car Dog Alarm project.

## Issue Tracking with bd (beads)

**IMPORTANT**: This project uses **bd (beads)** for ALL issue tracking. Do NOT use markdown TODOs, task lists, or other tracking methods.

### Why bd?

- Dependency-aware: Track blockers and relationships between issues
- Git-friendly: Auto-syncs to JSONL for version control
- Agent-optimized: JSON output, ready work detection, discovered-from links
- Prevents duplicate tracking systems and confusion

### Quick Start

**Check for ready work:**
```bash
bd ready --json
```

**Create new issues:**
```bash
bd create "Issue title" -t bug|feature|task -p 0-4 --json
bd create "Issue title" -p 1 --deps discovered-from:bd-123 --json
```

**Claim and update:**
```bash
bd update bd-42 --status in_progress --json
bd update bd-42 --priority 1 --json
```

**Complete work:**
```bash
bd close bd-42 --reason "Completed" --json
```

### Issue Types

- `bug` - Something broken
- `feature` - New functionality
- `task` - Work item (tests, docs, refactoring)
- `epic` - Large feature with subtasks
- `chore` - Maintenance (dependencies, tooling)

### Priorities

- `0` - Critical (security, data loss, broken builds)
- `1` - High (major features, important bugs)
- `2` - Medium (default, nice-to-have)
- `3` - Low (polish, optimization)
- `4` - Backlog (future ideas)

### Workflow for AI Agents

1. **Check ready work**: `bd ready` shows unblocked issues
2. **Claim your task**: `bd update <id> --status in_progress`
3. **Work on it**: Implement, test, document
4. **Discover new work?** Create linked issue:
   - `bd create "Found bug" -p 1 --deps discovered-from:<parent-id>`
5. **Complete**: `bd close <id> --reason "Done"`
6. **Commit together**: Always commit the `.beads/issues.jsonl` file together with the code changes so issue state stays in sync with code state

### Auto-Sync

bd automatically syncs with git:
- Exports to `.beads/issues.jsonl` after changes (5s debounce)
- Imports from JSONL when newer (e.g., after `git pull`)
- No manual export/import needed!

### MCP Server (Recommended)

If using Claude or MCP-compatible clients, install the beads MCP server:

```bash
pip install beads-mcp
```

Add to MCP config (e.g., `~/.config/claude/config.json`):
```json
{
  "beads": {
    "command": "beads-mcp",
    "args": []
  }
}
```

Then use `mcp__beads__*` functions instead of CLI commands.

### Important Rules

- ✅ Use bd for ALL task tracking
- ✅ Always use `--json` flag for programmatic use
- ✅ Link discovered work with `discovered-from` dependencies
- ✅ Check `bd ready` before asking "what should I work on?"
- ❌ Do NOT create markdown TODO lists
- ❌ Do NOT use external issue trackers
- ❌ Do NOT duplicate tracking systems

For more details, see README.md and the [beads repository](https://github.com/steveyegge/beads).

## Code Quality Workflow

**IMPORTANT**: After making any code changes, ALWAYS follow this workflow to ensure code quality:

### 1. Format Code

```bash
dart format .
```

- Automatically formats all Dart code to match project style
- Must be run before committing
- Use `dart format` (NOT `flutter format` - deprecated)

### 2. Run Static Analysis

```bash
flutter analyze
```

- Checks for code issues, warnings, and style violations
- **Fix ALL issues** before proceeding
- Zero warnings/errors required for PR approval

### 3. Run Tests

```bash
flutter test
```

- Runs all unit, widget, and integration tests
- **Fix ALL failing tests** before committing
- All tests must pass for PR approval

### 4. Verify Code Coverage

```bash
flutter test --coverage
```

Then check coverage:
```bash
# Count total lines
grep -c "^DA:" coverage/lcov.info

# Count covered lines
grep "^DA:" coverage/lcov.info | grep -cv ",0$"

# Calculate percentage
echo "scale=2; <covered> * 100 / <total>" | bc
```

**Coverage Requirements:**
- **Minimum**: 80% overall coverage
- **Critical paths**: 90% coverage (alarm, sensors, providers)
- **UI code**: 70% coverage (screens, widgets)

If coverage is below 80%, **add more tests** before committing.

### Complete Workflow Example

```bash
# 1. Make your code changes
vim lib/services/my_service.dart

# 2. Format code
dart format .

# 3. Analyze
flutter analyze
# Fix any issues, then re-run until clean

# 4. Test
flutter test
# Fix any failures, then re-run until all pass

# 5. Check coverage
flutter test --coverage
# Add tests if below 80%

# 6. Commit (include .beads/issues.jsonl if you used bd)
git add .
git commit -m "feat: add new feature"
git push
```

### Pre-Commit Checklist

Before every commit, verify:
- ✅ Code is formatted with `dart format`
- ✅ `flutter analyze` passes with zero issues
- ✅ All tests pass with `flutter test`
- ✅ Code coverage is ≥ 80%
- ✅ `.beads/issues.jsonl` is committed if you updated bd issues
