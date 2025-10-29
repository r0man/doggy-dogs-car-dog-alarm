#!/bin/bash

# Claude Code Start Session Hook
# Runs setup script for Claude Code Web environment

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"

# Run setup script
"$SCRIPT_DIR/scripts/setup-claude-code-web.sh"
