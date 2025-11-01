#!/usr/bin/env bash
# Flutter run script with Guix environment

set -e

# Run flutter in guix shell with proper library paths
exec guix shell -m manifest.scm -- bash -c \
  'export LD_LIBRARY_PATH="$GUIX_ENVIRONMENT/lib:$LD_LIBRARY_PATH" && ~/flutter/bin/flutter run -d linux "$@"' -- "$@"
