#!/usr/bin/env bash
set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(dirname "$SCRIPT_DIR")"

echo "Running daily-planner tests..."

emacs --batch \
    -L "$PROJECT_ROOT" \
    -L "$PROJECT_ROOT/test" \
    -l ert \
    -l daily-planner \
    -l test-helper \
    -l daily-planner-test \
    -f ert-run-tests-batch-and-exit

echo "All tests passed!"
