#!/usr/bin/env bash
set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(dirname "$SCRIPT_DIR")"

# CLI smoke test that outputs dashboard content for agent inspection
# This allows AI agents to verify the package works without a GUI

echo "=== Daily Planner CLI Test ==="
echo "Running with mock data to verify rendering..."
echo ""

emacs --batch \
    -L "$PROJECT_ROOT" \
    -L "$PROJECT_ROOT/test" \
    -l daily-planner \
    -l test-helper \
    --eval '
(progn
  ;; Use mock data
  (cl-letf (((symbol-function (quote daily-planner-github--fetch))
             (lambda () daily-planner-test--mock-github-data))
            ((symbol-function (quote daily-planner-linear--fetch))
             (lambda () daily-planner-test--mock-linear-data))
            ((symbol-function (quote daily-planner-hn--fetch))
             (lambda () daily-planner-test--mock-hn-stories)))
    ;; Render dashboard
    (let ((buf (get-buffer-create "*Daily Planner*")))
      (with-current-buffer buf
        (daily-planner-mode)
        (daily-planner-refresh)
        ;; Output buffer contents
        (princ (buffer-string))))))
'

echo ""
echo "=== CLI Test Complete ==="
echo "If you see the dashboard content above with GitHub, Linear, and HN sections,"
echo "the package is working correctly."
