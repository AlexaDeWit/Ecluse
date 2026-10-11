#!/usr/bin/env bash
# Dispatch pure contract discovery without interpreting the selected tier as shell code.
set -euo pipefail

action="${1:-}"
case "$action" in
  list|plan) ;;
  *) echo 'contract action must be list or plan' >&2; exit 2 ;;
esac
case "${ECLTEST_CONTRACT_TIER:-e2e}" in
  e2e) exec bash scripts/e2e.sh "$action" ;;
  integration) exec bash scripts/test-integration.sh "$action" ;;
  *) echo 'TIER must be e2e or integration' >&2; exit 2 ;;
esac
