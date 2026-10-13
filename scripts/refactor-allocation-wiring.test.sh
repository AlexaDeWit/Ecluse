#!/usr/bin/env bash
# SPDX-FileCopyrightText: 2026 Alexandra de Wit
# SPDX-License-Identifier: MIT
set -euo pipefail
repository="$(git rev-parse --show-toplevel)"
bash "$repository/scripts/refactor-diagnostic-wiring.sh" focused
