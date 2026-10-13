# SPDX-FileCopyrightText: 2026 Alexandra de Wit
# SPDX-License-Identifier: MIT
"""One child per wrapper keeps RUSAGE_CHILDREN peak RSS local to one sample."""
import json
import pathlib
import resource
import subprocess
import sys
import time

output = pathlib.Path(sys.argv[1])
command = sys.argv[2:]
start_ns = time.monotonic_ns()
status = 127
try:
    status = subprocess.call(command)
finally:
    end_ns = time.monotonic_ns()
    elapsed = (end_ns - start_ns) / 1_000_000_000
    usage = resource.getrusage(resource.RUSAGE_CHILDREN)
    output.parent.mkdir(parents=True, exist_ok=True)
    output.write_text(json.dumps({
        "command": command,
        "startedNs": start_ns,
        "endedNs": end_ns,
        "exitCode": status,
        "wallSeconds": elapsed,
        "userSeconds": usage.ru_utime,
        "systemSeconds": usage.ru_stime,
        "peakRssKiB": usage.ru_maxrss,
    }) + "\n", encoding="utf-8")
sys.exit(status if status >= 0 else 128 - status)
