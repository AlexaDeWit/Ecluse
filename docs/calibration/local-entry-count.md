# Shared local cache entry allowance

The computed count uses a 16 KiB allowance per shared local entry. This matches the
base charge for a present selected release, before its fields and compact metadata.
It avoids early count eviction across the measured positive-entry controls below.

The bound remains `clamp(256, 65536, aggregateBytes / 16384)` with integer division.
An explicit `cache.maxEntries` overrides it exactly. Both eligible stores share this
count and the existing byte bound. Full metadata remains ineligible for local retention.
The production compact byte expansion factor remains 7.5.

## Evidence and limits

[The capture manifest](local-entry-count.json) records all twelve measured charge pairs,
selected versions, source capture digests and byte sizes. The measurement source is
`868818d671fa30ea93957feb4a6b20a878d14d13`. The pooled storage contract comes from
`e8e05f87576b223ade375f45f45baf1142217c22`.

Three cold-selected, three retained-selected and three one-origin listing rows per
package passed source-digest and byte-size checks against the capture catalogue.
Selected charges agree across those six selected rows. Assembled charges are strict
output sizes plus 256 bytes. The manifest preserves each source capture's path, digest and byte size. Raw probe rows
and process measurements remain run artefacts.

The twelve selected charges range from 21,163 to 97,404 bytes. The twelve assembled
charges range from 81,173 to 10,347,438 bytes. Together these 24 entries charge
27,450,791 bytes. Their observed cardinality does not exercise the 256-entry floor.

The capacity study repeats these charges under hypothetical distinct keys. It covers
homogeneous samples, equal-name selected and assembled cycles, selected/assembled ratios
of 1:1, 9:1 and 99:1, and a 90:9:1 selected/absence/assembled cycle.
Each mixed control uses twelve starting phases. Ratios are sensitivity controls,
not observed production frequencies. An untagged absence charges 1024 bytes.

The study tests 64 MiB, 256 MiB and 1 GiB byte budgets. At all three, 16 KiB avoids
count pressure before byte pressure in every positive-only control. A 32 KiB allowance
count-limits five homogeneous selected samples. The smallest uses only 64.584% of its
byte budget. Selecting the exact 21,163-byte minimum would tie policy to one sample.

At 256 MiB:

| Allowance | Entry bound | Equal-name selected fill | 99:1 selected/assembled fill | Absence-only fill |
|---|---:|---:|---:|---:|
| 16 KiB | 16,384 | 99.972%-99.998% | 96.659%-99.997% | 6.25% |
| Previous 256 KiB | 1,024 | 16.835%-16.877% | 21.466%-26.635% | 0.390625% |
| Rejected 3 MiB | 256 | 4.192%-4.233% | 4.261%-9.433% | 0.09765625% |

The separate 64 KiB diagnostic fills 256 MiB with 4096 selected entries. The previous
allowance evicts at 1024 entries, leaving 192 MiB unused. This shape is a synthetic
control, not a captured corpus sample. The provider regression uses the same shape
at 64 MiB and also checks mixed-store bytes, absences, and explicit count pressure.

Without an explicit override, cheap-entry cardinality rises sixteen-fold at these
three budgets. At 256 MiB, the new count still limits bare absences to 16,384, versus
262,144 under a byte-only bound. The 65,536-entry maximum stays unchanged.
The count limits retained keys, recency and expiry indexes, and their maintenance work.
No measurement assigns an exact heap cost to these structures. Tiny assembled responses
can also reach count pressure early. This allowance does not guarantee heap fit.

## Repeat the measurements

The existing [material probe](../testing.md#material-admission-calibration) accepts
`--metadata-material-probe ECOSYSTEM MODE NAME VERSION 50331648 PATH +RTS -T -N1 -RTS`.
Use the manifest's ecosystem, package, selected version and capture path. Prefix capture
paths with `bench/corpus/`. Run `ColdSelected`, `RetainedSelected` and `ListingOneOrigin`
three times each through a scratch Taskfile in the Nix shell, as that guide describes.
The probe streams 32768-byte chunks. Label any new measurements with their actual
source revision. They do not replace the historical row digests in this manifest.

## Reproduce the capacity arithmetic

Run this from the repository root with Python 3. It consumes the recorded measured
charges without fetching live registry data or depending on a scratch directory.
The calculation stops before the first eviction. It does not model TTL, LRU, throughput,
cross-store eviction, or process memory.

```python
import json
from itertools import accumulate
from bisect import bisect_right
from pathlib import Path

samples = json.loads(Path("docs/calibration/local-entry-count.json").read_text())["samples"]
selected = [s["selected_charge"] for s in samples]
assembled = [s["assembled_charge"] for s in samples]


def capacity(weights, budget, divisor):
    prefix = [0, *accumulate(weights)]
    cycles, remainder = divmod(budget, prefix[-1])
    byte_count = cycles * len(weights) + bisect_right(prefix, remainder) - 1
    count_bound = min(65536, max(256, budget // divisor))
    held = min(byte_count, count_bound)
    cycles, remainder = divmod(held, len(weights))
    charge = cycles * prefix[-1] + prefix[remainder]
    return count_bound < byte_count, 100 * charge / budget


def cycle(selected_count, absence_count, assembled_count, phase):
    weights = []
    selected_index = assembled_index = phase
    for _ in range(12):
        for _ in range(selected_count):
            weights.append(selected[selected_index % 12])
            selected_index += 1
        weights.extend([1024] * absence_count)
        for _ in range(assembled_count):
            weights.append(assembled[assembled_index % 12])
            assembled_index += 1
    return weights


for budget in [64 * 2**20, 256 * 2**20, 1024 * 2**20]:
    for divisor in [16384, 32768, 262144, 3 * 2**20]:
        for counts in [(1, 0, 0), (0, 0, 1), (1, 0, 1), (9, 0, 1),
                       (99, 0, 1), (90, 9, 1), (0, 1, 0)]:
            results = [capacity(cycle(*counts, phase), budget, divisor)
                       for phase in range(12)]
            print(budget, divisor, counts, sum(r[0] for r in results),
                  min(r[1] for r in results), max(r[1] for r in results))
        for charge in selected + assembled:
            print(budget, divisor, charge, capacity([charge], budget, divisor))
```
