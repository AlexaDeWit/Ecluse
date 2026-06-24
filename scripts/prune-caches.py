#!/usr/bin/env python3
"""Select GitHub Actions cache ids to delete — for .github/workflows/cache-cleanup.yml.

Reads cache rows on stdin, one TSV row per cache:

    id<TAB>ref<TAB>key<TAB>created_at

(as produced by `gh api .../actions/caches --jq '... | @tsv'`), and writes the ids to
delete to stdout, one per line. The workflow feeds those ids to `gh api -X DELETE`.

Retention policy (matches the workflow's documented intent):

  * ref != refs/heads/main          -> delete. PRs restore caches but never save (see
                                       the setup-toolchain action), so any non-main
                                       entry is a straggler — e.g. left by a deleted
                                       branch.
  * on main, beyond the newest N     -> delete. Once a dependency epoch advances
    per key prefix                     (flake.lock / cabal.project[.freeze] change),
                                       the previous epoch's immutable-keyed entries are
                                       dead weight.

Prefix = the key with a trailing `-<16+ hex>` (a hashFiles digest) stripped, so every
epoch of one logical cache groups together. N defaults to 2 (current + one fallback for
in-flight runs / quick rollback); override via KEEP_PER_PREFIX.

Logic lives here rather than inline in YAML so it is reviewable, lintable, and runnable
outside CI. Try it against a sample:

    printf 'id\\tref\\tkey\\tcreated\\n' | KEEP_PER_PREFIX=2 python3 scripts/prune-caches.py
"""

import collections
import os
import re
import sys

# A trailing hashFiles digest: a dash followed by 16+ hex chars at the end of the key.
_DIGEST_SUFFIX = re.compile(r"-[0-9a-f]{16,}$")


def select_for_deletion(rows, keep):
    """Yield the cache ids to delete from an iterable of TSV row strings.

    Off-main rows are yielded as they are seen; on-main rows are grouped by key prefix
    and the newest `keep` per group are retained, the rest yielded.
    """
    groups = collections.defaultdict(list)  # prefix -> [(created_at, id)]
    for line in rows:
        line = line.rstrip("\n")
        if not line:
            continue
        cid, ref, key, created = (line.split("\t") + ["", "", "", ""])[:4]
        if ref != "refs/heads/main":
            yield cid  # off-main straggler
            continue
        prefix = _DIGEST_SUFFIX.sub("", key)
        groups[prefix].append((created, cid))

    for items in groups.values():
        items.sort(reverse=True)  # newest created_at first
        for _created, cid in items[keep:]:
            yield cid  # superseded epoch on main


def main():
    keep = int(os.environ.get("KEEP_PER_PREFIX", "2"))
    for cid in select_for_deletion(sys.stdin, keep):
        print(cid)


if __name__ == "__main__":
    main()
