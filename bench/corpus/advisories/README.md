# Advisory fixtures

These files are third-party data that the benchmarks read. The MIT licence of Écluse does not cover them.

The `GHSA-*.json` records come from the GitHub Advisory Database (https://github.com/advisories) and are licensed under the Creative Commons Attribution 4.0 International licence (https://creativecommons.org/licenses/by/4.0/). Each record is also published at https://github.com/advisories/<id>.

The `PYSEC-*.json` records come from the PyPI Advisory Database of the Python Packaging Authority (https://github.com/pypa/advisory-database) and are licensed under the same Creative Commons Attribution 4.0 International licence.

Both sets were copied without modification from the osv.dev export (https://osv-vulnerabilities.storage.googleapis.com/) on 2026-09-27. They are provided as is, without warranties, as sections 5 and 6 of the licence set out.

`epss.csv` holds a subset of rows, with the header lines, from the Exploit Prediction Scoring System feed of 2026-09-27 (https://epss.empiricalsecurity.com/epss_scores-2026-09-27.csv.gz). EPSS is maintained by the EPSS Special Interest Group at FIRST (https://www.first.org/epss/), and Empirical Security generates the scores.

`cargo/RUSTSEC-2021-0124.json` is an unchanged OSV response from the RustSec advisory database, captured on 2026-10-25.
It covers Tokio and aliases CVE-2021-45710 and GHSA-fg7r-2g4j-5cgr.
RustSec dedicates its original data under CC0-1.0: https://github.com/RustSec/advisory-db/blob/main/LICENSE.txt.
The exact dated CVE-2021-45710 EPSS row comes from FIRST's public API.
Its source and capture time are recorded separately in `pins.json`, beside the unchanged feed date.

## RubyGems

The pinned RubyGems records come unchanged from the OSV API.
They describe Rails advisory ranges and exact fixed releases.
The EPSS feed also retains the official 2026-09-27 score for CVE-2024-26143.
Their source records and capture identities are in `../pins.json`.
GitHub Advisory Database records use CC-BY-4.0:
https://github.com/github/advisory-database/blob/main/LICENSE.md.
These records contain no primary malicious-package advisory.
Shared synthetic malicious fixtures remain separate from captured advisory evidence.
