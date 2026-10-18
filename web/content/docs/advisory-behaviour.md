+++
title = "Advisory freshness and failures"
description = "Missing evidence, stale evidence, and a missing score are different conditions. They do not all follow the same fallback."

weight = 21

[extra]
nav_title = "Advisory freshness and failures"
group = "Detailed behaviour"
kind = "Reference"
reference = "/docs/configuration/#when-the-epss-feed-fails"
+++

## Adding a source changes the default policy {#source}

Setting `advisories.url` activates the default known-malicious deny and remediation rule. With no source, those defaults are inactive.

With the source configured, the defaults require accepted advisory evidence before public admission. Old package versions can also be refused during an advisory outage.

## What each failure means {#failure}

| Condition | Behaviour |
| --- | --- |
| No accepted database yet | Mounts whose rules require evidence remain unready. They continue polling. |
| A replacement fails to download or validate | A running process retains its last accepted database, subject to the maximum age. |
| The accepted database expires | Advisory denies refuse even with `onUnavailable: skip`. The remediation rule follows its own unavailable-data setting. |
| An individual EPSS score is missing | The EPSS deny abstains for that advisory. Missing CVSS follows different rules and is not treated as zero. |
| The EPSS feed fails during compilation | An ecosystem with an EPSS-dependent rule receives no new artifact. Other ecosystems can receive OSV data marked as lacking EPSS enrichment. |
| The consumer restarts | It begins with an empty advisory slot and acquires an accepted generation again. |

## Which security fixes can skip the wait {#fixes}

The advisory must identify the exact fixed version. A later version is not automatically an exact fix.

If another advisory still affects the fix, the configured differential compares stored CVSS and EPSS scores against its ceilings and gaps. Missing required scores cannot establish the exception.

These defaults are product choices, not proven safety thresholds. Higher-precedence denies and independent integrity checks still apply.

## A skipped check can outlive the outage {#skip}

Where `onUnavailable: skip` applies, the rule abstains. Another rule must still allow the version.

An admitted version can then be mirrored. Its trusted reads do not repeat public admission after the advisory source recovers.

The decision and logs record skipped checks. Removing that copy later requires a decisive revocation deny and successful cleanup.

## Readiness is not a freshness monitor {#readiness}

The first successful sync satisfies the advisory readiness gate. It does not keep readiness aligned with the database’s age.

Monitor publication age, acquisition failures, and advisory outage events separately. Keep the intended policy consistent across the publisher and every consumer.
