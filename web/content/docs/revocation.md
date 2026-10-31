+++
title = "Admission, revocation, and retained copies"
description = "Blocking a new public version and removing an existing private copy are separate operations."

weight = 46

[extra]
nav_title = "Admission and revocation"
group = "Detailed behaviour"
kind = "Reference"
reference = "/docs/operations/#revoking-a-mirrored-version-internal-yank"
+++

## Where a rule applies {#phases}

| Policy phase | What it controls |
| --- | --- |
| Admission | Public listings, public artifact requests, and worker re-admission before mirror writes. |
| Revocation | Whether cleanup can remove an already-stored version. |

An admission-only deny leaves stored copies readable through trusted private access. A deny applying to both phases can authorise removal when it becomes decisive.

New rules apply to both phases unless narrowed. The shipped npm install-script and external-dependency denies, and the shipped Cargo external-dependency deny, are admission-only.

## No allow does not mean delete {#retention}

Public admission denies when no rule allows the version. Cleanup keeps the stored version unless a named deny decides.

Removing an allow therefore does not itself revoke a copy. If that removal exposes a winning revocation deny, normal cleanup can apply.

Unavailable evidence never supplies deletion authority. Consent, first-party protection, backend capabilities, and deletion caps still apply.

## A mirror deletion is not the whole removal {#copies}

A private read repository can retain a cached copy after the mirror loses its source. Cleanup must remove eligible copies from both repositories.

Verify both inventories and authorised reads. An access error is not evidence that a package is absent.

Installed files and client caches remain outside registry revocation.

{{ diagram(name="revocation-cleanup", alt="Removal from the mirror and private cache, from the shared threat model.") }}

## Policy changes take time to converge {#rollout}

A worker still running the old policy can publish a version the updated proxy denies. That copy then uses the trusted private path.

When tightening policy, update admission and writer roles before cleanup. When relaxing a deny, update cleanup before writers rely on the new permission.

When adding an EPSS-dependent rule, prepare the advisory publisher first. Verify every role's intended policy and let older writes settle before concluding that removal is complete.

{{ next(path="@/docs/remove.md", title="Remove blocked packages") }}
