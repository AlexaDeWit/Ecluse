+++
title = "Control which dependencies reach your builds."
description = "Écluse gives small and mid-size companies a shared package policy across developer machines and CI."

weight = 52

[extra]
nav_title = "Why Écluse?"
group = "Understand \u00c9cluse"
kind = "Introduction"
reference = "https://github.com/AlexaDeWit/Ecluse/blob/main/README.md"
+++

## Give new releases time. {#why}

A compromised package can reach builds soon after publication. Écluse can hold new public versions for seven days, giving the ecosystem time to discover and remove a bad release.

Your package manager uses Écluse as its registry. Écluse filters the versions it offers and checks public downloads against your policy.

Age does not prove a package is harmless. The delay reduces exposure to releases removed during that window.

## When it fits {#fit}

A central platform, DevOps, or IT team runs Écluse as a shared service for developer teams and CI across the organisation.

You control the proxy, the policy, and its deployment. Existing registries hold your private packages and any public packages you choose to mirror.

To enforce the policy, restrict package downloads to Écluse. Changing a registry setting alone does not prevent a client from using another route.

## Start small. Add what you need. {#add}

| Capability | What it gives you |
| --- | --- |
| Public package proxy | A shared minimum release age and package rules. No private registry or cloud account required. |
| Advisory checks | Known-malicious package denial and early admission for qualifying security fixes. |
| Private packages | Access to your own packages through the same address, using each caller’s registry credentials. |
| Mirroring | Copies of admitted public versions in your registry. Complete copies can survive public outages or removal. |
| Package removal | Removal of stored versions that your current revocation rules deny, with separate permission to delete. |

## What you still own {#limits}

Your platform controls who can reach Écluse and which network destinations builds can reach. Your registries control private package access.

Écluse does not scan package code for malware or host package archives. It cannot remove a package already installed or cached on a client.

{{ task_link(path="@/docs/setup.md", title="Set up Écluse", description="Choose the capabilities you need and configure a deployment.") }}

{{ task_link(path="@/docs/security.md", title="Review the security model", description="Trust assumptions, enforcement limits, and the rendered threat model.") }}

{{ task_link(path="@/docs/behaviour.md", title="Read the detailed behaviour", description="Admission, private reads, advisory failures, and revocation.") }}
