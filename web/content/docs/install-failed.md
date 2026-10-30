+++
title = "Investigate a failed install"
description = "Use the package manager's error and proxy logs to investigate an install reported by a developer or CI job."

weight = 32

[extra]
nav_title = "Investigate a failed install"
group = "Operate and troubleshoot"
kind = "Guide"
reference = "/docs/operations/"
+++

## Find the likely cause {#symptoms}

| What you see | Check first |
| --- | --- |
| No matching version | The minimum age, npm install scripts, npm dependencies on a git source or a remote URL, explicit denies, and file eligibility. Filtered versions do not appear in listings. |
| 403 from an artifact request | The deciding policy rule or integrity refusal in the response and logs. Also check private credentials. |
| 503 | Whether Écluse can obtain required evidence or upstream data. Check advisory state, managed mirror completion, and whether the private registry answers. A failed private read refuses every package on the mount. |
| A first-party package is missing | The private repository and declared names. First-party requests never fall back to public. |
| The package downloads from a public host | Client overrides, lockfiles, the configured public URL, and the network boundary. |
| The proxy logs no request | Client caches and registry selection before changing policy. |

## Find the request {#request}

Use the package name, exact version, package manager, and failure time from the report or CI logs to locate the request.

Check the registry address the client used. A CodeArtifact credential does not establish that the request reached Écluse.

## Check why it failed {#decision}

1. Find the corresponding request and its refusal reason.
2. Inspect the resolved policy for that mount, including its rule order.
3. Check whether the request uses the public, private, or managed mirror path.
4. If evidence is unavailable, restore the missing dependency before changing policy.

Écluse distinguishes denied requests from requests it cannot evaluate. Keep that distinction when deciding whether a retry can help.

Set `server.helpMessage` to tell callers how to contact your team.

{{ task_link(path="@/docs/policy.md", title="Understand the policy decision", description="Defaults, precedence, and integrity checks.") }}

{{ task_link(path="@/docs/exceptions.md", title="Review a package exception", description="Allow one identity without disabling the policy globally.") }}

{{ task_link(path="@/docs/advisory-behaviour.md", title="Investigate unavailable advisory evidence", description="Missing generations, stale data, and feed failures.") }}
