+++
title = "Credentials and service permissions"
description = "Separate the caller's registry access from the credentials used by background services."

weight = 37

[extra]
nav_title = "Service permissions"
group = "Detailed behaviour"
kind = "Reference"
reference = "/docs/deployment/#role-identities-and-least-privilege"
+++

## Service permissions {#roles}

| Service | Needs |
| --- | --- |
| Proxy without a worker | Queue send and queue attributes when mirroring. Advisory reads when configured. Configured mirror-state reads and prefix listing (PyPI, Cargo, RubyGems). CodeArtifact repository descriptions for private topology checks. |
| Mirror worker | Queue receive, delete, visibility changes, and attributes. Mirror repository reads and publication. Advisory reads. Configured mirror-state reads, prefix listing, and conditional writes (PyPI, Cargo, RubyGems). |
| Advisory publisher | Advisory object read/write and prefix-scoped listing. Access to OSV and EPSS feeds. |
| Cleanup service | Inventory and metadata reads for both maintained stores. Consent inspection. Deletion only on those targets. Advisory reads and private topology inspection. |
| Caller | Read permission on the private read repository. Publication permission on the first-party repository if they publish. |

## CodeArtifact specifics {#aws}

Token minting requires `codeartifact:GetAuthorizationToken` on the domain and `sts:GetServiceBearerToken` with the CodeArtifact service condition.

Minting a token does not grant repository access. Scope repository reads and package writes separately.

The proxy and cleanup service inspect the private repository's upstream chain at startup. Grant `codeartifact:DescribeRepository` on every repository in that chain.

The cleanup full-walk cursor needs restricted tag-write permission. That permission must not allow the service to create or change its own consent marker.

## Exact actions and resource scopes {#references}

{{ task_link(path="@/docs/deployment.md#role-identities-and-least-privilege", title="Role permissions", description="AWS actions, role separation, and identity requirements.") }}

{{ task_link(path="@/docs/dredger.md#permissions", title="Cleanup permissions", description="Inventory, deletion, consent, and cursor scopes for each maintained target.") }}

{{ task_link(path="@/docs/pypi-setup.md#assign-credentials-and-roles", title="PyPI state permissions", description="Read and conditional-write access for completion state.") }}
