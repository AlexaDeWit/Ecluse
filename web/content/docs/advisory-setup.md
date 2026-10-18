+++
title = "Check security advisories"
description = "Add an advisory database to deny known-malicious public versions and admit qualifying security fixes before the age limit."

weight = 22

[extra]
nav_title = "Check security advisories"
group = "Set up"
kind = "Guide"
reference = "/docs/deployment/#running-pilot"
+++

## Configure the advisory store {#store}

Choose an S3-compatible store for the advisory databases. Give the publisher read, list, and write access. Give consumers read access.

**config.yaml addition**

```yaml
advisories:
  url: s3://your-advisory-bucket/ecluse
```

Every role must use the same intended policy. The publisher needs it to determine which advisory data each ecosystem requires.

## Provide local working space {#volume}

Mount a writable directory at `/var/lib/ecluse/advisories` for each process that reads or writes advisory databases. UID/GID `65532` must be able to write there.

On Kubernetes, an `emptyDir` is sufficient. Consumers download their database again after a restart.

## Publish before starting consumers {#publish}

The `pilot` role fetches OSV advisories and EPSS scores, compiles a database for each enabled ecosystem, and publishes it.

Run one publisher continuously, or schedule one-shot compilations without overlap. To publish an initial database, pass these arguments to the configured image:

**Container arguments: npm**

```bash
pilot compile --ecosystem npm --out /var/lib/ecluse/advisories --upload
```

**Container arguments: PyPI**

```bash
pilot compile --ecosystem pypi --out /var/lib/ecluse/advisories --upload
```

Confirm each compile succeeds before starting admission roles that need its database. Then keep publication running on your chosen schedule.

## Understand what changes {#changes}

Configuring the store activates the default known-malicious deny and remediation rule. Scored CVSS and EPSS denies remain opt-in.

With these defaults, public admission waits for an accepted database. Later, an expired database can stop public admission even for old versions.

The default remediation rule needs EPSS data. If that feed fails, the publisher retains the previous artifact instead of publishing an incomplete replacement.

Trusted private copies remain readable until eligible removal. Advisory outages do not authorise deletion.

{{ next(path="@/docs/advisory-behaviour.md", title="Understand advisory freshness and failures") }}
