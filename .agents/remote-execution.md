# Hosted and Web Agent Execution

Read this guide only in ephemeral hosted/web environments with an outbound proxy and local git
relay. Local contributor sessions do not need it.

## Signed commits

- Commit through local git, never a REST git-objects API. Repository rules require verified
  signatures on every branch.
- The environment is normally preconfigured with SSH signing through `/tmp/code-sign`. Use normal
  `git commit -S -s`; an empty `~/.ssh/commit_signing_key.pub` or empty GPG keyring does not prove
  signing is unavailable.
- Local verification can report `No signature` when `gpg.ssh.allowedSignersFile` is absent. Check
  `git cat-file -p HEAD` for a `gpgsig -----BEGIN SSH SIGNATURE-----` header, or use GitHub's
  Verified result.

## Relay and branch behaviour

- The relay rejects force-pushes and branch deletion. Never rewrite published history.
- Fetch `origin/main` before branching because the initial checkout can be stale. Base new work on
  current `origin/main` and reread affected files after syncing.
- Let `git push origin HEAD:<branch>` create a new branch. Do not pre-create it via API: a stale API
  ref can make the later signed push non-fast-forward and undeletable from the environment.
- Use the session's designated branch when one is provided.

## Local Verification

Agents **must not** skip local verification (`make check`).
Run `make check` inside the Nix shell:

```bash
env -u IN_NIX_SHELL nix develop --command make check
```

You **must** monitor CI results and follow up on any failures. Your task is not complete until both local verification and CI pass.
