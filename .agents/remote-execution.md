# Hosted and Web Agent Execution

Read this guide only in an ephemeral hosted or web environment with an outbound proxy and a local
git relay. A local contributor session does not need it.

## Signed commits

- Commit through local git, never a REST git-objects API. Repository rules require a verified
  signature on every branch.
- The environment normally comes preconfigured with SSH signing through `/tmp/code-sign`. Use plain
  `git commit -S -s`. An empty `~/.ssh/commit_signing_key.pub` or an empty GPG keyring does not
  prove that signing is unavailable.
- Local verification can report `No signature` when `gpg.ssh.allowedSignersFile` is absent. Check
  `git cat-file -p HEAD` for a `gpgsig -----BEGIN SSH SIGNATURE-----` header, or use GitHub's
  Verified result.

## Relay and branch behaviour

- The relay rejects a force-push and a branch deletion. Never rewrite published history.
- Fetch `origin/main` before you branch, because the initial checkout can be stale. Base new work on
  current `origin/main` and reread the affected files after syncing.
- Let `git push origin HEAD:<branch>` create a new branch. Do not pre-create it through the API: a
  stale API ref can make the later signed push non-fast-forward and undeletable from the
  environment.
- Use the branch the session designates, when it names one.

## Local Verification

Agents **must not** skip local verification (`task check`).
Run `task format`, then `task check`, inside the Nix shell:

```bash
env -u IN_NIX_SHELL nix develop --command task check
```

You **must** watch the CI results and follow up on any failure. Your task is not complete until
both local verification and CI pass.
