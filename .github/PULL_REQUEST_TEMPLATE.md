<!--
Keep it short. The "why" matters more than a play-by-play of the diff.
For a security fix, coordinate privately first. See SECURITY.md.
-->

## Summary

<!--
What changed and why, in two to five sentences, for a reader who has not opened the diff.
Lead with what a reviewer or operator gains or is protected from. For a security or behaviour
change, say who could do what before and what holds now. No play-by-play of files: the diff
already lists them. Closes #123

CONTRIBUTING.md -> "Pull requests" has the rules and the same change written verbose and concise.
-->

## Checklist

- [ ] `task check` passes locally (build, unit tests, fourmolu, hlint, Semgrep, weeder, stan)
- [ ] Docs updated in this PR (README / `docs/` / AGENTS.md) where behaviour, interfaces, or config changed
- [ ] Conventional Commit subjects, and every commit GPG-signed
- [ ] Every commit signed off under the DCO (`git commit -s`)
- [ ] Tests added or updated for the change
- [ ] Haddock meets the `docs/haddock.md` checklist (§12): the why rather than the what, one or two
      lines per export, no restated signatures, no project or PR narration

## Sign-off (DCO)

<!--
See CONTRIBUTING.md → "Developer Certificate of Origin (DCO)".
-->

## AI assistance

<!--
See CONTRIBUTING.md → "AI-assisted contributions". Disclose non-trivial AI use.
Editor autocomplete is exempt. You remain the author and are responsible for every line.
-->

- [ ] No AI assistance beyond editor autocomplete, or disclosed below and marked with an `Assisted-by:` trailer on the relevant commits.

<!-- If AI-assisted: which tool(s), and how you verified the output. -->
