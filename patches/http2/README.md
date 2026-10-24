# http2 decoded header bounds

Both build paths use http2 5.3.10 at upstream commit
`fe92e9c1b9cc4e774ddc45962cd71b340496bf9b` and `decoded-header-bounds.patch`.
The patch retains the existing receiver and timer behaviour.

The backport derives from upstream pull requests
[216](https://github.com/kazu-yamamoto/http2/pull/216) and
[217](https://github.com/kazu-yamamoto/http2/pull/217), through commit
`e9785abaccc7b8d7415501290ecfc24bdee8f661`.
It also takes the decoder drain from `cf63327d0b42fb1081c250b729eed10447b8780d`
and the empty-block guard from `ef92ac6d101461596f37ec76a358f6229db9614a`.
The upstream authors retain their copyright under BSD-3-Clause.
The upstream licence follows in `LICENSE`.

Nix applies the tracked patch with zero fuzz.
Cabal pins the same source and calls `ecluse-apply-http2-patch` from the Nix shell.
The literal digest in `cabal.project` invalidates Cabal's source cache when the patch changes.
The hook verifies the source revision and exact pristine or patched contents.
It accepts an exact patched checkout when Cabal reruns the hook offline.
The `http2-lockstep` check compares the source revision and patch digest in both build paths.

Run builds through the mandatory Nix shell.
A Cabal cache hit does not rerun the checkout hook.
The shell retains the immutable patch in its closure.
When changing the patch, update its digest in `cabal.project` in the same commit.
