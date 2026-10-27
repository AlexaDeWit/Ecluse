# Owned RubyGems fixtures

These synthetic gems belong to the repository. They contain no third-party package source.
The pinned Nix Ruby toolchain creates the archives, compact specifications and dependency replies.

Regenerate with `nix develop --command ruby test/fixtures/rubygems/generate.rb`.
`manifest.json` records the Ruby and RubyGems versions, coordinates, checksums and independently decoded specification fields.
The generator fixes the build epoch to 2020-01-01.

The root gem depends on the leaf gem and supplies a Ruby module and an executable.
Its development dependency has two requirement clauses.
The platform gem has Ruby, Java, Linux musl and Darwin artifacts for the same raw version.
The native gem contains an extension declaration. The future gem requires Ruby 99.
Compact specifications omit extension declarations even when the archive declares one.

`reference.rb` uses RubyGems, Marshal and zlib to observe controlled fixture or product-response bytes in tests.
It is an independent test oracle, never a production decoder.
Modes are `dependencies`, `compact`, `archive` and `info`. Each mode reads standard input and emits JSON.

For load setup, `--output DIRECTORY --payload-bytes N` writes a separate native registry.
Each platform artifact then contains `lib/payload.bin` with exactly N deterministic bytes.
Generate these archives before timing. Authenticate them against that output's manifest.
The default invocation leaves the committed archive bytes unchanged.

`legacy/` contains compact specifications with RubyGems' declared historical field counts.
`legacy.json` records the current Ruby decoder's observations for each layout.
`aliases.yaml` and `aliases.json` record a Psych-generated alias and its independent decoded value.
