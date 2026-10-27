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

For state probes, generate a separate synthetic registry with `--output DIRECTORY --state-versions N`.
N ranges from 1 to 256. Versions run from `1.0.0` to `1.0.(N-1)` for all four platform artifacts.
These outputs are generated stress data, not public captures. Their manifest records `synthetic_stress`.
The ordinary manifest, info rows, archives and compact paths keep the same format.

Use `--state-versions 128` for inventory growth with ordinary specifications.
Use `--state-versions 1 --metadata-bytes 1048576` for large external compact objects.
`--metadata-bytes` sets the exact description length in each genuine native specification.
The description uses deterministic pseudorandom hexadecimal text. RubyGems metadata hash values have native length limits.
The description limit is 2097152 bytes per artifact and 16777216 bytes across all generated artifacts.
The generator verifies each native archive and compares its decoded specification and compact description.
Generate before timing, then authenticate all selected bytes against the generated manifest.

`coordinate-ambiguity/` contains the genuine `acme-1` version `1.0` source gem and compact specification.
Regenerate it with `nix develop --command ruby test/fixtures/rubygems/generate.rb --output test/fixtures/rubygems/coordinate-ambiguity --coordinate acme-1,1.0,ruby`.
Its separate manifest records the selected coordinate and the native decoder observations.
The filename also admits an `acme` candidate, so routed tests check first-party absence without public fallback.
