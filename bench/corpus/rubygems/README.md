# RubyGems Compact Index captures

These files contain complete original named-gem `/info/<gem>` responses from RubyGems.org.
`../pins.json` records each source, capture time, media type, byte size and SHA-256.
`rubygemsPins` in `../pins.json` selects captured workloads without cutting their histories.
Each selected version already clears the default quarantine at its authenticated capture time.
The capture tool preserves these workload choices when it replaces snapshots.
`BENCH_CORPUS_ECOSYSTEM=rubygems task gen-bench-corpus` replaces these snapshots.
`BENCH_CORPUS_VERIFY=1` authenticates the committed bytes without network access.

Rails supplies dependency-heavy release history.
Nokogiri supplies multiple platform artifacts for individual releases.
AWS SDK EC2 supplies a long sequence of releases.
The captures retain prereleases, native requirements, dependencies, checksums and publication times.
They contain no global catalogue, compact specifications or gem archives.

RubyGems.org maintains these factual registry observations.
The proxy's MIT licence does not relicense the captured metadata.
The package source licences remain separate.
The source projects are https://github.com/rails/rails,
https://github.com/sparklemotion/nokogiri and https://github.com/aws/aws-sdk-ruby.

Every capture is below one MiB.
These files do not establish a native per-MiB resource factor.
Synthetic complexity inputs remain separate from public captures.
