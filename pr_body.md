## Summary

Implements SQLite compilation for extracted OSV vulnerability data. Connects the existing OSV streaming pipeline to `sqlite-simple` for batch insertion into a predictable flat table schema, ensuring data is ready for fast querying by the registry data plane. Closes #525.

## In plain terms

**The situation / the risk.** When the Écluse pilot node synchronizes vulnerability advisories from the OSV database, it pulls down a giant stream of JSON data. To serve vulnerability queries quickly (e.g. "is package X version Y vulnerable?"), this data needs to be stored in an indexed format that allows fast lookups on the package name.

**What changed.** We added a step at the end of the advisory streaming pipeline that compiles the vulnerability ranges into a local SQLite database (`osv.db`). It uses a flattened table structure with a composite primary key and an index on the package name, enabling O(1)-like lookups during registry operations. We also made the output directory configurable so operators can control where this data is stored on disk.

**The trade-off.** We chose to flatten the vulnerability ranges into individual rows rather than storing complex JSON blobs in the database. This increases the total number of rows but vastly simplifies and speeds up the querying logic needed during package resolution.

## Checklist
- [x] `make check` passes locally (build, unit tests, fourmolu, hlint, Semgrep)
- [x] Docs updated in this PR where behaviour, interfaces, or config changed
- [x] Conventional Commit subjects; commits are GPG-signed
- [x] Every commit is signed off, DCO (`git commit -s`), as the author
- [x] Tests added or updated for the change

## AI assistance
- [x] Disclosed: assisted by AI; `Assisted-by:` trailer on the
      relevant commits. Author reviewed and is responsible for every line.
