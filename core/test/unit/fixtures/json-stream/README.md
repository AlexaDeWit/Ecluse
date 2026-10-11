# C lexer reference

`c-lexer-events.json` records the C-backed `Data.JsonStream.CLexer` at Écluse commit
`53698eac7d10b32dc5be42eff04ae784a5a4c77a`, using GHC 9.10.3 on x86_64 Linux.
The vendored C lexer is unchanged from the targeted main implementation.

The 2,914 cases include every three-piece cut of the short directed documents, seventeen fixed
chunk sizes, and every single-byte value. Empty chunks remain in the input. Cases cover malformed
numbers, numeric overflow, incomplete literals, escaped and split strings, invalid UTF-8,
lenient separators, and trailing bytes.

Events preserve token order, values, ASCII flags, waits, failures, and leftover contexts. Byte
payloads use their length and SHA-256 digest. The native scanner did not generate these expectations.
The fixture assumes a 64-bit C long, as both supported Linux CI architectures provide.

# Decoder reference

`unescape-oracle.json` records the digest and provenance of 68,577 outcomes from the inherited
decoder before the ST port. `Data.JsonStream.UnescapeSpec` reproduces the input sequence and hashes
canonical output records, including the exact error constructor, description and offending byte.
The reference accepted 18,299 cases, rejected 50,278, and threw no exceptions.

The cases cover every one-byte and two-byte input, every lone surrogate escape, UTF-8 boundaries,
truncations, mutations, sentinel-backed slices and long strings. Auxiliary Aeson and text results
on the directed cases remain part of the frozen digest and depend on the recorded library versions.
The test calls the public lexer interface and does not include the old decoder implementation.
