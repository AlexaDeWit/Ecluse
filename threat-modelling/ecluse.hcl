threatmodel "Ecluse" {
  description = "Initial STRIDE threat model for Écluse, a supply-chain resilience proxy for package registries. The 'High Level' diagram captures the canonical deployment posture: per-caller passthrough credentials, the three-registry topology (first-party private store, public-derived mirror store, and a pull-through read endpoint), and AWS CodeArtifact reached over VPC endpoints, with the demand-driven mirror worker minting its own write token under the container role. Edge access control and storage-layer scanning are operator responsibilities and are out of scope (recorded as trust assumptions). This is an iterative sketch, refined collaboratively."
  author = "Alexandra de Wit"

  information_asset "npm client (developer / CI)" {
    description = "The caller. Presents its own CodeArtifact bearer token; authenticated to the proxy by the operator's access edge, not by Écluse."
    information_classification = "Public"
  }
  information_asset "Public npm registry" {
    description = "The public upstream. Its tarball bytes and author-supplied manifest fields are untrusted — gated by the rules engine and the integrity floor, never served blindly. The fields the registry itself asserts are trusted only to the floor that the registry is an honest operator: the per-version publish time is server-stamped (npm/PyPI/RubyGems record it server-side and it is absent from the publish document, so a publisher cannot backdate it), as is server-side integrity. A compromised or malicious registry operator forging those is below this necessary trust floor and out of scope. Queried anonymously (caller credential stripped)."
    information_classification = "Public"
  }
  information_asset "AWS IMDS + STS" {
    description = "Instance-metadata + token endpoint. Source of the container-role credentials the worker exchanges for a CodeArtifact write token. Reached over amazonka's own client, off the guarded data-plane manager."
    information_classification = "Confidential"
  }
  information_asset "Écluse proxy" {
    description = "The request pipeline: router, parallel upstream fetch, packument merge, deny-by-default rules + integrity gate, and serve. Relays publishes. Holds forwarded caller credentials transiently. Polls S3 for OSV db updates and performs atomic shadow-swap for CVE boundaries."
    information_classification = "Confidential"
  }
  information_asset "Mirror worker" {
    description = "Demand-driven async worker: polls the mirror queue, back-fills the approved artifact from the public upstream, verifies bytes against dist.integrity, and publishes to the mirror store with Écluse's own minted write token."
    information_classification = "Confidential"
  }

  threat "Forwarded caller credentials aggregated in proxy memory" {
    description = "Under the canonical passthrough strategy the proxy transiently holds every caller's own CodeArtifact bearer in process memory while relaying reads and publishes. A single proxy compromise (a heap/memory dump, a log-field leak, or a malicious dependency in Écluse's own supply chain) therefore harvests the credentials of all callers in transit, not one. Passthrough distributes credential exposure across every user, where a service identity would concentrate it in one short-lived token."
    impacts     = ["Information disclosure"]
    control "Mitigated" {
      description = "Tokens are carried as a redacted type (the Secret newtype, whose Show renders a fixed placeholder) so they never reach a log field, and retention is request-scoped; the token is unwrapped only at the point of use, to attach the bearer to an outbound request. The data-plane and WAI span instrumentation records no request or response headers, so an Authorization header is never lifted onto a span (regression-tested). Residual: a GC'd runtime cannot guarantee prompt erasure from the heap, so hardening Écluse's own runtime and supply chain — the attested, reproducible image, kept clean by the image vulnerability-scan gate — is the first-class compensating control. The service strategy (post-launch) is available to operators who want the smallest credential surface."
    }
  }

  threat "Chokepoint exhaustion via pathological upstream payload" {
    description = "Écluse is a mandatory chokepoint, so degrading its availability is itself a supply-chain attack: builds either fail or operators are tempted to bypass the gate. A hostile or compromised upstream, or a pathological public package, could return an oversized, version-flooded, or deeply-nested packument whose parsing and per-version rule evaluation exhaust CPU or memory."
    impacts     = ["Denial of service"]
    control "Mitigated" {
      description = "Input is bounded fail-closed by body-size, version-count, and nesting-depth caps (PROXY_MAX_RESPONSE_BYTES / PROXY_MAX_VERSION_COUNT / PROXY_MAX_NESTING_DEPTH), the bounded read stopping mid-stream. The serve path is O(n log n) in version count (a Map-based merge, no super-linear blow-up), the single-flight cache coalesces concurrent misses for the same package onto one computation, and a per-request timeout caps any single request. The version cap is being made to fail fast at the cap rather than projecting the whole document first — a release-gated fix in progress (#400; no release ships without it). A future CVE-informed rules feature (not yet built) must likewise batch its advisory lookups rather than querying per version (#399), so the amplification is a forward design constraint on that feature, not a current cost; resident-bytes and serve-concurrency admission bounds further cap aggregate resident cost (#418, #419). The residual is resource amplification, not algorithmic complexity: a near-cap document still costs real CPU and heap, and distinct-key floods (worst under a hostile or compromised upstream) bypass single-flight, so volumetric and concurrency rate-limiting is an operator-edge responsibility, as with access control."
    }
  }

  threat "Off-by-default edge auth assumes a sound network boundary" {
    description = "PROXY_AUTH_TOKEN is off by default: Écluse delegates 'who may reach the proxy' to the operator's access edge (gateway / mesh / network policy). If that boundary fails, notably east-west (a compromised neighbour reaching the pod directly, bypassing an ingress-only IP allow-list), an unauthenticated caller can drive the proxy."
    impacts     = ["Spoofing"]
    control "Mitigated" {
      description = "Compensating control: under passthrough the request carries only the caller's own forwarded token and the read path never substitutes a standing credential, so no forwarded token means no private read — a breach of the edge exposes only the public-gated view plus the untrusted-egress and DoS surface, never private packages. The one exception is the publish path: a configured static publication-target credential (PUBLICATION_TARGET_TOKEN) is used as a fallback for a tokenless publish, so 'no token, no publish' holds only for pure passthrough. The internal-credential publish mode is therefore fail-closed by construction: a configured publication-target token requires a verifiable inbound edge (PROXY_AUTH_TOKEN or stronger), so the composition root refuses internal-credential-plus-open-edge at boot (landed in #424, closing #420), making that state unrepresentable — the same principle the trusted-edge read identity follows. Operators must restrict both north-south and east-west access (the Golden Path documents this); any edge mode that substitutes Écluse's own identity, read or write, must require a verifiable edge (mTLS / shared secret), never a bare spoofable header."
    }
  }

  threat "Caller credential leak to the public upstream" {
    description = "The public upstream is attacker-influenceable and must never receive a caller's credential. A failure to strip the caller token on the public fetch, or following a cross-host 3xx with the bearer attached, would disclose a live CodeArtifact token to public npm or an attacker-chosen redirect target, over the unguarded private manager."
    impacts     = ["Information disclosure"]
    control "Mitigated" {
      description = "The caller credential is stripped before every public fetch (queried anonymously). Credential-bearing requests never follow redirects: redirectCount=0 is set structurally at withToken, the single bearer-attach point (the in-use http-client does not drop Authorization on a cross-host redirect)."
    }
  }

  threat "SSRF via crafted identifier or upstream-declared dist.tarball" {
    description = "Outbound URLs are built from client-supplied package identifiers and upstream-declared artifact locations. A traversal / encoded-slash / absolute-URL name, or a dist.tarball pointing at an internal or attacker-chosen host, could steer a fetch to an unintended target (cloud metadata, the private network)."
    impacts     = ["Elevation of privilege"]
    control "Mitigated" {
      description = "Identifier canonicalisation with encode-on-build; an outbound host:port allow-list (the load-bearing control) enforced where the request URL is built. Registry egress is https-only by construction: every outbound registry URL is built through a typed boundary (mkRegistryUrl) that rejects any non-https scheme, a non-https configured endpoint fails closed at boot, and TLS certificate validation authenticates the dialled host, so a name steered to an internal or rebound address cannot present a CA-trusted certificate for the requested host and the resolve-to-internal / DNS-rebinding SSRF class is closed by certificate validation rather than a resolved-IP recheck. No data-plane request follows an upstream redirect (redirectCount=0 is pinned universally at withToken, landed in #431), so there is no redirect hop that could escape the build-time allow-list or downgrade the scheme. A disallow-by-default same-authority (host and port) dist.tarball policy applies; a legacy http dist.tarball is upgraded to https on the same host, or dropped and recorded on a foreign host. The trusted private origin is held to the same https requirement. A cheap pure literal internal-range block remains as defence-in-depth on the dist.tarball host gate, and an operator can extend that fixed range set with ECLUSE_ADDITIONAL_BLOCKED_RANGES for internal space the module cannot know about in advance (widen-only, fails closed at boot on a malformed entry; #178)."
    }
  }

  threat "Package shadowing via first-party publish" {
    description = "A publish is relayed to the private store with the publisher's own token, and the packument merge serves private versions as trusted, winning collisions over public. A compromised-CI or insider publisher who clears (or slips past) the publish-scope check could publish a name that the merge then serves as a trusted version over the public package, a dependency-confusion path through Écluse's own trust model."
    impacts     = ["Tampering"]
    control "Mitigated" {
      description = "The PUBLISH_SCOPES allow-list refuses any name outside the operator's scopes before any upstream write (the anti-shadowing guard); the scope match is exact on the parsed namespace, so a prefix such as @acme-evil does not satisfy an @acme allow-list. Soundness requires the authorised identity to be the written identity: the publish document's own declared name and _id (and per-version names) are validated equal to the scope-guarded URL-path name before the relay (the body-name agreement leg landed in #425, closing #391), and the write URL is built from that same canonical name, so the guarded name, the written name, and the merge collision key are one identity by construction. Residual: shadowing within an allow-listed scope and allow-precedence choices remain the operator's risk (least-privilege the publisher's target credential)."
    }
  }

  threat "Mirror-write credential is a standing privilege over the trusted store" {
    description = "The mirror worker holds Écluse's only standing self-minted credential, with write access to the mirror store (Registry B) that feeds the trusted read path. A worker compromise, or any bypass of the admission gate, could write attacker-chosen bytes into the trusted store, poisoning future reads."
    impacts     = ["Elevation of privilege"]
    control "Mitigated" {
      description = "Containment of this standing privilege is least-privilege IAM on the container/task role (write to Registry B only; CodeArtifact tokens bear the role's own permissions, so this is an IAM policy rather than a token-level scope), container-role minting over static credentials, and a minimal TTL (CodeArtifact caps at 12h); the publish runs with redirectCount=0 (the mint token is attached at withToken, the single bearer-attach point). The mirror queue is part of the same trust boundary and is isolated and managed at the infrastructure level: a job is unauthenticated data that directs the worker to fetch-and-publish, so queue-send access is equivalent to trusted-write access — scope the queue's IAM so only the serve role enqueues (SendMessage) and only the worker receives and acks. Message authenticity is deliberately controlled by access, not signatures, the standard pattern for an internal single-producer/single-consumer queue. The worker's own attack surface is small: it hashes the fetched bytes and forwards them unchanged (no decompression or tarball parsing), so a malicious artifact is a poor code-execution vector. The dist.integrity check is anti-tamper-in-transit and anti-downgrade (it fails closed when the strongest present digest is in an uncomputable algorithm, never downgrading to a forgeable weaker one): it proves the bytes match the upstream's asserted digest, so it catches back-fill corruption but not a hostile upstream or a worker compromise. The poisoning-of-future-reads consequence is therefore bounded by admission-gate soundness (the trusted store is only as clean as what the gate admits, and only the gate may enqueue), the role's blast radius, and queue access control — not by the integrity check."
    }
  }

  threat "SSRF to the instance-metadata credential endpoint" {
    description = "Container-role token minting must reach the instance-metadata endpoint (169.254.169.254 / STS). An SSRF that reached metadata could mint the worker's CodeArtifact credential."
    impacts     = ["Elevation of privilege"]
    control "Mitigated" {
      description = "Écluse only follows internal-resolving locations on the trusted private origin, never on a client- or upstream-influenced target, so the data plane cannot be steered at metadata; minting uses amazonka's own client off the guarded data-plane manager. Operator defence-in-depth: require IMDSv2 and set the hop limit to 1; do not block metadata outright."
    }
  }

  threat "Cross-client disclosure of a private package via shared cache (#115)" {
    description = "A cache key carries no credential dimension, so were a private-origin document cached, one caller could warm an entry and another, differently-authorised caller could be served it without their own request being authorised upstream."
    impacts     = ["Information disclosure"]
    control "Mitigated" {
      description = "The private origin is never entered into the shared cache, under any strategy, made unrepresentable by construction; only the anonymous public-gated origin is cached, and the private origin is re-consulted per request (with the caller's own token under passthrough, or Écluse's own workload identity under service). The cache-recovering designs that would have shared a private entry (delegated-cache, memoised) are rejected by design, so no shared private cache exists to leak."
    }
  }

  threat "Registry collapse erases provenance and per-store policy" {
    description = "Écluse supports collapsing its internal registry roles onto as few as one store. The recommended topology keeps the first-party store (A) and the public-derived mirror store (B) separate and unions them into the pull-through read endpoint (C) at the registry level; collapsing them onto a single shared store is the degenerate floor — and the configuration default, since an unset MIRROR_TARGET_URL folds the mirror onto the private upstream. Collapse loses the physical separation between first-party and public-derived inventory: distinct storage-level rule-sets and scanning per provenance become impossible, and post-disclosure incident scoping ('which mirrored public packages did we hold?') is muddied, weakening the arithmetic-not-forensics response."
    impacts     = ["Repudiation"]
    control "Mitigated" {
      description = "Deploy the recommended three-registry topology (the Golden Path): a first-party store, a public-derived mirror store, and a pull-through aggregator read endpoint unioning the two at the registry level, each independently governable. The single-registry collapse remains supported but is discouraged; the trade it makes is auditability and defence-in-depth, not the perimeter. Operators who deliberately choose a collapsed topology accept that local residual risk according to their threat tolerance."
    }
  }

  threat "Undetected artifact substitution across upstreams" {
    description = "The merge flags an integrity divergence when private and public copies of a version contradict on a shared digest algorithm. A weak-only or absent digest, or a flaw in how divergence is keyed, could let a substituted artifact pass undetected and be served as the trusted copy."
    impacts     = ["Tampering"]
    control "Mitigated" {
      description = "A public version must carry a digest meeting the integrity floor (uniform default SHA-256, hard-floored) to be admitted, and divergence is compared on each digest's asserted algorithm rather than a bucketed tag (the asserted-algorithm keying landed in #380, closing #376). A real same-version contradiction on a shared algorithm is detected by the merge and consumed on the serve path: it is logged at WARNING (naming the package, the contradicting versions, and their digests) and metered as ecluse.registry.merge.divergence, so a substitution is surfaced, never silently reconciled. The trusted copy always wins the served bytes; the operator's ECLUSE_DIVERGENCE_POLICY then decides whether the contested version is additionally withheld from the listing (fail-closed) or served with the alarm (warn, the default). Residual: warn detects without withholding, so an operator wanting prevention rather than detection must enable fail-closed; and the deliberately operator-loosenable trusted-floor path trades strictness for availability, the remaining way a weak digest could be accepted."
    }
  }

  threat "Upstream registry forges its own server-asserted metadata (e.g. a backdated publish time)" {
    description = "Écluse's freshness quarantine and integrity reasoning consume fields the upstream registry asserts — notably the per-version publish time and server-side integrity. A registry that asserted forged values (a backdated time to defeat the age quarantine, or a manufactured digest) could admit content the proxy's age- and integrity-based gates would otherwise hold back."
    impacts     = ["Tampering"]
    control "Accepted" {
      description = "Risk treatment: accepted by trust assumption. The primary registries stamp these fields server-side — the publish time is not part of the publish document, so a publisher cannot forge it — and Écluse necessarily extends a floor of trust to the registry operator's honesty: it reads the registry's metadata, so a hostile operator is an adversary the model cannot counter (the same class as 'what if npm itself is malicious'). What is untrusted here are the tarball contents and author-supplied fields, which the rules engine and integrity floor do gate. The freshness quarantine's age signal does depend on the upstream's timestamp honesty, so a registry asserting a forged time could in theory defeat it; Écluse could re-anchor age to its own first observation of a version to remove that dependence, but this is deliberately not pursued at this time. The central public registries (npmjs, PyPI, and their peers) are foundational enough to modern software infrastructure that trusting their server-stamped timestamps is the only practical recourse, and a robust first-observation anchor would require durable, replica-shared state at odds with Écluse's network-broker design while only narrowing a surface that is already outside the practical treatment boundary. The possibility is acknowledged as accepted residual risk rather than marked mitigated."
    }
  }

  threat "Malicious mirrored version persists and is served as trusted (no automatic post-ingestion revocation)" {
    description = "Écluse mirrors approved public versions into Registry B and, by design, resists upstream yanks so a benign yank does not break installs. The flip side: a version later found malicious persists in B and is served as trusted (the merge serves the private origin unfiltered by the rules), with no automatic removal — neither an upstream yank nor a rules change reaches an already-mirrored artifact."
    impacts     = ["Tampering"]
    control "Mitigated" {
      description = "The freshness quarantine (`AllowIfOlderThan`) is the primary defence — it delays serving a new version until advisories have had time to surface, so most malicious versions are denied at admission before they are ever mirrored; this threat is the residual for a version found bad after it cleared the quarantine and was mirrored. Detection is delegated (operator scanning of Registry B, plus upstream advisories and security-holding signals, decide what to revoke). Enforcement is layered across the version's lifecycle: the hard deny-by-identity rule (DenyByIdentity, shipped via #499, closing #470) halts re-admission on the serve path and re-mirroring at the worker ingest re-check (#414, shipped) — the immediate, surgical stop that also breaks the re-mirror treadmill; an automated reaper (the Écluse Dredger, #478 — a separate zero-ingress service sharing the core rules engine) continually prunes already-mirrored versions matching advisory or age conditions, automating recovery once an alert is public; and the operator can purge a version from Registry B directly (rules never run on trusted content, so a purge — manual or by the reaper — is what removes the already-mirrored copy). Ordered deny-then-purge so demand does not re-mirror during the purge; purge alone is a treadmill while the version is still live upstream. The typical pattern is the inverse — an upstream yank or security-hold removes or changes the bytes first, after which re-mirroring cannot reproduce them and a purge clears the stale copy. Irreducible residual: a malicious version with no public advisory cannot be reaped (there is nothing to detect on) — the bound the freshness quarantine exists to provide."
    }
  }

  threat "SSRF via the worker back-fill fetch (a blind sink)" {
    description = "The mirror worker fetches the approved artifact from an upstream-declared dist.tarball location to replicate it. Like the serve-path public fetch this is untrusted egress to an attacker-influenceable target, so in principle it carries the same SSRF surface — a dist.tarball steered at an internal or cloud-metadata address."
    impacts     = ["Elevation of privilege"]
    control "Mitigated" {
      description = "The fetch runs on the same validating-TLS data-plane manager as the serve path (https-only egress with certificate validation authenticating the host, and the universal no-redirect invariant landed in #431), over an https-only dist.tarball, and the location host was admitted by the outbound allow-list at serve time before the job was enqueued. Decisively, it is a blind sink: the bytes are verified against dist.integrity and published, never returned to a caller, and an internal or metadata response cannot present a CA-trusted certificate for the host nor match the asserted digest, so the job fails closed (dropped) rather than exfiltrating. Its impact is well below the serve-path fetch."
    }
  }

  threat "Dredger inappropriately purges valid packages" {
    description = "A misconfiguration in Dredger or poisoned OSV data could cause it to delete legitimate, needed packages from Registry B, causing cache misses or upstream fetching failures."
    impacts     = ["Denial of service"]
    control "Mitigated" {
      description = "Dredger only deletes from mirror; on next request, proxy can re-mirror if it passes admission. It behaves as a cache eviction."
    }
  }

  threat "Private-upstream aggregation admits the public registry, bypassing the gate" {
    description = "The recommended topology unions the trusted stores (first-party A and the sanitized mirror B) into the pull-through read endpoint C at the registry level (e.g. CodeArtifact upstream relationships). If that aggregation also includes a direct connection to the public registry, raw public packages reach clients through C as a trusted source, skipping Écluse's gate (rules, integrity floor, freshness quarantine) entirely. The same upstream-merger mechanism that makes the ideal topology work makes this the natural misconfiguration: a CodeArtifact repository's default npm-store upstream to npmjs is exactly this shape."
    impacts     = ["Tampering"]
    control "Mitigated" {
      description = "An operator-architecture invariant, documented in the registry model and the Golden Path: the aggregating private upstream composes trusted stores only (first-party plus Écluse's sanitized mirror) and never carries a direct public upstream — public content enters only through Écluse's gate. Écluse cannot detect a violation (the private upstream is trusted by construction, and its upstream wiring is external to the proxy), so the control is operator discipline plus this documented invariant rather than a structural check."
    }
  }

  threat "Dredger container-role privilege escalation" {
    description = "Dredger has standing high privilege (delete-only) to Registry B. If compromised, it could wipe the entire registry."
    impacts     = ["Elevation of privilege"]
    control "Open" {
      description = "Zero network ingress; least-privilege IAM (delete-only, scoped to Registry B); prefers container-role minting over static secrets."
    }
  }

  threat "Connect-time reachability/timing oracle for an attacker-controlled allowlisted DNS" {
    description = "An attacker who controls the DNS for an allowlisted host can repoint it at internal addresses. https-only egress with certificate validation means the TLS handshake fails (an internal address cannot present a CA-trusted certificate for the requested host), so no request is sent and no data is exfiltrated; but the success or failure and the timing of the TCP connect and TLS handshake is a coarse internal-reachability or port-scan oracle."
    impacts     = ["Information disclosure"]
    control "Accepted" {
      description = "Risk treatment: accepted residual. No data crosses the boundary (the connection is refused at the TLS handshake before any request body is sent), the surface is limited to allowlisted hosts whose DNS the attacker already controls, and the signal is coarse (connect and handshake timing only). The host allowlist bounds which names can be aimed inward at all, but does not remove the residual timing signal."
    }
  }

  threat "Poisoned OSV payload exploits parser" {
    description = "A maliciously crafted or unexpectedly massive OSV payload from upstream could cause Pilot to exhaust memory or crash during JSON parsing."
    impacts     = ["Denial of service"]
    control "Mitigated" {
      description = "Pilot is decoupled from the proxy. If it OOMs or fails, it only delays updates; the proxy continues serving traffic using the last-known-good osv.db snapshot."
    }
  }

  threat "First-party data loss from collapsed registries" {
    description = "If the mirror target and publication target are collapsed onto a single registry, Dredger could purge first-party packages thinking they are stale or vulnerable public ones."
    impacts     = ["Denial of service"]
    control "Open" {
      description = "Dredger refuses to boot when MIRROR_TARGET_URL == PUBLICATION_TARGET_URL. Collapsing registries intentionally surrenders Dredger's automated pruning capabilities."
    }
  }

  threat "Pilot container-role privilege escalation" {
    description = "If Pilot is compromised, its standing container credentials could be leveraged."
    impacts     = ["Elevation of privilege"]
    control "Mitigated" {
      description = "Least-privilege IAM restricted to s3:PutObject for the specific bucket prefix. Runs in a segregated container separate from the proxy."
    }
  }

  threat "Proxy compromised via tampered OSV database" {
    description = "If the S3 bucket is writable by an attacker, they could supply a tampered osv.db to bypass vulnerability gates, or worse, exploit memory corruption vulnerabilities in the underlying C SQLite engine (e.g., Magellan-style exploits, malicious triggers) when the proxy executes queries."
    impacts     = ["Tampering"]
    control "Mitigated" {
      description = "S3 bucket is private. Proxy IAM role is granted GetObject only. Atomic shadow-swap prevents partial reads. In addition, the proxy explicitly binds the SQLite connection to Read-Only mode and disables Trusted Schema (PRAGMA trusted_schema = OFF;) upon opening the connection to prevent execution of attacker-controlled triggers or views. Acceptance further verifies the artifact before it is served: the schema epoch stamp, a PRAGMA quick_check integrity walk (which also verifies stored values against each STRICT table's declared column types), the required tables being real STRICT tables carrying the required columns, and the ecosystem; a failing artifact is refused as a rejection value with its ETag remembered, and the last-good database keeps serving."
    }
  }

  threat "Docker Hub Static Credential Leakage" {
    description = "Docker Hub does not support OIDC, forcing the CI release workflow to rely on a static, long-lived DOCKERHUB_TOKEN."
    impacts     = ["Information disclosure"]
    control "Mitigated" {
      description = "The workflow was migrated to GitHub Container Registry (GHCR), completely removing Docker Hub static secrets in favor of the ephemeral, OIDC-backed GITHUB_TOKEN."
    }
  }

  threat "Pathological OSV Payload (DoS)" {
    description = "OSV.dev or a compromised upstream could serve an overly large, deeply nested, or malformed JSON file, consuming excessive CPU/memory and crashing the Pilot."
    impacts     = ["Denial of service"]
    control "Mitigated" {
      description = "Pilot streams the archive and bounds each advisory as it is unzipped: an entry past a per-advisory byte cap (8 MiB) is drained to its boundary and dropped before it reaches the decoder, and an entry whose JSON does not decode is dropped, both logged and tallied, so a few poisoned records never halt ingestion. An advisory that fans out into an anomalous number of ranges is logged but still ingested. Deep nesting is bounded implicitly: the per-entry byte cap holds decode cost to a constant multiple of the input, and Pilot runs under the boot-resolved process heap ceiling (ECLUSE_MAX_HEAP_BYTES, else cgroup memory.max), so a small-but-deep payload fails as a bounded, clean process exit rather than exhausting the machine. A systemic drop rate (a mostly-unusable feed, the shape of a compromised or truncated export) aborts the compile without publishing, so the proxy keeps its last-good osv.db instead of adopting a hole-ridden one. Residual: an isolated depth bomb is a bounded Pilot crash rather than a per-record soft drop, and volumetric abuse of the fetch itself stays an operator-edge concern."
    }
  }

  threat "Massive Purge DoS" {
    description = "A bug in Dredger or malicious rule configuration could trigger thousands of deletion requests at once, exhausting registry API limits and effectively DoSing the private mirror."
    impacts     = ["Denial of service"]
    control "Open" {
      description = "Awaiting implementation. Dredger's deletion logic must be explicitly batched and rate-limited."
    }
  }

  threat "Mirror-write credential can be sent to a misconfigured registry target" {
    description = "The mirror target URL and the mirror-write credential provider are configured separately. If an operator explicitly supplies a CodeArtifact credential identity while pointing ECLUSE_MOUNTS__NPM__MIRROR_TARGET at a different registry, the worker can attach a CodeArtifact bearer to that wrong endpoint. The default inferred CodeArtifact path fails closed for a non-CodeArtifact mirror host, but the explicit MIRROR_CODE_ARTIFACT_* path can still bind a minted token to an unrelated target."
    impacts     = ["Information disclosure"]
    control "Open" {
      description = "Open correctness work is tracked in #808: decide the intended provider/endpoint binding invariant and add either a boot-time guard or an explicit operator override for deliberate cross-binding. Until then, the operational control is to keep the mirror target and credential provider identity aligned, prefer deriving CodeArtifact identity from the mirror-target host when using CodeArtifact, and scope the container role write-only to the intended mirror store. Residual impact is primarily credential disclosure to the configured wrong endpoint; public registries should reject the bearer, but an attacker-controlled endpoint could log it."
    }
  }

  threat "Oracle Blackout / Supply Chain DoS via OSV.dev compromise" {
    description = "If an attacker gains control of osv.dev and pushes malicious vulnerability records (or combines a malicious package with an OSV compromise), they can trigger false positives or fast-lane malicious remediation packages. Écluse explicitly trusts the OSV database as the oracle of truth."
    impacts     = ["Spoofing"]
    control "Accepted" {
      description = "Risk treatment: accepted by trust assumption. A compromised security oracle is a foundational supply-chain compromise. Pilot relies on OSV as a source of vulnerability truth; if the oracle is hostile, the defensive mechanism is inherently defeated. Transport, parsing, validation, and last-good-database controls can mitigate tampering in transit, malformed payloads, and update outages, but they cannot make a hostile source of truth trustworthy."
    }
  }

  threat "Accidental permanent deletion of registry data" {
    description = "Dredger issues permanent hard deletions to the mirror registry. If misconfigured or pointed at the wrong registry, it will permanently destroy data."
    impacts     = ["Elevation of privilege"]
    control "Open" {
      description = "Dredger must verify explicit operator consent before executing any destructive actions. It queries the target CodeArtifact repository for a specific resource tag (e.g., `Dredger: PermanentDeletionAllowed`). If absent, Dredger fails-closed. Additionally, it refuses to boot if MIRROR_TARGET == PUBLICATION_TARGET."
    }
  }

  mermaid "High Level" {
    content = <<-EOF
      graph TD;
        A["npm client"] -->|"npm install"| B["Écluse proxy"];
        B -->|"fetch upstream"| C["Public npm registry"];
        B -->|"queue background mirror"| D["Mirror worker"];
        D -->|"download tarball"| C;
        D -->|"get token"| E["AWS IMDS + STS"];
    EOF
  }
}