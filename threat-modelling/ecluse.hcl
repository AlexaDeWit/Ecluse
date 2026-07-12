threatmodel "Ecluse" {
  description = "Initial STRIDE threat model for Écluse, a supply-chain resilience proxy for package registries. The 'High Level' diagram captures the canonical deployment posture: per-caller passthrough credentials, the three-registry topology (first-party private store, public-derived mirror store, and a pull-through read endpoint), and AWS CodeArtifact reached over VPC endpoints, with the demand-driven mirror worker minting its own write token under the container role. Edge access control and storage-layer scanning are operator responsibilities and are out of scope (recorded as trust assumptions). This is an iterative sketch, refined collaboratively."
  author      = "Alexandra de Wit"

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

  threat "Upstream registry forges its own server-asserted metadata (e.g. a backdated publish time)" {
    description = "Écluse's freshness quarantine and integrity reasoning consume fields the upstream registry asserts — notably the per-version publish time and server-side integrity. A registry that asserted forged values (a backdated time to defeat the age quarantine, or a manufactured digest) could admit content the proxy's age- and integrity-based gates would otherwise hold back."
    impacts     = ["Tampering"]
    control "Trust Assumption" {
      description = "Risk treatment: accepted by trust assumption. The primary registries stamp these fields server-side — the publish time is not part of the publish document, so a publisher cannot forge it — and Écluse necessarily extends a floor of trust to the registry operator's honesty: it reads the registry's metadata, so a hostile operator is an adversary the model cannot counter (the same class as 'what if npm itself is malicious'). What is untrusted here are the tarball contents and author-supplied fields, which the rules engine and integrity floor do gate. The freshness quarantine's age signal does depend on the upstream's timestamp honesty, so a registry asserting a forged time could in theory defeat it; Écluse could re-anchor age to its own first observation of a version to remove that dependence, but this is deliberately not pursued at this time. The central public registries (npmjs, PyPI, and their peers) are foundational enough to modern software infrastructure that trusting their server-stamped timestamps is the only practical recourse, and a robust first-observation anchor would require durable, replica-shared state at odds with Écluse's network-broker design while only narrowing a surface that is already outside the practical treatment boundary. The possibility is acknowledged as accepted residual risk rather than marked mitigated."
      risk_reduction = 0
    }
  }

  threat "Forwarded caller credentials aggregated in proxy memory" {
    description = "Under the canonical passthrough strategy the proxy transiently holds every caller's own CodeArtifact bearer in process memory while relaying reads and publishes. A single proxy compromise (a heap/memory dump, a log-field leak, or a malicious dependency in Écluse's own supply chain) therefore harvests the credentials of all callers in transit, not one. Passthrough distributes credential exposure across every user, where a service identity would concentrate it in one short-lived token."
    impacts     = ["Information disclosure"]
    control "Redacted Types and Memory Hardening" {
      description = "Tokens are carried as a redacted type (the Secret newtype, whose Show renders a fixed placeholder) so they never reach a log field, and retention is request-scoped; the token is unwrapped only at the point of use, to attach the bearer to an outbound request. The data-plane and WAI span instrumentation records no request or response headers, so an Authorization header is never lifted onto a span (regression-tested). Residual: a GC'd runtime cannot guarantee prompt erasure from the heap, so hardening Écluse's own runtime and supply chain — the attested, reproducible image, kept clean by the image vulnerability-scan gate — is the first-class compensating control. The service strategy (post-launch) is available to operators who want the smallest credential surface."
      risk_reduction = 80
    }
  }

  threat "Chokepoint exhaustion via pathological upstream payload" {
    description = "Écluse is a mandatory chokepoint, so degrading its availability is itself a supply-chain attack: builds either fail or operators are tempted to bypass the gate. A hostile or compromised upstream, or a pathological public package, could return an oversized, version-flooded, or deeply-nested packument whose parsing and per-version rule evaluation exhaust CPU or memory."
    impacts     = ["Denial of service"]
    control "Resource Limits and Caching" {
      description = "Input is bounded fail-closed by body-size, version-count, and nesting-depth caps (PROXY_MAX_RESPONSE_BYTES / PROXY_MAX_VERSION_COUNT / PROXY_MAX_NESTING_DEPTH), the bounded read stopping mid-stream. The serve path is O(n log n) in version count (a Map-based merge, no super-linear blow-up), the single-flight cache coalesces concurrent misses for the same package onto one computation, and a per-request timeout caps any single request. The version cap is being made to fail fast at the cap rather than projecting the whole document first — a release-gated fix in progress (#400; no release ships without it). A future CVE-informed rules feature (not yet built) must likewise batch its advisory lookups rather than querying per version (#399), so the amplification is a forward design constraint on that feature, not a current cost; resident-bytes and serve-concurrency admission bounds further cap aggregate resident cost (#418, #419). The residual is resource amplification, not algorithmic complexity: a near-cap document still costs real CPU and heap, and distinct-key floods (worst under a hostile or compromised upstream) bypass single-flight, so volumetric and concurrency rate-limiting is an operator-edge responsibility, as with access control."
      risk_reduction = 90
    }
  }

  threat "Proxy compromised via tampered OSV database" {
    description = "If the S3 bucket is writable by an attacker, they could supply a tampered osv.db to bypass vulnerability gates, or worse, exploit memory corruption vulnerabilities in the underlying C SQLite engine (e.g., Magellan-style exploits, malicious triggers) when the proxy executes queries."
    impacts     = ["Tampering"]
    control "S3 ACL and SQLite Hardening" {
      description = "S3 bucket is private. Proxy IAM role is granted GetObject only. Atomic shadow-swap prevents partial reads. In addition, the proxy explicitly binds the SQLite connection to Read-Only mode and disables Trusted Schema (PRAGMA trusted_schema = OFF;) upon opening the connection to prevent execution of attacker-controlled triggers or views. Acceptance further verifies the artifact before it is served: the schema epoch stamp, a PRAGMA quick_check integrity walk (which also verifies stored values against each STRICT table's declared column types), the required tables being real STRICT tables carrying the required columns, and the ecosystem; a failing artifact is refused as a rejection value with its ETag remembered, and the last-good database keeps serving."
      risk_reduction = 95
    }
  }

  threat "Docker Hub Static Credential Leakage" {
    description = "Docker Hub does not support OIDC, forcing the CI release workflow to rely on a static, long-lived DOCKERHUB_TOKEN."
    impacts     = ["Information disclosure"]
    control "Migrate to GHCR" {
      description = "The workflow was migrated to GitHub Container Registry (GHCR), completely removing Docker Hub static secrets in favor of the ephemeral, OIDC-backed GITHUB_TOKEN."
      risk_reduction = 100
    }
  }

  threat "Registry collapse erases provenance and per-store policy" {
    description = "Écluse supports collapsing its internal registry roles onto as few as one store. The recommended topology keeps the first-party store (A) and the public-derived mirror store (B) separate and unions them into the pull-through read endpoint (C) at the registry level; collapsing them onto a single shared store is the degenerate floor — and the configuration default, since an unset MIRROR_TARGET_URL folds the mirror onto the private upstream. Collapse loses the physical separation between first-party and public-derived inventory: distinct storage-level rule-sets and scanning per provenance become impossible, and post-disclosure incident scoping ('which mirrored public packages did we hold?') is muddied, weakening the arithmetic-not-forensics response."
    impacts     = ["Repudiation"]
    control "Trust Assumption" {
      description = "Accepted by trust assumption. Operators must consciously deploy the 3-registry topology."
      risk_reduction = 0
    }
  }

  threat "Private-upstream aggregation admits the public registry, bypassing the gate" {
    description = "The recommended topology unions the trusted stores (first-party A and the sanitized mirror B) into the pull-through read endpoint C at the registry level (e.g. CodeArtifact upstream relationships). If that aggregation also includes a direct connection to the public registry, raw public packages reach clients through C as a trusted source, skipping Écluse's gate (rules, integrity floor, freshness quarantine) entirely. The same upstream-merger mechanism that makes the ideal topology work makes this the natural misconfiguration: a CodeArtifact repository's default npm-store upstream to npmjs is exactly this shape."
    impacts     = ["Tampering"]
    control "Trust Assumption" {
      description = "Accepted by trust assumption. Operators must configure their first-party stores to not silently proxy public registries behind the scenes."
      risk_reduction = 0
    }
  }

  threat "Undetected artifact substitution across upstreams" {
    description = "The merge flags an integrity divergence when private and public copies of a version contradict on a shared digest algorithm. A weak-only or absent digest, or a flaw in how divergence is keyed, could let a substituted artifact pass undetected and be served as the trusted copy."
    impacts     = ["Tampering"]
    control "Digest Algorithm Constraints" {
      description = "A public version must carry a digest meeting the integrity floor to be admitted, and divergence is compared on each digest's asserted algorithm. A real same-version contradiction on a shared algorithm is detected by the merge and consumed on the serve path."
      risk_reduction = 90
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
