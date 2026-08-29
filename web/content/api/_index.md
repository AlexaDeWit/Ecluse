+++
title = "API reference"
description = "Écluse API reference: Haddock for the ecluse-core capability core, the ecluse-runtime effectful edge, and the ecluse composition shell."
+++

Écluse is built as three libraries: a pure capability core, an effectful runtime edge, and a
thin composition shell. Each publishes its own Haddock, generated from `main`.

<div class="cards">
  <a class="card" href="ecluse/ecluse-core/index.html">
    <h2><code>ecluse-core</code></h2>
    <p>The capability core: the registry protocol, the rules engine, the queue and credential
    handles, version and integrity, and the request pipeline. It names no backend and reads no
    environment.</p>
  </a>
  <a class="card" href="ecluse/ecluse-runtime/index.html">
    <h2><code>ecluse-runtime</code></h2>
    <p>The effectful edge: the OpenTelemetry SDK and OTLP export, the warp server binding, the
    log scribes, and the cloud adapters (SQS, CodeArtifact, and S3) that implement the core's
    handles.</p>
  </a>
  <a class="card" href="ecluse/index.html">
    <h2><code>ecluse</code></h2>
    <p>The composition shell: the <code>run</code> entry point, configuration loading and
    resolution, the composition root that assembles the runtime, and the proxy, pilot, and
    dredger role runners.</p>
  </a>
  <a class="card" href="/docs/protocol-support/">
    <h2>Protocol support</h2>
    <p>Which registry protocols this server speaks, and exactly what is supported and what is
    not, per ecosystem. A statement for humans, not a client-integration contract.</p>
  </a>
</div>
