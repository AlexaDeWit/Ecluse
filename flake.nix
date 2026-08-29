{
  description = "ecluse: supply-chain resilience proxy for package registries";

  inputs = {
    # Single pinned nixpkgs: every tool comes from this one set. See
    # docs/architecture/release-supply-chain.md.
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-26.05";
    flake-utils.url = "github:numtide/flake-utils";
  };

  # Nothing here needs the flake's own source tree, but Nix always passes `self`,
  # so absorb it with `...` rather than binding it.
  outputs = { nixpkgs, flake-utils, ... }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};
        hlib = pkgs.haskell.lib;

        # OpenTelemetry 1.0 overlay. The pinned nixpkgs (26.05) ships the older
        # 0.x line: sdk 0.1.0.1, api 0.3.1.0, and no api-types package at all. The
        # project targets OTel 1.0, the May-2026 release that ships metrics and logs
        # alongside tracing, with OTLP export. So this overlay injects the 1.0 stack
        # for the Nix path, where callCabal2nix resolves from this set rather than
        # Hackage. That matches the 1.0 pin the cabal and Hackage path carries in
        # cabal.project and cabal.project.freeze. `callHackageDirect` pins each source
        # by exact version and Hackage tarball sha256: no flake input, and no runtime
        # fetch beyond the fixed-output derivation. The whole stack moves together.
        # Each 1.0 package then resolves its OTel deps against the other 1.0 packages
        # here, never the 0.x ones still in the base set.
        # See docs/architecture/observability.md → "OpenTelemetry as the substrate".
        otelOverlay = hself: _hsuper: {
          # cvss 0.3 adds CVSS v4 parsing and fixes v2 scoring. The base set pins
          # 0.2.0.1, which rejects the v4 vectors that about a third of the scored
          # npm OSV advisories carry. `dontCheck` skips its tasty test suite, which
          # is not ours to gate, so its test-only version bounds stay out of the
          # closure.
          cvss = pkgs.haskell.lib.dontCheck (hself.callHackageDirect {
            pkg = "cvss";
            ver = "0.3.0.0";
            sha256 = "sha256-fRdZv2yVIAgBz9R36+V69GHc5h91/4lPXU4RU5Q2Q4Q=";
          } { });
          hs-opentelemetry-api-types =
            hself.callHackageDirect {
              pkg = "hs-opentelemetry-api-types";
              ver = "1.0.0.0";
              sha256 = "sha256-9ByP41wlV45TMCqbyyVpwejQDi5fsG0+j8bMk8ORLw8=";
            } { };
          hs-opentelemetry-api =
            hself.callHackageDirect {
              pkg = "hs-opentelemetry-api";
              ver = "1.0.0.0";
              sha256 = "sha256-COhj9Ms1eu1Gt9wTC21oQ37k6vJ9mxlJvYpHtvXff6A=";
            } { };
          hs-opentelemetry-otlp =
            hself.callHackageDirect {
              pkg = "hs-opentelemetry-otlp";
              ver = "1.0.0.0";
              sha256 = "sha256-kVuKKi6qRx+oBQclTpUnx20Eqw+CRQk8pT4tkcxt1xo=";
            } { };
          hs-opentelemetry-semantic-conventions =
            hself.callHackageDirect {
              pkg = "hs-opentelemetry-semantic-conventions";
              ver = "1.40.0.0";
              sha256 = "sha256-7cIC9dTrd5bJjAsiEyyupi1xSZyc17FpjbACnm0p5ik=";
            } { };
          # The SDK 1.0 re-exports the standard propagators, so they travel with it
          # on the 1.0 line. W3C TraceContext is the default, and B3, Jaeger, X-Ray,
          # and Datadog are alternates a deployment selects. The Datadog one is the
          # optional, vendor-specific propagator from the observability design.
          hs-opentelemetry-propagator-b3 =
            hself.callHackageDirect {
              pkg = "hs-opentelemetry-propagator-b3";
              ver = "1.0.0.0";
              sha256 = "sha256-gsNe818CprXM9l61mLUsdnePxIQChfml9kegmCDoAmw=";
            } { };
          hs-opentelemetry-propagator-datadog =
            hself.callHackageDirect {
              pkg = "hs-opentelemetry-propagator-datadog";
              ver = "1.0.0.0";
              sha256 = "sha256-nTXEtira3bktvycZkjDmPZewyMJ1IEEDygLT9OiIFYo=";
            } { };
          hs-opentelemetry-propagator-jaeger =
            hself.callHackageDirect {
              pkg = "hs-opentelemetry-propagator-jaeger";
              ver = "1.0.0.0";
              sha256 = "sha256-VL+3YwKbqe0elfZQ0EN7icNS0+pxmtlxlKauPHRqhb8=";
            } { };
          hs-opentelemetry-propagator-w3c =
            hself.callHackageDirect {
              pkg = "hs-opentelemetry-propagator-w3c";
              ver = "1.0.0.0";
              sha256 = "sha256-p8d2Tx8bCVRk6hps8k0qAg/L2gdBVoYuLYJbTzTbI3s=";
            } { };
          hs-opentelemetry-propagator-xray =
            hself.callHackageDirect {
              pkg = "hs-opentelemetry-propagator-xray";
              ver = "1.0.0.0";
              sha256 = "sha256-Tg7TrCMb8GA+jm+ohMAqMW7othRm/HLEyr9SifGa6qI=";
            } { };
          hs-opentelemetry-exporter-handle =
            hself.callHackageDirect {
              pkg = "hs-opentelemetry-exporter-handle";
              ver = "1.0.0.0";
              sha256 = "sha256-DCoVG0Y2aaMjinOP2GWmew0WmjN96j3/UUzEWxN7Ajs=";
            } { };
          # The in-memory exporter is part of the SDK's own dependency closure in
          # this set. The base set's 0.x build does not compile against the 1.0 api,
          # because a SpanProcessor field changed type, so it moves to 1.0 too.
          hs-opentelemetry-exporter-in-memory =
            hself.callHackageDirect {
              pkg = "hs-opentelemetry-exporter-in-memory";
              ver = "1.0.0.0";
              sha256 = "sha256-bJjUHBNMRKhmkqRRnUrAQIDLWpUrox7F418r2QbVQ6o=";
            } { };
          hs-opentelemetry-sdk =
            hself.callHackageDirect {
              pkg = "hs-opentelemetry-sdk";
              ver = "1.0.0.0";
              sha256 = "sha256-kG8gmP8Lr9mPCnJjukCduFI/tADgKCfuelxcQZcXyA8=";
            } { };
          # HTTP/protobuf is the default. The gRPC path would pull in grapesy, and
          # it stays behind the package's cabal flag, off, matching the `-grpc` flag
          # the cabal freeze resolves. We need no gRPC.
          hs-opentelemetry-exporter-otlp =
            hself.callHackageDirect {
              pkg = "hs-opentelemetry-exporter-otlp";
              ver = "1.0.0.0";
              sha256 = "sha256-rHgsisH2d45CI9woEDb/j0WnTzllxaE2Mkx5/OmWn0c=";
            } { };
          # The pull-side metrics transport. The pinned SDK resolves
          # OTEL_METRICS_EXPORTER=prometheus to a no-op push exporter and leaves the
          # endpoint to the application, so renderPrometheusText has to come from
          # here. Same monorepo and same 1.0 line as the OTLP exporter above.
          hs-opentelemetry-exporter-prometheus =
            hself.callHackageDirect {
              pkg = "hs-opentelemetry-exporter-prometheus";
              ver = "1.0.0.0";
              sha256 = "sha256-6yCjTQ/1hriFD2n3zprJDHKBsjLAD5jzlSfOuN/3ej0=";
            } { };
          # Request-lifecycle instrumentation: the WAI server span and the
          # http-client data-plane child spans. The http-client instrumentation pulls
          # the conduit instrumentation as a 1.0 dependency, so it travels on the line
          # too. Their only OTel deps, api and semantic-conventions, are above.
          hs-opentelemetry-instrumentation-wai =
            hself.callHackageDirect {
              pkg = "hs-opentelemetry-instrumentation-wai";
              ver = "1.0.0.0";
              sha256 = "sha256-gPU9k2H1MpMEGh0F1Oi5ri8gdsZMCvQBRTnXgDhVAa0=";
            } { };
          hs-opentelemetry-instrumentation-conduit =
            hself.callHackageDirect {
              pkg = "hs-opentelemetry-instrumentation-conduit";
              ver = "1.0.0.0";
              sha256 = "sha256-J4iv0uTsnmntoXOb6tf8CBnKa0KsspomwLN/mJ2ypTA=";
            } { };
          hs-opentelemetry-instrumentation-http-client =
            hself.callHackageDirect {
              pkg = "hs-opentelemetry-instrumentation-http-client";
              ver = "1.0.0.0";
              sha256 = "sha256-/+XwCJzMYtmBoHBuDGkmHR8ETKkxpMMtWNNWpbAGPYQ=";
            } { };
        };

        # amazonka, built from source at the exact rev the cabal path pins in
        # cabal.project (source-repository-package). The Hackage release stops at
        # 2.0/2023 (GHC <= 9.6), so it must come from the upstream monorepo. Pinning
        # our own rev here, rather than riding nixpkgs' internal amazonkaSrc pin,
        # keeps amazonka steady. A flake.lock refresh can never move it out from
        # under the cabal path. `checks.amazonka-lockstep` holds this rev and
        # cabal.project's tag together: bump both in the same commit, then run
        # `task freeze`.
        amazonkaRev = "b562aa3f24845e34b95748daae671860017426be";
        amazonkaSrc = pkgs.fetchFromGitHub {
          owner = "brendanhay";
          repo = "amazonka";
          rev = amazonkaRev;
          hash = "sha256-zeA69byUJv59avBMfstNuHzeG8V09o87Fp9N98aioII=";
        };

        # The same subdirectory set cabal.project vendors: the umbrella package,
        # its own dependencies, and the service leaves we call. `dontCheck` for the
        # same reason as cvss above: their test suites are not ours to gate.
        amazonkaOverlay = hself: _hsuper:
          let
            fromMonorepo = name: subdir:
              hlib.dontCheck
                (hself.callCabal2nix name "${amazonkaSrc}/${subdir}" { });
          in {
            amazonka = fromMonorepo "amazonka" "lib/amazonka";
            amazonka-core = fromMonorepo "amazonka-core" "lib/amazonka-core";
            amazonka-sso = fromMonorepo "amazonka-sso" "lib/services/amazonka-sso";
            amazonka-sts = fromMonorepo "amazonka-sts" "lib/services/amazonka-sts";
            amazonka-codeartifact =
              fromMonorepo "amazonka-codeartifact" "lib/services/amazonka-codeartifact";
            amazonka-sqs = fromMonorepo "amazonka-sqs" "lib/services/amazonka-sqs";
            amazonka-s3 = fromMonorepo "amazonka-s3" "lib/services/amazonka-s3";
          };

        # HSEC advisory pins. The 26.05 set resolves lines that carry Haskell
        # advisories with released fixes. This overlay pins the fixed versions until
        # the base set carries them, the same mechanism as the OTel overlay above.
        # HSEC-2026-0007 (aeson and text-iso8601) is a decode-time denial of service,
        # and untrusted registry JSON is the serve hot path. HSEC-2026-0008
        # (crypton-x509 and crypton-x509-validation) leaves X.509 Name Constraints
        # unenforced, and certificate validation is the guarantor of the HTTPS-only
        # egress posture. The x509 family, tls, and crypton-connection move as one
        # coherent set. Version 2.3 is the first tls line accepting x509 1.9, and
        # crypton-connection 0.4.6 is the first accepting tls 2.3. Both tls 2.3 and
        # x509 1.9 need crypton 1.1. `aeson-pretty` 0.8.11 is the first release
        # admitting aeson 2.3. dontCheck for the same reason as cvss above.
        advisoryOverlay = hself: hsuper:
          let
            fromHackage = pkg: ver: sha256:
              hlib.dontCheck (hself.callHackageDirect { inherit pkg ver sha256; } { });
          in {
            aeson = fromHackage "aeson" "2.3.0.0"
              "sha256-nIUIE+wLCHTxhiKimKb1v8iTFxpQrzgyF8yY+27BrXY=";
            text-iso8601 = fromHackage "text-iso8601" "0.2.0.0"
              "sha256-EGwbQnjRumL2ztwXman6uEEvB0oRspEUB1tl0NyjAGI=";
            aeson-pretty = fromHackage "aeson-pretty" "0.8.11"
              "sha256-SmMKRymEU239HsYJL5oVh5eJpFqHoQdKTcw/jyWLeAU=";
            crypton-x509 = fromHackage "crypton-x509" "1.9.1"
              "sha256-8VZ64FbEiLj4o+Nm9ZzpNH7ZYs9w6rTkLvT9p2PgBf4=";
            crypton-x509-store = fromHackage "crypton-x509-store" "1.9.0"
              "sha256-GQcCEDOYh9N42GSh/A0pYGuIRqk3zwZ/lpOGxFrnClQ=";
            crypton-x509-system = fromHackage "crypton-x509-system" "1.9.0"
              "sha256-a3rnvpO1xYVFxxIKhp7aObh+oVj49KJqGcv2NuaAgPs=";
            crypton-x509-validation = fromHackage "crypton-x509-validation" "1.9.1"
              "sha256-YKufVgXC8qz80tScE3vENVrwJD1MgZYRNnjqirscrLA=";
            tls = fromHackage "tls" "2.3.0"
              "sha256-kip1dltP9SvauoTYSJ7Hi0bwM6bg5nVJnjTgQlZByhI=";
            crypton-connection = fromHackage "crypton-connection" "0.4.6"
              "sha256-B+fGwEBaChFx9mrrG5JbIjTnCSfiwuPetOOoua5FMGA=";
            crypton = fromHackage "crypton" "1.1.4"
              "sha256-tJSK0HoDabSqcUGORs856Jl6aWZnrqyXPSAh66HsKMM=";
            hpke = fromHackage "hpke" "0.1.0"
              "sha256-7bZvdB8unUZxBjrp0CSHn6dIMEJpJ20GrsbRLCT62B0=";
            http-client-tls = fromHackage "http-client-tls" "0.4.0"
              "sha256-OvQMk+ewLWN+zMrWc6hphID9DQ32FnasXNeMG/+A/OE=";
            # attoparsec-aeson 2.2.2.0 revision 1 is Hackage's own widening to
            # aeson <2.4. The set builds the tarball's original <2.3 file.
            attoparsec-aeson = hlib.overrideCabal hsuper.attoparsec-aeson (_drv: {
              revision = "1";
              editedCabalFile = "sha256-CJSPRbiSxXWNLELiL+L71BpPbcOV+wpDwr9Fih8pVzY=";
            });
            # cborg's library has no aeson dependency. Only its test suite caps
            # aeson <2.3, so dropping the suite drops the conflict.
            cborg = hlib.dontCheck hsuper.cborg;
            # No insert-ordered-containers or openapi3 release or revision admits
            # aeson 2.3 yet. They compile against it, as this build and the suites
            # prove, so strip the stale caps until upstream widens.
            insert-ordered-containers = hlib.doJailbreak hsuper.insert-ordered-containers;
            openapi3 = hlib.doJailbreak hsuper.openapi3;
          };

        hpkgs = pkgs.haskell.packages.ghc910.override {
          overrides = pkgs.lib.composeManyExtensions [
            otelOverlay
            amazonkaOverlay
            advisoryOverlay
          ];
        };

        # Shell tooling resolves from the unmodified ghc910 set. The tools do not
        # ship in the app closure, so the overlays above do not apply to them. The
        # pristine set keeps them binary-cached instead of rebuilding each tool's own
        # dependency graph against every pin. It is the same compiler as hpkgs, so
        # the .hie-consuming tools (stan, weeder) and doctest stay compatible.
        #
        # Standalone tools enter the shells as STATIC executables, never as set
        # packages. A dynamically linked tool drags its whole Haskell library
        # universe into the shell closure. Two sets are in play, hpkgs for the app
        # and this one for tools. That closure bloat pushes the CI Nix store past its
        # cache trim caps (setup-toolchain's nix-gc-max-store-size), and turns every
        # cache save partial. Tools with a top-level nixpkgs attribute
        # (pkgs.hlint, pkgs.fourmolu, and so on) are already static and
        # Hydra-cached, and justStaticExecutables below wraps the rest. Only the
        # genuinely set-coupled tools (ghc, doctest, hspec-discover) stay as set
        # packages.
        toolHpkgs = pkgs.haskell.packages.ghc910;

        # The cabal package, built by Nix. callCabal2nix reads ecluse.cabal and
        # resolves dependencies from the nixpkgs GHC 9.10 set that flake.lock pins,
        # not from Hackage. This is the build and CI artifact. `cabal` in the dev
        # shell stays the incremental inner loop, because Nix rebuilds the whole
        # package on any change and is poor for edit-compile cycles.
        ecluseRaw = hpkgs.callCabal2nix "ecluse" ./. { };

        # Dep-extraction variant of ecluseRaw: cabal2nix omits benchmark
        # components unless asked, and the freeze must pin the bench deps too.
        # This variant feeds getCabalDeps below and is never built, so enabling
        # benchmarks here costs the artifact nothing.
        ecluseForDeps =
          hpkgs.callCabal2nixWithOptions "ecluse" ./. "--benchmark" { };

        # Every Haskell dependency of every ecluse component (library, executable,
        # test suites, benchmarks, build tools) as one GHC package environment.
        # `ghc-pkg` can then report the exact resolved closure, boot libraries
        # included.
        ecluseDepEnv =
          let d = ecluseForDeps.getCabalDeps;
          in hpkgs.ghcWithPackages (_:
            pkgs.lib.filter (x: x != null) (pkgs.lib.concatLists [
              (d.buildDepends or [ ])
              (d.libraryHaskellDepends or [ ])
              (d.executableHaskellDepends or [ ])
              (d.testHaskellDepends or [ ])
              (d.benchmarkHaskellDepends or [ ])
              (d.setupHaskellDepends or [ ])
              (d.libraryToolDepends or [ ])
              (d.executableToolDepends or [ ])
              (d.testToolDepends or [ ])
              (d.benchmarkToolDepends or [ ])
            ]));

        # cabal.project.freeze, derived from this flake's package set. The Nix side
        # is the version authority, and the freeze projects it onto the cabal path.
        # So `cabal` in the dev shell and the CI gate resolves the exact closure
        # behind the shipped artifact. Versions only: flag choices follow each
        # side's defaults, and no index-state line appears, because cabal.project
        # owns the only copy. The z- ids ghc-pkg also registers are internal
        # sublibraries of dependencies, not solver targets, hence the filter.
        # Regenerate with `task freeze`. checks.freeze-sync keeps the committed file
        # honest.
        cabalFreeze = pkgs.runCommand "cabal.project.freeze" { } ''
          ${ecluseDepEnv}/bin/ghc-pkg list --simple-output \
            | tr ' ' '\n' \
            | awk -F- '
                NF >= 2 && $NF ~ /^[0-9][0-9.]*$/ {
                  v = $NF
                  name = $1
                  for (i = 2; i < NF; i++) name = name "-" $i
                  if (name != "ecluse" && name !~ /^z-/) print name " " v
                }' \
            | LC_ALL=C sort -u \
            | awk '
                BEGIN { pre = "constraints: " }
                { lines[NR] = pre "any." $1 " ==" $2; pre = "             " }
                END {
                  for (i = 1; i < NR; i++) print lines[i] ","
                  print lines[NR]
                }' \
            > $out
        '';

        # Release artifact: the library and the executable only. dontCheck keeps
        # the build from running the test suites. Those run on the cabal path (see
        # docs/testing.md), and the impure suites (integration → Docker, smoke →
        # live network) never belong in a hermetic build.
        ecluse = hlib.dontCheck ecluseRaw;

        # The executable alone, stripped and with its reference to the full
        # Haskell library closure removed (justStaticExecutables). A plain dynamic
        # build drags that whole closure in and bloats the image to about 500 MB.
        # This keeps only the binary and its system C deps for the container image.
        ecluseBinUnpruned = hlib.justStaticExecutables ecluse;

        # GHC's x86_64 Linux RTS links libdw and libelf for DWARF stack unwinding.
        # The nixpkgs build puts those libraries and the unused libdebuginfod in one
        # elfutils output. `libdebuginfod` alone retains curl, libssh2, OpenSSL,
        # Kerberos, and the HTTP/2 and HTTP/3 stacks in the image closure. Build the
        # same elfutils ABI without debuginfod, keeping the RTS feature and removing
        # that unreachable network-client surface. `elfutils` still needs pkg-config
        # at configure time, although nixpkgs ties that native input to the
        # debuginfod option.
        elfutilsWithoutDebuginfod =
          (pkgs.elfutils.override { enableDebuginfod = false; }).overrideAttrs
            (old: {
              nativeBuildInputs =
                (old.nativeBuildInputs or [ ]) ++ [ pkgs.pkg-config ];
            });

        # The arm64 GHC build does not enable DWARF support, and Darwin does not
        # ship this Linux image closure. So only the affected release platform needs
        # the ABI-compatible substitution.
        ecluseBin =
          if pkgs.stdenv.hostPlatform.isLinux
            && pkgs.stdenv.hostPlatform.isx86_64
          then
            pkgs.replaceDependency {
              drv = ecluseBinUnpruned;
              oldDependency = pkgs.lib.getLib pkgs.elfutils;
              newDependency = pkgs.lib.getLib elfutilsWithoutDebuginfod;
            }
          else
            ecluseBinUnpruned;

        # The npm version-ordering oracle (`node-semver`) for the differential
        # smoke suite and `task gen-version-fixtures`. nixpkgs 26.05 removed the
        # node2nix-generated `nodePackages` set. The blessed replacement is to build
        # node_modules from a committed lockfile. `importNpmLock` reads the
        # integrity hashes already in test/oracles/package-lock.json, so there is no
        # separate Nix hash to maintain. The oracle sits deliberately outside
        # Renovate's reach (renovate.json5 ignorePaths): it is reference test data,
        # refreshed by hand with the fixture workflow, never auto-bumped. NODE_PATH
        # below exposes it, so `require("semver")` resolves.
        oracleNodeModules = pkgs.importNpmLock.buildNodeModules {
          npmRoot = ./test/oracles;
          inherit (pkgs) nodejs;
        };

        # ---- Dev-shell composition -------------------------------------------
        # Env shared by every shell. A UTF-8 locale, so hspec's '✔' and other
        # Unicode output encode regardless of host locale. Plus the NODE_PATH that
        # makes `require("semver")` resolve for the npm version oracle (fixture
        # generator and smoke suite).
        shellEnv = {
          LANG = "C.UTF-8";
          LC_ALL = "C.UTF-8";
          NODE_PATH = "${oracleNodeModules}/node_modules";
          AWS_EC2_METADATA_DISABLED = "true";
        };

        # ---- Tool tiers --------------------------------------------------------
        # The shells compose from explicit tiers, each a strict layer on the one
        # below. They are `toolchain` (compile and test), `gate` (what the gating CI
        # jobs add), and `ops` (release and vulnerability scanning). Then `bench`
        # (benchmarking and profiling) and `ide` (interactive only). `ci` is
        # toolchain ∪ gate ∪ ops ∪ bench, deliberately ONE closure (see
        # `ciShellInputs`). `default` adds `ide` on top, and `mcp` is toolchain plus
        # the LSP bridge. A tool's reason for being in the closure stays documented
        # beside it.

        # Compiling and testing: what every build path needs before any gate
        # tooling. `go-task` lives here and here only, because every job and every
        # dev loop runs through it.
        toolchainInputs = [
          pkgs.bashInteractive
          toolHpkgs.ghc
          pkgs.cabal-install
          pkgs.zlib
          pkgs.pkg-config
          # Reference version-ordering oracles for the differential smoke suite
          # and `task gen-version-fixtures`. They are node-semver (npm, built by
          # oracleNodeModules and put on NODE_PATH above), Python packaging (PyPI),
          # and Ruby Gem::Version (built into ruby).
          pkgs.nodejs
          (pkgs.python3.withPackages (ps: [ ps.packaging ]))
          pkgs.ruby
          pkgs.go-task
        ];

        # What the gating jobs add on top of the toolchain: formatting, linting,
        # doctests, and coverage conversion. Also SAST, workflow linting, dead-code
        # and static analysis, and the site build.
        gateInputs = [
          pkgs.fourmolu
          pkgs.hlint
          # Run the >>> examples in Haddock comments as tests (`task doctest`),
          # via `cabal repl --with-ghc=doctest`. Must come from the same GHC 9.10
          # set as the compiler it stands in for. See docs/haddock.md → "Examples that
          # run".
          toolHpkgs.doctest
          # Convert HPC coverage output (.tix/.mix) to Codecov JSON for the
          # `coverage` target (see CONTRIBUTING.md → "Coverage").
          (hlib.justStaticExecutables toolHpkgs.hpc-codecov)
          pkgs.semgrep
          # GitHub Actions linting (`task lint-workflows`). `actionlint` checks
          # correctness (shellcheck over `run:` blocks, expression and context
          # checks), and zizmor checks security (template injection, credential
          # persistence, excessive permissions, dangerous triggers). It mechanises
          # the injection-free workflow rule in AGENTS.md → "CI & Security".
          pkgs.actionlint
          pkgs.zizmor
          # shellcheck for `task lint-scripts` (scripts/*.sh). actionlint already
          # runs shellcheck on workflow `run:` blocks. This lints the committed
          # scripts too.
          pkgs.shellcheck
          # Dead-code (weeder) and HIE-based static analysis (stan). Both gate CI
          # through their own jobs, and both are in `task check`.
          (hlib.justStaticExecutables toolHpkgs.weeder)
          (hlib.justStaticExecutables toolHpkgs.stan)
          # Zola builds the static site under web/, and pagefind indexes the built
          # output into the search bundle. Both the publish and the site gate run them.
          pkgs.zola
          pkgs.pagefind
          # d2 renders web/diagrams/*.d2 to SVG in the site build (`task site` and
          # the site-stub gate). A single Go binary: no npm closure, no client JS.
          pkgs.d2
        ];

        # Interactive-only tooling: in the default (human) shell, never needed by
        # CI. HLS and ghcid give live feedback, and hoogle and cabal-plan search the
        # API and the build plan (see AGENTS.md).
        ideInputs = [
          pkgs.haskell-language-server
          pkgs.ghcid
          (hlib.justStaticExecutables toolHpkgs.hoogle)
          (hlib.justStaticExecutables toolHpkgs.cabal-plan)
          # The Spec.hs entry points carry `-pgmF hspec-discover`, a source
          # preprocessor GHC shells out to. cabal supplies it during a build through
          # build-tool-depends. HLS runs the same preprocessor when it loads those
          # modules, and does not reproduce cabal's build-tool PATH, so it reports
          # "could not execute: hspec-discover". Put it on the shell PATH for HLS,
          # from the same GHC 9.10 set, matching the build-tool-depends version. CI
          # never needs it here, because cabal provides it, so it stays out of the
          # CI tiers.
          toolHpkgs.hspec-discover
          # The LSP<->MCP bridge, defined below and also exposed as
          # packages.agent-lsp. This shell launches it through .mcp.json. Its whole
          # closure is about 53 MiB, so it rides the ide tier rather than earning a
          # shell of its own.
          agent-lsp
        ];

        # Release and vulnerability-scanning tooling.
        #
        # `skopeo` converts the two Nix-built per-arch archives into the OCI layout
        # that release.yml pushes (scripts/push-multiarch.sh), with no Docker daemon
        # needed. `sbomnix` generates the Nix-native SBOM (`task sbom`), which is more
        # accurate than scanning a distroless image, whose static Haskell deps a
        # scanner cannot see. The provenance and SBOM attestations themselves come
        # from the GitHub attest-actions in CI, as immutable OCI referrers. See
        # docs/architecture/release-supply-chain.md → "Supply-chain attestations".
        #
        # `grype` is the C-closure scan authority (`task scan`). It scans the
        # sbomnix SBOM of the image's C closure (openssl, curl, glibc, and the rest)
        # against its maintained DB. It emits the SARIF that security.yml uploads to
        # code scanning. `vulnix` is a secondary, Nix-native cross-check
        # (`task scan-vulnix`): wider and patch-aware, but un-graded, so not the
        # authority. `osv-scanner` is the Haskell-closure (HSEC) scan authority
        # (`task scan-osv`). OSV.dev carries the HSEC advisories, and the scan reads
        # cabal.project.freeze natively. `checks.freeze-sync` keeps that file
        # equal to this flake's package set, so it describes the closure the image
        # ships. Its SARIF goes to code scanning too. See
        # docs/architecture/release-supply-chain.md → "Vulnerability scanning and
        # dependency freshness".
        opsInputs = [
          pkgs.skopeo
          # regclient ships `regctl`, which assembles the two per-arch OCI
          # layouts skopeo writes into the single multi-arch index a release
          # publishes (scripts/push-multiarch.sh). Daemonless like skopeo, so
          # the publish job needs no container engine.
          pkgs.regclient
          pkgs.sbomnix
          pkgs.grype
          pkgs.vulnix
          pkgs.osv-scanner
          # jq: scripts/sarif-locations.sh post-processes both scanners' SARIF for
          # GitHub code scanning, and scripts/push-multiarch.sh reads the pushed
          # index for its per-platform digests. Pinned here rather than relying on
          # the runner's.
          pkgs.jq
          # reuse: per-file SPDX licence headers. `task lint-spdx` stamps and gates
          # through `reuse lint-file` and `reuse annotate`. A Python tool, justified
          # over a hand-rolled shell gate because it is the SPDX and REUSE reference
          # implementation. It stays scoped to tracked .hs files, with no repo-wide
          # REUSE regime, and lives in the dev shell only, never on the product
          # path. See docs/style.md, "Licence headers".
          pkgs.reuse
        ];

        # Benchmarking and profiling: `task bench`, `task bench-load`, and the
        # cost-centre flame graph from `task bench-profile`. ghc-prof-flamegraph
        # comes from the same pinned ghc910 set as every other Haskell tool. The
        # 26.05 default set happens to be 9.10 too, so this is set-consistency
        # insurance rather than a closure change.
        benchInputs = [
          (hlib.justStaticExecutables toolHpkgs.ghc-prof-flamegraph)
          pkgs.flamegraph
          pkgs.oha
        ];

        # agent-lsp: the LSP<->MCP bridge for this project. It lets an MCP client,
        # an agent harness for example, drive haskell-language-server's semantic
        # navigation instead of lexical grep. That covers go-to-definition,
        # find-references, hover and type-at-point, diagnostics, and rename. It is
        # built on a *complete* LSP client. The obvious alternative fails.
        # `mcp-language-server` v0.1.1 is an incomplete client that leaves HLS
        # deadlocked at about 0 % CPU on every semantic request, verified against
        # this project. The same HLS is flawless under VS Code's
        # vscode-languageclient. See AGENTS.md → "Build & Tooling". It is not in
        # nixpkgs, so buildGoModule builds it from tagged source. `go.mod` needs Go
        # 1.26 (the 26.05 set ships go_1_26), and it is pure-Go (modernc sqlite, no
        # cgo). It rides the ide tier, because that shell already carries the GHC
        # 9.10 toolchain and the hspec-discover that HLS needs to load the Spec.hs
        # modules.
        agent-lsp = (pkgs.buildGoModule.override { go = pkgs.go_1_26; }) rec {
          pname = "agent-lsp";
          version = "0.15.0";
          src = pkgs.fetchFromGitHub {
            owner = "blackwell-systems";
            repo = "agent-lsp";
            rev = "v${version}"; # commit ab89838db139125bcf0e3c4e0c10addf57ed52c6
            hash = "sha256-l04uuMP4giVUykDpR4mWK2P+Tkj/E16EqDuMOEYNa8U=";
          };
          vendorHash = "sha256-/y+v/aCzqigLut3kljCwa5iMD5yMLK1L5ul9ue8YFqU=";
          subPackages = [ "cmd/agent-lsp" ]; # the server binary only, skipping ./scripts, ./test, experiments
          doCheck = false; # its test suite spins up real language servers
          ldflags = [ "-s" "-w" "-X main.Version=${version}" ];
        };

        # Every tool any CI job drives through `task`, in ONE closure: the
        # toolchain tier plus everything the gate, ops, and bench jobs add. Bundling
        # them means build-test realises the whole thing and writes a single GitHub
        # Actions cache entry. Every downstream job then gets a 100% cache hit
        # instead of substituting its own tools from cache.nixos.org on each run.
        # Every gate job therefore enters `.#ci`. Do not add a job-specific shell
        # without a closure that genuinely differs, because a second closure means a
        # second cache entry to warm and evict.
        ciShellInputs =
          toolchainInputs ++ gateInputs ++ opsInputs ++ benchInputs;
      in {
        packages = {
          default = ecluse;
          ecluse = ecluse;
          agent-lsp = agent-lsp; # LSP<->MCP bridge (see mcpInputs), built by `nix build .#agent-lsp`

          # The exact stripped, static binary that ships inside the image
          # (`justStaticExecutables`, no Haskell-library closure). Exposed so the
          # SBOM and any verifier can target what the image contains, through
          # `nix build .#ecluse-bin`, rather than the noisier dynamic package.
          ecluse-bin = ecluseBin;

          # The freeze projection of the package set (see cabalFreeze above).
          # `task freeze` copies it over cabal.project.freeze.
          cabal-freeze = cabalFreeze;

          # Lean, reproducible OCI image, built straight from the binary's Nix
          # closure, with no Dockerfile and no base distro. It contains only the
          # runtime closure plus CA certificates: no shell, no package manager, and
          # it runs non-root. `buildLayeredImage` splits the closure into
          # cache-friendly layers, and the build is bit-for-bit reproducible, a
          # fitting property for a supply-chain tool. `tag = null` derives a unique
          # content-hash tag for local use. A release retags at push time, because
          # the target repo enforces immutable tags (see CONTRIBUTING.md
          # "Releases"). release.yml pushes the multi-arch index. The GitHub
          # attest-actions attach the provenance and SBOM attestations in CI.
          dockerImage = pkgs.dockerTools.buildLayeredImage {
            name = "ecluse";
            tag = null;
            contents = [ ecluseBin pkgs.cacert ];
            config = {
              Entrypoint = [ "${ecluseBin}/bin/ecluse" ];
              ExposedPorts = { "8080/tcp" = { }; }; # default ECLUSE_SERVER__PORT
              User = "65532:65532"; # nonroot, distroless convention
              Env = [
                # A distroless image has no system trust store, so tls/x509-system
                # finds no CAs and every outbound HTTPS fetch fails. `contents`
                # above puts cacert in the closure. Point GHC's TLS stack at it.
                "SSL_CERT_FILE=${pkgs.cacert}/etc/ssl/certs/ca-bundle.crt"
              ];
              Labels = {
                "org.opencontainers.image.title" = "ecluse";
                "org.opencontainers.image.description" =
                  "Supply-chain resilience proxy for package registries";
                "org.opencontainers.image.source" =
                  "https://github.com/AlexaDeWit/Ecluse";
                "org.opencontainers.image.licenses" = "MIT";
              };
            };
          };
        };

        # Flake checks. The CI `docs` job builds all of them.
        #
        # This one is here rather than in the Taskfile because Nix can do something
        # cabal cannot. The dependency closure comes prebuilt from the pinned Haskell
        # set WITH its .haddock interfaces, so only ecluse compiles and haddocks. The
        # cabal path (`task docs-check`) instead rebuilds the whole ~188-package
        # closure on every CI run. `cabal haddock` wants a documentation variant of
        # the deps that build-test's `cabal build` store does not have. That
        # asymmetry is the bar a check must clear to earn a Nix implementation. A
        # check that merely re-runs a tool the dev shell already pins belongs in the
        # Taskfile, where it runs incrementally and is what actually gates.
        # `doHaddock` forces the Haddock pass, so a broken doc comment fails the
        # build. `dontCheck` skips the test suites.
        checks.docs = hlib.doHaddock ecluse;

      # The two checks below clear the same bar in a different way. Each compares
      # the Nix and cabal views of one pin, which only Nix evaluation can see side
      # by side.

      # The committed freeze must equal the one derived from the package set.
      checks.freeze-sync = pkgs.runCommand "freeze-sync" { } ''
        if ! diff -u ${./cabal.project.freeze} ${cabalFreeze}; then
          echo "cabal.project.freeze does not match the Nix package set." >&2
          echo "Regenerate with 'task freeze' and commit the result." >&2
          exit 1
        fi
        touch $out
      '';

      # cabal.project's amazonka source-repository-package tag must equal the
      # rev this flake builds amazonka from.
      checks.amazonka-lockstep = pkgs.runCommand "amazonka-lockstep" { } ''
        if ! grep -Eq '^[[:space:]]*tag:[[:space:]]*${amazonkaRev}[[:space:]]*$' \
            ${./cabal.project}; then
          echo "cabal.project's amazonka tag differs from flake.nix amazonkaRev." >&2
          echo "Bump both in the same commit, then run 'task freeze'." >&2
          exit 1
        fi
        touch $out
      '';

      devShells = {
        # The shell every CI job enters. See `ciShellInputs` for why it is one
        # closure rather than a per-job set.
        ci = pkgs.mkShell (shellEnv // {
          name = "ecluse-ci";
          buildInputs = ciShellInputs;
        });

        # The shell for humans: everything CI has, plus the interactive tooling CI
        # never needs. That difference is the ONLY one, and it is deliberately a
        # strict superset. A human can reproduce any gate job locally, and the IDE
        # closure stays out of what CI realises and caches. HLS alone is about 8 GB,
        # the heaviest and flakiest part to substitute.
        default = pkgs.mkShell (shellEnv // {
          name = "ecluse";
          buildInputs = ciShellInputs ++ ideInputs;
        });

      };
    });
}
