{
  description = "ecluse: supply-chain resilience proxy for package registries";

  inputs = {
    # Packages share one pinned nixpkgs. External source and binary pins stay explicit.
    # See docs/architecture/release-supply-chain.md.
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-26.05";
    flake-utils.url = "github:numtide/flake-utils";
    # The v0.10.0 packaging commit, one after the tag. A release tag's Nix
    # package still installs the previous release's binary.
    saerskriven.url = "github:AlexaDeWit/Saerskriven/b1ade081654a0cfbd4c6e965605db2e34748d4e9";
    saerskriven.inputs.nixpkgs.follows = "nixpkgs";
    saerskriven.inputs.flake-utils.follows = "flake-utils";
  };

  outputs = { nixpkgs, flake-utils, saerskriven, ... }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = nixpkgs.legacyPackages.${system}.extend (_final: prev: {
          # Remove when actionlint releases support for $/ (rhysd/actionlint#732).
          actionlint = prev.actionlint.overrideAttrs (old: {
            patches = (old.patches or [ ]) ++ [
              # fetchpatch strips the index lines, whose hash length GitHub changes.
              (prev.fetchpatch {
                url = "https://github.com/rhysd/actionlint/compare/011a6d15e749bb3f2d771eed9c7aa0e7e3e10ee7...b02c24b743cc88a26b280339814eb4ece91c32cb.diff";
                hash = "sha256-ssleL7s/8EDmmN8b5RNHanVfn0Imz2X2MY3wAMwDVQw=";
              })
            ];
          });
        });
        hlib = pkgs.haskell.lib;

        # A hash-pinned Hackage release with dontCheck. Upstream suites are not ours to
        # run, and their test-only inputs and version bounds stay out of the closure.
        hackageNoCheck = hself: pkg: ver: sha256:
          hlib.dontCheck (hself.callHackageDirect { inherit pkg ver sha256; } { });

        # True when pkg's output differs from the binary-cached base set's.
        builtFromSource = name: pkg:
          let served = builtins.tryEval (pkgs.haskell.packages.ghc910.${name}.outPath or null);
          in !(served.success && served.value == pkg.outPath);

        # Returns value. Evaluation fails instead, and names the packages, when a third-party
        # Haskell package in the closure of roots builds from source with its suite on.
        noUpstreamSuites = roots: value:
          let
            node = drv: { key = drv.outPath; val = drv; };
            closure = builtins.genericClosure {
              startSet = map node roots;
              operator = { val, ... }: map node val.getBuildInputs.haskellBuildInputs;
            };
            runs = { val, ... }:
              val.doCheck && !(builtins.elem val.pname [ "ecluse" "ecluse-lzma-managed" ])
              && builtFromSource val.pname val;
            running = map ({ val, ... }: val.pname) (builtins.filter runs closure);
          in
          assert pkgs.lib.assertMsg (running == [ ]) ("Upstream suites run in a CI build. "
            + "Turn them off in flake.nix: ${pkgs.lib.concatStringsSep ", " running}");
          value;

        # The base set carries OTel 0.x. Keep these 1.0 pins aligned with cabal.project
        # and its freeze (docs/architecture/observability.md).
        otelOverlay = hself: _hsuper:
          let fromHackage = hackageNoCheck hself;
          in {
            # 0.3 parses CVSS v4 and fixes v2 scoring. The base set's 0.2.0.1 rejects
            # the v4 vectors that about a third of scored npm OSV advisories carry.
            cvss = fromHackage "cvss" "0.3.0.0"
              "sha256-fRdZv2yVIAgBz9R36+V69GHc5h91/4lPXU4RU5Q2Q4Q=";
            hs-opentelemetry-api-types =
              fromHackage "hs-opentelemetry-api-types" "1.0.0.0"
                "sha256-9ByP41wlV45TMCqbyyVpwejQDi5fsG0+j8bMk8ORLw8=";
            hs-opentelemetry-api =
              fromHackage "hs-opentelemetry-api" "1.0.0.0"
                "sha256-COhj9Ms1eu1Gt9wTC21oQ37k6vJ9mxlJvYpHtvXff6A=";
            hs-opentelemetry-otlp =
              fromHackage "hs-opentelemetry-otlp" "1.0.0.0"
                "sha256-kVuKKi6qRx+oBQclTpUnx20Eqw+CRQk8pT4tkcxt1xo=";
            hs-opentelemetry-semantic-conventions =
              fromHackage "hs-opentelemetry-semantic-conventions" "1.40.0.0"
                "sha256-7cIC9dTrd5bJjAsiEyyupi1xSZyc17FpjbACnm0p5ik=";
            # The SDK 1.0 re-exports these, so they stay on its line. W3C TraceContext
            # is the default. B3, Jaeger, X-Ray, and Datadog are alternates to select.
            hs-opentelemetry-propagator-b3 =
              fromHackage "hs-opentelemetry-propagator-b3" "1.0.0.0"
                "sha256-gsNe818CprXM9l61mLUsdnePxIQChfml9kegmCDoAmw=";
            hs-opentelemetry-propagator-datadog =
              fromHackage "hs-opentelemetry-propagator-datadog" "1.0.0.0"
                "sha256-nTXEtira3bktvycZkjDmPZewyMJ1IEEDygLT9OiIFYo=";
            hs-opentelemetry-propagator-jaeger =
              fromHackage "hs-opentelemetry-propagator-jaeger" "1.0.0.0"
                "sha256-VL+3YwKbqe0elfZQ0EN7icNS0+pxmtlxlKauPHRqhb8=";
            hs-opentelemetry-propagator-w3c =
              fromHackage "hs-opentelemetry-propagator-w3c" "1.0.0.0"
                "sha256-p8d2Tx8bCVRk6hps8k0qAg/L2gdBVoYuLYJbTzTbI3s=";
            hs-opentelemetry-propagator-xray =
              fromHackage "hs-opentelemetry-propagator-xray" "1.0.0.0"
                "sha256-Tg7TrCMb8GA+jm+ohMAqMW7othRm/HLEyr9SifGa6qI=";
            hs-opentelemetry-exporter-handle =
              fromHackage "hs-opentelemetry-exporter-handle" "1.0.0.0"
                "sha256-DCoVG0Y2aaMjinOP2GWmew0WmjN96j3/UUzEWxN7Ajs=";
            # The unit suites use the in-memory exporter. The 0.x build fails against
            # the 1.0 api, where a SpanProcessor field changed type, so it moves to 1.0.
            hs-opentelemetry-exporter-in-memory =
              fromHackage "hs-opentelemetry-exporter-in-memory" "1.0.0.0"
                "sha256-bJjUHBNMRKhmkqRRnUrAQIDLWpUrox7F418r2QbVQ6o=";
            hs-opentelemetry-sdk =
              fromHackage "hs-opentelemetry-sdk" "1.0.0.0"
                "sha256-kG8gmP8Lr9mPCnJjukCduFI/tADgKCfuelxcQZcXyA8=";
            # HTTP/protobuf only. The gRPC flag stays off, as on the cabal path,
            # because gRPC pulls in grapesy.
            hs-opentelemetry-exporter-otlp =
              fromHackage "hs-opentelemetry-exporter-otlp" "1.0.0.0"
                "sha256-rHgsisH2d45CI9woEDb/j0WnTzllxaE2Mkx5/OmWn0c=";
            # The pinned SDK maps OTEL_METRICS_EXPORTER=prometheus to a no-op push
            # exporter, so the pull endpoint's renderPrometheusText comes from here.
            hs-opentelemetry-exporter-prometheus =
              fromHackage "hs-opentelemetry-exporter-prometheus" "1.0.0.0"
                "sha256-6yCjTQ/1hriFD2n3zprJDHKBsjLAD5jzlSfOuN/3ej0=";
            # The WAI server span and the http-client child spans. The http-client one
            # needs the conduit one at 1.0, so the three move together.
            hs-opentelemetry-instrumentation-wai =
              fromHackage "hs-opentelemetry-instrumentation-wai" "1.0.0.0"
                "sha256-gPU9k2H1MpMEGh0F1Oi5ri8gdsZMCvQBRTnXgDhVAa0=";
            hs-opentelemetry-instrumentation-conduit =
              fromHackage "hs-opentelemetry-instrumentation-conduit" "1.0.0.0"
                "sha256-J4iv0uTsnmntoXOb6tf8CBnKa0KsspomwLN/mJ2ypTA=";
            hs-opentelemetry-instrumentation-http-client =
              fromHackage "hs-opentelemetry-instrumentation-http-client" "1.0.0.0"
                "sha256-/+XwCJzMYtmBoHBuDGkmHR8ETKkxpMMtWNNWpbAGPYQ=";
          };

        transportOverlay = hself: _hsuper:
          let fromHackage = hackageNoCheck hself;
          in {
            warp = fromHackage "warp" "3.4.16"
              "03sn2vjpy99xhkj7yi80hc00hr7aaskwjypv6crknj1arhc7h4pd";
            wai = fromHackage "wai" "3.2.5"
              "17ay74pj72jhzvs60xqmhx50qrc0779mac3a1f4vfnfmp57z8s1y";
            http2 = fromHackage "http2" "5.4.9"
              "16018v8rxxcl3axzz0d8c4r7dhpazyll9k7d6lfbg571sbpd61wv";
            http-semantics = fromHackage "http-semantics" "0.4.1"
              "0pl6fm4q11j2i9li736i4ynrrzbcvyn3fd7q34ckslmlsq860cwg";
            time-manager = fromHackage "time-manager" "0.4.0"
              "sha256-olmyB/haQjdvaPKOoB0GqUcu6Ncp5JzW+AjKRwSUq1I=";
            network = fromHackage "network" "3.2.9.0"
              "1vabvmmyypr8p2y7skw62i3q7m0dl9ia930x8ivfd3ad4nd5a92n";
            recv = fromHackage "recv" "0.1.2"
              "12cs8cibkx6mz6593x708j3wj4iv76ga90sc7v31bg0dzzgphpdd";
          };

        # The rev cabal.project pins, not nixpkgs' (Hackage's amazonka stops at GHC 9.6).
        # checks.amazonka-lockstep keeps the two equal: bump both, then run `task freeze`.
        amazonkaRev = "b562aa3f24845e34b95748daae671860017426be";
        amazonkaSrc = pkgs.fetchFromGitHub {
          owner = "brendanhay";
          repo = "amazonka";
          rev = amazonkaRev;
          hash = "sha256-zeA69byUJv59avBMfstNuHzeG8V09o87Fp9N98aioII=";
        };

        # The subdirectories cabal.project vendors: the umbrella package, its dependencies,
        # and the services we call. `dontCheck` for the reason hackageNoCheck gives.
        amazonkaOverlay = hself: _hsuper:
          let
            fromMonorepo = name: subdir:
              hlib.dontCheck
                (hself.callCabal2nix name "${amazonkaSrc}/${subdir}" { });
          in {
            amazonka = fromMonorepo "amazonka" "lib/amazonka";
            # amazonka-core at this rev caps crypton below 1.2 and compiles against
            # 2.2. Drop the jailbreak when a rev admits crypton 2.2.
            amazonka-core =
              hlib.doJailbreak (fromMonorepo "amazonka-core" "lib/amazonka-core");
            amazonka-sso = fromMonorepo "amazonka-sso" "lib/services/amazonka-sso";
            amazonka-sts = fromMonorepo "amazonka-sts" "lib/services/amazonka-sts";
            amazonka-codeartifact =
              fromMonorepo "amazonka-codeartifact" "lib/services/amazonka-codeartifact";
            amazonka-sqs = fromMonorepo "amazonka-sqs" "lib/services/amazonka-sqs";
            amazonka-s3 = fromMonorepo "amazonka-s3" "lib/services/amazonka-s3";
          };

        # Fixes for HSEC-2026-0007 (JSON decoding DoS) and HSEC-2026-0008 (X.509 Name Constraints).
        # tls 2.4.11 needs hpke 0.3, x509 1.10, and crypton 2.1.9 or later.
        advisoryOverlay = hself: hsuper:
          let fromHackage = hackageNoCheck hself;
          in {
            aeson = fromHackage "aeson" "2.3.0.0"
              "sha256-nIUIE+wLCHTxhiKimKb1v8iTFxpQrzgyF8yY+27BrXY=";
            text-iso8601 = fromHackage "text-iso8601" "0.2.0.0"
              "sha256-EGwbQnjRumL2ztwXman6uEEvB0oRspEUB1tl0NyjAGI=";
            # 0.8.11 admits aeson 2.3.
            aeson-pretty = fromHackage "aeson-pretty" "0.8.11"
              "sha256-SmMKRymEU239HsYJL5oVh5eJpFqHoQdKTcw/jyWLeAU=";
            crypton-x509 = fromHackage "crypton-x509" "1.10.0"
              "sha256-1DPBXykSxOHBl1Eb/FTRy72Faw+bgZnDBAstgDwgxiY=";
            crypton-x509-store = fromHackage "crypton-x509-store" "1.10.0"
              "sha256-aPUxmoF6OQSq8xJyFfPQwG4+EAwoBZs3y6vxSERqVdI=";
            crypton-x509-system = fromHackage "crypton-x509-system" "1.10.0"
              "sha256-CweEELCYvhtGiodo7x0PK4lr8xxD6khrXsI5/ZLqHb8=";
            crypton-x509-validation = fromHackage "crypton-x509-validation" "1.10.0"
              "sha256-PTK9YMG+MGpHCnDDuIBKH2IzafLNJoK45iyTpoTSQo4=";
            tls = fromHackage "tls" "2.4.11"
              "sha256-X/BL0b3b+gNIn9Sy7fXce76jJ7JZlUPhUtsFEs8lYqc=";
            # Drop the jailbreak when crypton-connection admits x509 1.10. It also
            # strips the tls <2.4 cap that Hackage revision 1 already lifts.
            crypton-connection = hlib.doJailbreak
              (fromHackage "crypton-connection" "0.4.6"
                "sha256-B+fGwEBaChFx9mrrG5JbIjTnCSfiwuPetOOoua5FMGA=");
            # At least 2.2.1. From 2.1.8 to 2.2.0, crypton compiles all of its C with
            # AVX2 and BMI2 on x86-64, so the binary fails on older processors.
            crypton = fromHackage "crypton" "2.2.1"
              "sha256-GuTj0ChGkqAYER//1drYRnwEoozCBnJVn/31neRMPJg=";
            hpke = fromHackage "hpke" "0.3.0"
              "sha256-nOLaB+4hq66QKM5Fftn0QNRxDXjM57rB5oJQqPcYRD4=";
            http-client-tls = fromHackage "http-client-tls" "0.4.0"
              "sha256-OvQMk+ewLWN+zMrWc6hphID9DQ32FnasXNeMG/+A/OE=";
            # attoparsec-aeson 2.2.2.0 revision 1 is Hackage's own widening to
            # aeson <2.4. The set builds the tarball's original <2.3 file.
            attoparsec-aeson = hlib.overrideCabal hsuper.attoparsec-aeson (_drv: {
              revision = "1";
              editedCabalFile = "sha256-CJSPRbiSxXWNLELiL+L71BpPbcOV+wpDwr9Fih8pVzY=";
            });
            # No insert-ordered-containers or openapi3 release or revision admits aeson
            # 2.3 yet. Both compile against it, so strip the stale caps until upstream widens.
            insert-ordered-containers = hlib.doJailbreak hsuper.insert-ordered-containers;
            openapi3 = hlib.doJailbreak hsuper.openapi3;
          };

        # Upstream suites are not ours to run. CI builds these from source, and
        # noUpstreamSuites fails evaluation when one is missing.
        upstreamSuitesOverlay = _hself: hsuper:
          pkgs.lib.genAttrs [
            "attoparsec-aeson"
            "autodocodec"
            "autodocodec-openapi3"
            "binary-conduit"
            "bitvec"
            "bzlib-conduit"
            "cborg"
            "conduit"
            "conduit-extra"
            "crypton-socks"
            "hspec-wai"
            "http2"
            "insert-ordered-containers"
            "iproute"
            "katip"
            "libyaml"
            "microlens-aeson"
            "mono-traversable"
            "network-bsd"
            "openapi3"
            "regression-simple"
            "simple-sendfile"
            "streaming-commons"
            "tasty-bench-fit"
            "vector-algorithms"
            "wai-extra"
            "wai-logger"
            "warp"
            "xml-conduit"
            "yaml"
            "zip-stream"
          ] (name: hlib.dontCheck hsuper.${name});

        # nixpkgs passes haddock -j$NIX_BUILD_CORES, and GHC 9.10's haddock then
        # sometimes deadlocks in the RTS scheduler (GHC #21539). -j1 avoids it.
        sequentialHaddock = drv: hlib.overrideCabal drv (old: {
          haddockFlags = (old.haddockFlags or [ ]) ++ [ "--haddock-option=-j1" ];
        });

        # GHC 9.10's object code varies between builds under -j, so modules compile one at
        # a time. The nixpkgs option drops haddock's -j too, and the RTS flags restored here.
        sequentialCompile = drv: hlib.overrideCabal drv (old: {
          enableParallelBuilding = false;
          configureFlags = (old.configureFlags or [ ]) ++ [
            "--ghc-option=+RTS"
            "--ghc-option=-A64M"
            "--ghc-option=-RTS"
          ];
        });

        # Only libraries whose output differs from the base set's take the two
        # overrides, so no path cache.nixos.org serves changes.
        sequentialBuildOverlay = _hself: hsuper:
          builtins.mapAttrs (name: pkg:
            if (pkg.isHaskellLibrary or false) && builtFromSource name pkg
            then sequentialCompile (sequentialHaddock pkg)
            else pkg) hsuper;

        managedLzmaOverlay = hself: _hsuper: {
          ecluse-lzma-managed = hself.callCabal2nix "ecluse-lzma-managed" ./vendor/lzma-managed { };
        };

        hpkgs = pkgs.haskell.packages.ghc910.override {
          overrides = pkgs.lib.composeManyExtensions [
            otelOverlay
            transportOverlay
            amazonkaOverlay
            advisoryOverlay
            upstreamSuitesOverlay
            managedLzmaOverlay
            sequentialBuildOverlay
          ];
        };

        # The unmodified, binary-cached set, on the app's compiler. Tools not coupled to the
        # compiler are static, so no second library closure passes the CI cache cap.
        toolHpkgs = pkgs.haskell.packages.ghc910;

        # The build and CI artifact: ecluse.cabal against the pinned set, not Hackage. The
        # dev shell's cabal stays the inner loop, because Nix rebuilds the whole package.
        ecluseRaw = hpkgs.callCabal2nix "ecluse" ./. { };

        # Only for getCabalDeps, never built. cabal2nix omits benchmarks unless asked,
        # and the freeze must pin their dependencies too.
        ecluseForDeps =
          hpkgs.callCabal2nixWithOptions "ecluse" ./. "--benchmark" { };

        # Every dependency of every ecluse component as one GHC package environment, so
        # ghc-pkg reports the resolved closure, boot libraries included.
        ecluseDepEnv =
          let d = ecluseForDeps.getCabalDeps;
          in noUpstreamSuites [ ecluseForDeps ] (hpkgs.ghcWithPackages (_:
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
            ])));

        # The set's versions as cabal constraints, without flags, sublibraries or index-state.
        # `task freeze` regenerates it, and checks.freeze-sync catches drift.
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

        # Build every library and executable here. Tests run on the Cabal path,
        # because the integration and smoke suites need Docker or live registries.
        ecluse = let pkg = hlib.dontCheck ecluseRaw; in noUpstreamSuites [ pkg ] pkg;

        # Compile only exe:ecluse sequentially for the image. The whole-package
        # Haddock build above keeps parallel compilation and stays outside it.
        ecluseBinUnpruned = hlib.justStaticExecutables
          (hlib.setBuildTarget (sequentialCompile ecluse) "exe:ecluse");

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

        # The image's trust store. Écluse's TLS stack takes every certificate in
        # SSL_CERT_FILE as a trust anchor and ignores OpenSSL trust rules, so the
        # image carries cacert's plain bundle alone: the roots NSS trusts for TLS
        # servers. The blacklist drops the roots eMudhra's compromised intermediate
        # CAs chain to, and the other emSign roots Mozilla removed with them.
        # buildcatrust fails on a label NSS no longer carries, so drop the emSign
        # labels when cacert reaches NSS 3.131.1, which removes those roots itself.
        tlsCaBundle = pkgs.runCommand "tls-ca-bundle" { } ''
          install -D -m 0444 ${
            pkgs.cacert.override {
              blacklist = [
                "Comodo AAA Services root"
                "emSign ECC Root CA - G3"
                "emSign Root CA - C1"
                "emSign ECC Root CA - C3"
              ];
            }
          }/etc/ssl/certs/ca-no-trust-rules-bundle.crt $out/etc/ssl/certs/ca-bundle.crt
        '';

        # importNpmLock builds the node-semver oracle from its committed lock
        # and integrity hashes. Renovate excludes this reference data.
        # Refresh it through the fixture workflow. NODE_PATH exposes semver.
        oracleNodeModules = pkgs.importNpmLock.buildNodeModules {
          npmRoot = ./test/oracles;
          inherit (pkgs) nodejs;
        };

        cargoSemverOracle = pkgs.rustPlatform.buildRustPackage {
          pname = "cargo-semver-oracle";
          version = "0.1.0";
          src = ./test/oracles/cargo-semver;
          cargoLock.lockFile = ./test/oracles/cargo-semver/Cargo.lock;
          meta.license = pkgs.lib.licenses.mit;
        };

        # Remove when the registry publishes semgrep/semgrep-rules#4042.
        semgrepActionPinningSource =
          "https://raw.githubusercontent.com/nopcorn/semgrep-rules/734df2ec594a19c83b0694b9cf70feb7af012258/yaml/github-actions/security/github-actions-mutable-action-tag";

        # ---- Dev-shell composition -------------------------------------------
        shellEnv = {
          LANG = "C.UTF-8";
          LC_ALL = "C.UTF-8";
          NODE_PATH = "${oracleNodeModules}/node_modules";
          ECLTEST_RUBY = "${pkgs.ruby}/bin/ruby";
          ECLTEST_BUNDLER_269 = "${bundler269}/bin/bundle-2.6.9";
          ECLTEST_BUNDLER_4023 = "${bundler4023}/bin/bundle-4.0.23";
          AWS_EC2_METADATA_DISABLED = "true";
          ECLTEST_SEMGREP_ACTION_RULE = pkgs.fetchurl {
            url = "${semgrepActionPinningSource}.yaml";
            hash = "sha256-VxpbQCJKLOcb8IJA3qE3VT9qXy9U3WthoMl0d7lGil8=";
          };
          ECLTEST_SEMGREP_ACTION_TEST = pkgs.fetchurl {
            url = "${semgrepActionPinningSource}.test.yaml";
            hash = "sha256-I7rd0RSbR4JDW+jYiiJ7Ec3Zq0bUVZD0NJ5CGE7TigY=";
          };
          # GHC loads archive libraries while evaluating compile-time splices.
          LD_LIBRARY_PATH = pkgs.lib.makeLibraryPath [ pkgs.zlib pkgs.bzip2 pkgs.xz ];
        };

        # CI shares one closure: toolchain, gate, ops, and bench.
        # The default shell adds IDE tools. Keep each tool in one tier.

        # Official 2023-11-16 release manifest pins the minimum Cargo client.
        # The current pinned rustc compiles owned fixtures for both clients.
        cargoFloorTarget = {
          x86_64-linux = {
            target = "x86_64-unknown-linux-gnu";
            sha256 = "f219386d4569c40b660518e99267afff428c13bf980bda7a614c8d4038d013f6";
          };
          aarch64-linux = {
            target = "aarch64-unknown-linux-gnu";
            sha256 = "a18dc9132cf76ccba90bcbb53b56a4d37ebfb34845f61e79f7b5d4710a269647";
          };
          x86_64-darwin = {
            target = "x86_64-apple-darwin";
            sha256 = "5c1c4f5985a48ad02bcff881c5a9c983218bc1eefc083403579147a3292ba073";
          };
          aarch64-darwin = {
            target = "aarch64-apple-darwin";
            sha256 = "5c14e9b3a458d728d89e02f4e024b710d5b0eb8c45249066fe666d2094fbf233";
          };
        }.${system};
        cargoFloor = pkgs.stdenv.mkDerivation {
          pname = "cargo-client-floor";
          version = "1.74.0";
          src = pkgs.fetchurl {
            url = "https://static.rust-lang.org/dist/2023-11-16/cargo-1.74.0-${cargoFloorTarget.target}.tar.xz";
            inherit (cargoFloorTarget) sha256;
          };
          nativeBuildInputs = pkgs.lib.optionals pkgs.stdenv.isLinux [ pkgs.autoPatchelfHook ];
          buildInputs = pkgs.lib.optionals pkgs.stdenv.isLinux [
            pkgs.stdenv.cc.cc.lib
            pkgs.curl
            pkgs.openssl
            pkgs.zlib
          ];
          dontConfigure = true;
          dontBuild = true;
          dontStrip = true;
          installPhase = ''
            runHook preInstall
            install -Dm755 cargo/bin/cargo "$out/bin/cargo-1.74"
            runHook postInstall
          '';
          meta.license = [ pkgs.lib.licenses.mit pkgs.lib.licenses.asl20 ];
        };

        # Both Bundler clients use the pinned Ruby runtime without a registry install.
        bundlerClient = version: hash: pkgs.stdenvNoCC.mkDerivation {
          pname = "bundler-client";
          inherit version;
          src = pkgs.fetchurl {
            url = "https://rubygems.org/gems/bundler-${version}.gem";
            inherit hash;
          };
          nativeBuildInputs = [ pkgs.ruby pkgs.makeWrapper ];
          dontUnpack = true;
          dontConfigure = true;
          dontBuild = true;
          installPhase = ''
            runHook preInstall
            cp "$src" "$TMPDIR/bundler-${version}.gem"
            gem install --local "$TMPDIR/bundler-${version}.gem" --no-document \
              --install-dir "$out/gems" --bindir "$TMPDIR/bundler-bin"
            mkdir -p "$out/bin"
            makeWrapper ${pkgs.ruby}/bin/ruby "$out/bin/bundle-${version}" \
              --set GEM_PATH "$out/gems" \
              --set BUNDLE_VERSION "${version}" \
              --add-flags "-I$out/gems/gems/bundler-${version}/lib" \
              --add-flags "$out/gems/gems/bundler-${version}/exe/bundle"
            ln -s "bundle-${version}" "$out/bin/bundler-${version}"
            runHook postInstall
          '';
          meta.license = pkgs.lib.licenses.mit;
        };
        bundler269 = bundlerClient "2.6.9" "sha256-olZ1/70FWuEYZ2bMHhILTPYliOiKu1m5nFfiKxxVyes=";
        bundler4023 = bundlerClient "4.0.23" "sha256-QHuoodko9BKEf4Gapfi2rvfFn7Uf61FxEq+KFlSHKHs=";

        # Compiling and testing share this toolchain in development and CI.
        toolchainInputs = [
          pkgs.bashInteractive
          toolHpkgs.ghc
          pkgs.cabal-install
          pkgs.zlib
          pkgs.bzip2
          pkgs.xz
          pkgs.pkg-config
          # Reference version-ordering oracles for the differential smoke suite
          # and `task gen-version-fixtures`. They are node-semver (npm, built by
          # oracleNodeModules and put on NODE_PATH above), Python packaging (PyPI),
          # Ruby Gem::Version, and the locked Rust semver precedence oracle.
          pkgs.nodejs
          pkgs.cargo
          pkgs.rustc
          pkgs.stdenv.cc
          cargoFloor
          cargoSemverOracle
          # pip rides along because the end-to-end tier drives it as a real client
          # against a pypi mount, the way it drives npm against an npm mount.
          (pkgs.python3.withPackages (ps: [ ps.packaging ps.pip ]))
          pkgs.ruby
          bundler269
          bundler4023
          pkgs.go-task
          pkgs.flock
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
          (hlib.justStaticExecutables (hlib.dontCheck toolHpkgs.hpc-codecov))
          pkgs.semgrep
          pkgs.actionlint
          pkgs.zizmor
          # yq (the Go yq-go) turns each workflow into JSON for the runner-policy
          # check in `task lint-workflows` (scripts/ci-runner-policy.sh).
          pkgs.yq-go
          # shellcheck for `task lint-scripts` (scripts/*.sh). actionlint already
          # runs shellcheck on workflow `run:` blocks. This lints the committed
          # scripts too.
          pkgs.shellcheck
          # Dead-code (weeder) and HIE-based static analysis (stan). Both gate CI
          # through their own jobs, and both are in `task check`.
          (hlib.justStaticExecutables (hlib.dontCheck toolHpkgs.weeder))
          (hlib.justStaticExecutables (hlib.dontCheck toolHpkgs.stan))
          # Zola builds the static site under web/, and pagefind indexes the built
          # output into the search bundle. Both the publish and the site gate run them.
          pkgs.zola
          pkgs.pagefind
          saerskriven.packages.${system}.saerskriven
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

        # skopeo converts release archives without a Docker daemon. sbomnix
        # records static Haskell dependencies that image scanners cannot see.
        # grype owns C-library scanning, vulnix cross-checks Nix patches, and
        # osv-scanner reads HSEC advisories against cabal.project.freeze.
        # See docs/architecture/release-supply-chain.md for the scan contracts.
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
          # openssl: `scripts/image-archive.sh trust-store` fingerprints each
          # certificate in the image's CA bundles with it, in the release build.
          pkgs.openssl
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
          (hlib.justStaticExecutables (hlib.dontCheck toolHpkgs.ghc-prof-flamegraph))
          pkgs.flamegraph
          pkgs.oha
        ];

        # agent-lsp drives HLS through MCP. mcp-language-server v0.1.1
        # leaves this project's HLS requests blocked, so it cannot replace it.
        # The tagged source needs Go 1.26 and uses no cgo. Keep it in the IDE
        # tier beside HLS's matching GHC and hspec-discover.
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

          # The image contains the static binary's runtime closure and CA roots.
          # tag = null gives local builds a content-hash tag. Releases retag it
          # for the immutable registry, then publish the multi-arch index and
          # attestations through release.yml.
          dockerImage = pkgs.dockerTools.buildLayeredImage {
            name = "ecluse";
            tag = null;
            contents = [ ecluseBin tlsCaBundle ];
            config = {
              Entrypoint = [ "${ecluseBin}/bin/ecluse" ];
              ExposedPorts = { "8080/tcp" = { }; }; # default ECLUSE_SERVER__PORT
              User = "65532:65532"; # nonroot, distroless convention
              Env = [
                # A distroless image has no system trust store, so tls/x509-system
                # finds no CAs and every outbound HTTPS fetch fails. Point GHC's TLS
                # stack at the bundle `contents` puts in the closure. The release build
                # checks it with `scripts/image-archive.sh trust-store`.
                "SSL_CERT_FILE=${tlsCaBundle}/etc/ssl/certs/ca-bundle.crt"
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

        # The docs job builds these checks. Nix reuses dependency Haddock
        # interfaces, avoiding cabal's separate documentation closure build.
        # Use Nix checks where its store or evaluated inputs are required.
        # doHaddock forces documentation generation. dontCheck skips tests.
        checks.docs = sequentialHaddock (hlib.doHaddock ecluse);

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
        datadog = pkgs.mkShell {
          name = "ecluse-datadog-example";
          packages = [ pkgs.go-task pkgs.opentofu ];
        };

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
