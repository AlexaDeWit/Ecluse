#!/usr/bin/env bash
# A cabal change that can reach the release image must run the dry-run. Run via `task test-scripts`.
set -euo pipefail

here="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
script="$here/ci-cabal-diff.sh"

work="$(mktemp -d)"
trap 'rm -rf "$work"' EXIT
cat > "$work/base.cabal" <<'CABAL'
cabal-version: 3.0
name:          demo
version:       0.1.0

flag dev
  description: A test-only flag.
  default:     False
  manual:      True

common shared
  ghc-options: -Wall
  build-depends:
    , base ^>=4.20

library
  import:          shared
  hs-source-dirs:  src
  exposed-modules:
    Demo
    Demo.Core
  if flag(dev)
    exposed-modules: Demo.Dev
  build-depends:
    , text ^>=2.1

executable demo
  import:         shared
  main-is:        Main.hs
  hs-source-dirs: app
  build-depends:  demo

test-suite demo-doc
  type:           exitcode-stdio-1.0
  main-is:        Doc.hs
  hs-source-dirs: doc

-- Unit tests.
test-suite demo-unit
  import:         shared
  type:           exitcode-stdio-1.0
  main-is:        Spec.hs
  hs-source-dirs: test
  other-modules:
    DemoSpec
  if flag(dev)
    other-modules: DemoDevSpec
  build-depends:
    , demo
    , hspec ^>=2.11

test-suite demo-e2e
  if !flag(dev)
    buildable: False
  type:           exitcode-stdio-1.0
  main-is:        E2E.hs
  hs-source-dirs: e2e
  build-depends:
    , demo

benchmark demo-bench
  import:         shared
  type:           exitcode-stdio-1.0
  main-is:        Bench.hs
  hs-source-dirs: bench
  build-depends:
    , demo
CABAL

fail=0

# Print the diff of one sed edit of the base file, as the classifier runs it. $1 context, $2 edit.
edit_diff() {
  sed -e "$2" "$work/base.cabal" > "$work/new.cabal"
  ! cmp -s "$work/base.cabal" "$work/new.cabal" || return 1
  git diff --no-index --no-color --no-ext-diff --no-textconv --unified="$1" \
    "$work/base.cabal" "$work/new.cabal" || [ $? -eq 1 ]
}

# Assert the verdict on $work/diff. $1 name, $2 expected exit.
check() {
  local got=0
  bash "$script" < "$work/diff" >/dev/null 2>&1 || got=$?
  if [ "$got" = "$2" ]; then
    printf 'ok   - %s\n' "$1"
  else
    printf 'FAIL - %s (want exit %s, got %s)\n' "$1" "$2" "$got"
    fail=1
  fi
}

# Assert the verdict on one edit of the base file. $1 name, $2 expected exit, $3 sed script.
check_edit() {
  if edit_diff 1000000 "$3" > "$work/diff"; then
    check "$1" "$2"
  else
    printf 'FAIL - %s (the edit changed nothing)\n' "$1"
    fail=1
  fi
}

module_list='/^    Demo.Core$/a\    Demo.Extra
/^    DemoSpec$/a\    Demo.ExtraSpec'

unit='/^test-suite demo-unit$/,/^test-suite demo-e2e$/'

check_edit "a module-list edit skips the dry-run" 0 "$module_list"
check_edit "a module list under a conditional skips the dry-run" 0 \
  's/exposed-modules: Demo.Dev$/exposed-modules: Demo.Dev Demo.Dev.Http/'
check_edit "a test-suite field that names no dependency skips the dry-run" 0 \
  's/^  main-is:        Spec\.hs$/  main-is:        Main.hs/'
check_edit "a benchmark stanza edit skips the dry-run" 0 \
  '/^  hs-source-dirs: bench$/a\  ghc-options: -O2'
check_edit "a comment in a library's build-depends skips the dry-run" 0 \
  '/^    , text ^>=2.1$/a\  -- Text handling.'
check_edit "a test-suite build-depends edit runs the dry-run" 1 \
  '/^    , hspec/a\    , tls ^>=2.1'
check_edit "a test-suite build-tool-depends edit runs the dry-run" 1 \
  "$unit"' s/^  hs-source-dirs: test$/&\n  build-tool-depends: hspec-discover:hspec-discover/'
check_edit "a test-suite build-tools edit runs the dry-run" 1 \
  "$unit"' s/^  hs-source-dirs: test$/&\n  build-tools: alex/'
check_edit "a test-suite pkgconfig-depends edit runs the dry-run" 1 \
  "$unit"' s/^  hs-source-dirs: test$/&\n  pkgconfig-depends: zlib/'
check_edit "a test-suite extra-libraries edit runs the dry-run" 1 \
  "$unit"' s/^  hs-source-dirs: test$/&\n  extra-libraries: z/'
check_edit "a test-suite frameworks edit runs the dry-run" 1 \
  "$unit"' s/^  hs-source-dirs: test$/&\n  frameworks: Foundation/'
check_edit "a deleted buildable: False in a test-suite runs the dry-run" 1 \
  '/^    buildable: False$/d'
check_edit "a buildable: False turned True in a test-suite runs the dry-run" 1 \
  's/^    buildable: False$/    buildable: True/'
check_edit "a new test-suite header that takes over another's dependencies runs the dry-run" 1 \
  '/^  hs-source-dirs: e2e$/a\
test-suite demo-e2e-deps\
  type:           exitcode-stdio-1.0\
  main-is:        Deps.hs'
check_edit "a test-suite import edit runs the dry-run" 1 \
  "$unit"' s/^  import:         shared$/&\n  import:         extra/'
check_edit "a test-suite conditional edit runs the dry-run" 1 \
  "$unit"' s/^  if flag(dev)$/  if !flag(dev)/'
check_edit "a misplaced line in a test-suite runs the dry-run" 1 's/^    DemoSpec$/  DemoSpec/'
check_edit "a library build-depends edit runs the dry-run" 1 \
  '/^    , text/a\    , containers ^>=0.7'
check_edit "an executable stanza edit runs the dry-run" 1 's/Main\.hs$/Server.hs/'
check_edit "a ghc-options edit runs the dry-run" 1 \
  's/^  ghc-options: -Wall$/  ghc-options: -Wall -O2/'
check_edit "a flag edit runs the dry-run" 1 's/^  default:     False$/  default:     True/'
check_edit "a version edit runs the dry-run" 1 's/^version:       0\.1\.0$/version:       0.2.0/'
check_edit "a module-list header that takes over a dependency runs the dry-run" 1 \
  '/^    , text/i\  other-modules:'
check_edit "a module-list header under hs-source-dirs runs the dry-run" 1 \
  's/^  hs-source-dirs: app$/&\n    other-modules:\n      src/'
check_edit "a module-list header indented under the field above runs the dry-run" 1 \
  's/^  exposed-modules:$/   exposed-modules:/'
check_edit "a module-list header indented out of its stanza's column runs the dry-run" 1 \
  's/^  exposed-modules:$/ exposed-modules:/'
check_edit "a removed test-suite header that gives its fields to an executable runs the dry-run" 1 \
  '/^test-suite demo-doc$/d'
check_edit "a line outside its field's indentation runs the dry-run" 1 \
  's/^    Demo.Core$/  Demo.Core/'
check_edit "a carriage return inside a comment runs the dry-run" 1 \
  's/^    , text ^>=2.1$/&\n  -- crypto pins\r    , unix/'
check_edit "a tab in a module list runs the dry-run" 1 's/^    Demo.Core$/    Demo.Core\t/'
check_edit "a brace in a module list runs the dry-run" 1 's/^    Demo.Core$/    Demo.Core }/'

printf 'not a diff\n' > "$work/diff"
check "a malformed diff runs the dry-run" 1

: > "$work/diff"
check "an empty diff runs the dry-run" 1

edit_diff 3 '/^  exposed-modules:$/a\    Demo.First' > "$work/diff"
check "a diff without whole-file context runs the dry-run" 1

edit_diff 1000000 "$module_list" | head -n -1 > "$work/diff"
check "a truncated diff runs the dry-run" 1

exit "$fail"
