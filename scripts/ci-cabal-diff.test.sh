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

-- Unit tests.
test-suite demo-unit
  import:         shared
  type:           exitcode-stdio-1.0
  main-is:        Spec.hs
  hs-source-dirs: test
  other-modules:
    DemoSpec
  build-depends:
    , demo
    , hspec ^>=2.11

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

check_edit "a module-list edit skips the dry-run" 0 "$module_list"
check_edit "a module list under a conditional skips the dry-run" 0 \
  's/exposed-modules: Demo.Dev$/exposed-modules: Demo.Dev Demo.Dev.Http/'
check_edit "a test-suite build-depends edit skips the dry-run" 0 \
  '/^    , hspec/a\    , tls ^>=2.1'
check_edit "a benchmark stanza edit skips the dry-run" 0 \
  '/^  hs-source-dirs: bench$/a\  ghc-options: -O2'
check_edit "a comment edit skips the dry-run" 0 \
  's/^-- Unit tests\.$/-- Unit tests, pure and fast./'
check_edit "a library build-depends edit runs the dry-run" 1 \
  '/^    , text/a\    , containers ^>=0.7'
check_edit "an executable stanza edit runs the dry-run" 1 's/Main\.hs$/Server.hs/'
check_edit "a ghc-options edit runs the dry-run" 1 \
  's/^  ghc-options: -Wall$/  ghc-options: -Wall -O2/'
check_edit "a flag edit runs the dry-run" 1 's/^  default:     False$/  default:     True/'
check_edit "a version edit runs the dry-run" 1 's/^version:       0\.1\.0$/version:       0.2.0/'
check_edit "a module-list header that takes over a dependency runs the dry-run" 1 \
  '/^    , text/i\  other-modules:'
check_edit "a line outside its field's indentation runs the dry-run" 1 \
  's/^    Demo.Core$/  Demo.Core/'
check_edit "a tab in a module list runs the dry-run" 1 's/^    Demo.Core$/    Demo.Core\t/'

printf 'not a diff\n' > "$work/diff"
check "a malformed diff runs the dry-run" 1

: > "$work/diff"
check "an empty diff runs the dry-run" 1

edit_diff 3 '/^  exposed-modules:$/a\    Demo.First' > "$work/diff"
check "a diff without whole-file context runs the dry-run" 1

edit_diff 1000000 "$module_list" | head -n -1 > "$work/diff"
check "a truncated diff runs the dry-run" 1

exit "$fail"
