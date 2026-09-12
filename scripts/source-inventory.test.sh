#!/usr/bin/env bash
# Exercise selection, argument boundaries, and task invalidation in a temporary checkout.
set -euo pipefail

here="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
mkdir -p "$here/../scratchpad"
temporary="$(mktemp -d "$here/../scratchpad/source-inventory.XXXXXX")"
trap 'rm -rf "$temporary"' EXIT
mkdir -p "$temporary/scripts" "$temporary/bin"
cp "$here/source-inventory.sh" "$temporary/scripts/"
cp "$here/../Taskfile.yml" "$temporary/"
cp "$here/../.gitignore" "$temporary/"
cd "$temporary"
git init -q

expected_hs=(app/Main.hs bench/Bench.hs core/src/Core.hs core/test/CoreSpec.hs runtime/src/Runtime.hs runtime/test/RuntimeSpec.hs src/App.hs test/AppSpec.hs tools/generator/Gen.hs web/site-gen/Site.hs 'src/With Space.hs')
expected_sh=(scripts/source-inventory.sh scripts/check.sh 'web/With Space.sh')
for path in "${expected_hs[@]}" "${expected_sh[@]}"; do
  mkdir -p "$(dirname "$path")"
  [[ -e "$path" ]] || printf 'fixture\n' > "$path"
done
git add .gitignore app scripts
for directory in scratchpad .agents/worktrees/nested .claude/worktrees/nested dist-newstyle dist-custom coverage _site web/generated web/public; do
  mkdir -p "$directory"
  touch "$directory/Excluded.hs" "$directory/excluded.sh"
done
mkdir -p src/nested
# An unignored nested checkout must also stop discovery.
git -C src/nested init -q
touch src/nested/Nested.hs src/nested/nested.sh
git -C src/nested worktree add -q --orphan ../linked-worktree
touch src/linked-worktree/Linked.hs src/linked-worktree/linked.sh
ln -s ../scratchpad/Excluded.hs src/Linked.hs

cat > bin/record <<'STUB'
#!/usr/bin/env bash
set -euo pipefail
printf 'call\n' >> "$RECORD_CALLS"
for argument in "$@"; do
  case "$argument" in
    *.hs | *.sh) printf '%s\0' "$argument" >> "$RECORD_ARGUMENTS" ;;
  esac
done
STUB
chmod +x bin/record
for tool in fourmolu hlint shellcheck; do
  ln -s record "bin/$tool"
done
export PATH="$temporary/bin:$PATH"
export RECORD_CALLS="$temporary/calls" RECORD_ARGUMENTS="$temporary/arguments"

# Compare NUL records so a spaced path cannot pass as two tool arguments.
check_arguments() {
  printf '%s\0' "$@" | sort -z > expected
  sort -z "$RECORD_ARGUMENTS" > actual
  cmp expected actual
}

for target in format format-check lint lint-scripts; do
  : > "$RECORD_CALLS"
  : > "$RECORD_ARGUMENTS"
  task "$target"
  if [[ "$target" == lint-scripts ]]; then
    check_arguments "${expected_sh[@]}"
    source_path='web/With Space.sh'
    added_path=scripts/Added.sh
  else
    check_arguments "${expected_hs[@]}"
    source_path='src/With Space.hs'
    added_path=src/Added.hs
  fi
  [[ "$target" != format ]] || continue
  task "$target"
  [[ $(wc -l < "$RECORD_CALLS") -eq 1 ]]
  printf 'ignored edit\n' >> scratchpad/Excluded.hs
  printf 'ignored edit\n' >> scratchpad/excluded.sh
  printf 'nested edit\n' >> src/nested/Nested.hs
  printf 'nested edit\n' >> src/nested/nested.sh
  printf 'worktree edit\n' >> src/linked-worktree/Linked.hs
  printf 'worktree edit\n' >> src/linked-worktree/linked.sh
  task "$target"
  [[ $(wc -l < "$RECORD_CALLS") -eq 1 ]]
  printf 'source edit\n' >> "$source_path"
  task "$target"
  [[ $(wc -l < "$RECORD_CALLS") -eq 2 ]]
  printf 'unstaged addition\n' > "$added_path"
  task "$target"
  [[ $(wc -l < "$RECORD_CALLS") -eq 3 ]]
  rm "$added_path"
  task "$target"
  [[ $(wc -l < "$RECORD_CALLS") -eq 4 ]]
done
printf 'source inventory and task invalidation checks passed\n'
