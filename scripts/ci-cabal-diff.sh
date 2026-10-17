#!/usr/bin/env bash
# Decide whether a full-context `git diff` of ecluse.cabal, read on stdin, can change the release
# image. Exit 0 when every changed line is a comment, a blank line, a module-list entry, or a
# field of a test-suite or benchmark stanza that names no dependency. Exit 1 on any other change,
# and on input this script cannot parse. The image build compiles no test or benchmark, but
# evaluating it resolves the names they depend on. e2e-prepare builds the arm64 image, which
# covers a module-list change.
set -euo pipefail
export LC_ALL=C

declare -A kind=([old]="" [new]="") stanza=([old]="" [new]="") field=([old]="" [new]="")
declare -A indent=([old]=0 [new]=0) line=([old]=0 [new]=0)
changed=0

reaches() {
  echo "cabal: $1"
  exit 1
}

# Cabal ignores both, so neither moves a field or changes the build.
blank_or_comment() {
  [[ "$1" =~ ^[[:space:]]*(--.*)?$ ]]
}

# Update one side's stanza and field from the next line of that side's file.
track() {
  local side="$1" text="$2" lead
  line[$side]=$((line[$side] + 1))
  [[ "$text" != *[[:cntrl:]]* ]] ||
    reaches "${side} line ${line[$side]} has a control character, which Cabal may read as a newline"
  ! blank_or_comment "$text" || return 0
  [[ "$text" != *[{}]* ]] || reaches "${side} line ${line[$side]} has a brace, which is not parsed"
  lead="${text%%[^ ]*}"
  # Cabal reads any line deeper than the current field as part of it, whatever the line says.
  if [ -n "$lead" ] && [ -n "${field[$side]}" ] && [ "${field[$side]}" != conditional ] &&
    ((${#lead} > indent[$side])); then
    return 0
  fi
  if [[ "$text" =~ ^([A-Za-z][A-Za-z0-9_-]*)[[:space:]]*: ]]; then
    kind[$side]=package
    stanza[$side]=package
    field[$side]="${BASH_REMATCH[1],,}"
  elif [ -z "$lead" ]; then
    stanza[$side]="$text"
    kind[$side]="${text%%[[:space:]]*}"
    kind[$side]="${kind[$side],,}"
    field[$side]=""
  elif [[ "$text" =~ ^\ +([A-Za-z][A-Za-z0-9_-]*)[[:space:]]*: ]]; then
    field[$side]="${BASH_REMATCH[1],,}"
  elif [[ "$text" =~ ^\ +(if|elif|else)([[:space:]]|\(|$) ]]; then
    field[$side]=conditional
  else
    field[$side]=unknown
  fi
  indent[$side]=${#lead}
}

# Succeed when a changed line, already tracked, cannot change the release image.
harmless() {
  local side="$1" text="$2"
  ! blank_or_comment "$text" || return 0
  case "${field[$side]}" in
    exposed-modules | other-modules) return 0 ;;
    # Evaluating the image resolves every dependency name, a test's or a benchmark's included.
    build-depends | build-tool-depends | build-tools | pkgconfig-depends) return 1 ;;
    extra-libraries | frameworks | import | conditional | unknown) return 1 ;;
  esac
  case "${kind[$side]}" in test-suite | benchmark) return 0 ;; esac
  return 1
}

require() {
  local where="${stanza[$1]:-no stanza}, ${field[$1]:-no field}"
  harmless "$1" "$2" || reaches "${1} line ${line[$1]} (${where}) reaches the image: ${2}"
}

change() {
  track "$1" "$2"
  changed=$((changed + 1))
  require "$1" "$2"
}

# An unchanged line under an added or removed header has a new meaning, so it counts as changed.
context() {
  track old "$1"
  track new "$1"
  [ "${kind[old]} ${field[old]}" != "${kind[new]} ${field[new]}" ] || return 0
  require old "$1"
  require new "$1"
}

hunk=""
while IFS= read -r row || [ -n "$row" ]; do
  if [ -z "$hunk" ]; then
    case "$row" in
      "diff --git "* | "index "* | "--- "* | "+++ "*) continue ;;
    esac
    # Only one hunk from line 1 shows every stanza header and field name a change sits under.
    [[ "$row" =~ ^@@\ -1(,([0-9]+))?\ \+1(,([0-9]+))?\ @@ ]] ||
      reaches "the diff does not open with one whole-file hunk: ${row}"
    hunk="${BASH_REMATCH[2]:-1} ${BASH_REMATCH[4]:-1}"
    continue
  fi
  case "$row" in
    " "*) context "${row:1}" ;;
    "-"*) change old "${row:1}" ;;
    "+"*) change new "${row:1}" ;;
    "\\"*) ;;
    *) reaches "unexpected line in the hunk: ${row}" ;;
  esac
done

[ -n "$hunk" ] || reaches "no diff to read"
[ "$hunk" = "${line[old]} ${line[new]}" ] ||
  reaches "the hunk counts ${hunk} do not match the lines read (${line[old]} ${line[new]})"
echo "cabal: each of the ${changed} changed lines is a comment, a blank line, a module-list entry,"
echo "cabal: or a test-suite or benchmark field that names no dependency"
