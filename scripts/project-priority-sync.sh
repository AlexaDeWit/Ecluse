#!/usr/bin/env bash
#
# Mirror an issue's priority:* label onto the "Priority" single-select field of the
# Ecluse Bug Triage board (user project AlexaDeWit/#2). Labels are the source of
# truth; this drives the board field one-directionally so the Priority swimlanes
# track the labels with no manual sidebar edits. Invoked from
# .github/workflows/project-priority-sync.yml on issues: [labeled, unlabeled].
#
# Severity precedence (highest wins if more than one is somehow present):
#   priority:critical -> Critical, priority:normal -> Normal, priority:nit -> Nit.
# No priority:* label -> the field is cleared (labels stay authoritative).
# An issue not on the board is a no-op (only bugs are auto-added there).
#
# Env: GH_TOKEN = a PAT with `project` scope — the default GITHUB_TOKEN can neither
# read nor write user-owned Projects v2. REPO / ISSUE_NUMBER / ISSUE_NODE_ID come
# from the workflow. The only writes are to the project field; the repo is read-only.
set -euo pipefail

owner="AlexaDeWit"
project_number=2
field_name="Priority"

: "${GH_TOKEN:?need a PAT with project scope}"
: "${REPO:?}"
: "${ISSUE_NUMBER:?}"
: "${ISSUE_NODE_ID:?}"

# 1. Resolve the project id, the Priority field id, and its option name->id map once.
proj="$(
  gh api graphql -f query='
    query($login:String!, $num:Int!, $field:String!) {
      user(login:$login) {
        projectV2(number:$num) {
          id
          field(name:$field) { ... on ProjectV2SingleSelectField { id options { id name } } }
        }
      }
    }' -F login="$owner" -F num="$project_number" -F field="$field_name" \
    --jq '.data.user.projectV2'
)"
project_id="$(jq -r '.id' <<<"$proj")"
field_id="$(jq -r '.field.id' <<<"$proj")"

# 2. Effective priority from the issue's CURRENT labels (authoritative — covers
#    add, remove, and the case of more than one priority:* label present).
priority_labels="$(
  gh issue view "$ISSUE_NUMBER" --repo "$REPO" --json labels \
    --jq '[.labels[].name | select(startswith("priority:"))] | join(" ")'
)"
case " $priority_labels " in
  *" priority:critical "*) want="Critical" ;;
  *" priority:normal "*) want="Normal" ;;
  *" priority:nit "*) want="Nit" ;;
  *) want="" ;;
esac

# 3. Find this issue's item on the board (no-op if it isn't tracked there).
item_id="$(
  gh api graphql -f query='
    query($id:ID!) {
      node(id:$id) { ... on Issue { projectItems(first:50) { nodes { id project { id } } } } }
    }' -F id="$ISSUE_NODE_ID" --jq '.data.node.projectItems.nodes[]' \
    | jq -r --arg p "$project_id" 'select(.project.id==$p) | .id' | head -n1
)"
if [ -z "$item_id" ]; then
  echo "issue #$ISSUE_NUMBER not on $owner project #$project_number — nothing to sync"
  exit 0
fi

# 4. No priority:* label -> clear the field; otherwise set the matching option.
if [ -z "$want" ]; then
  gh api graphql -f query='
    mutation($p:ID!, $i:ID!, $f:ID!) {
      clearProjectV2ItemFieldValue(input:{projectId:$p, itemId:$i, fieldId:$f}) { clientMutationId }
    }' -F p="$project_id" -F i="$item_id" -F f="$field_id" >/dev/null
  echo "issue #$ISSUE_NUMBER: no priority:* label -> Priority cleared"
  exit 0
fi

option_id="$(jq -r --arg n "$want" '.field.options[] | select(.name==$n) | .id' <<<"$proj")"
if [ -z "$option_id" ]; then
  echo "Priority field has no '$want' option — board options changed?" >&2
  exit 1
fi

gh api graphql -f query='
  mutation($p:ID!, $i:ID!, $f:ID!, $o:String!) {
    updateProjectV2ItemFieldValue(
      input:{projectId:$p, itemId:$i, fieldId:$f, value:{singleSelectOptionId:$o}}
    ) { projectV2Item { id } }
  }' -F p="$project_id" -F i="$item_id" -F f="$field_id" -F o="$option_id" >/dev/null
echo "issue #$ISSUE_NUMBER: Priority -> $want"
