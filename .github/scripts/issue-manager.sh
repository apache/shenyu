#!/usr/bin/env bash
set -euo pipefail

# Minimal issue manager script to handle a subset of prow-style commands
# Reads the GitHub issue_comment event JSON pointed to by GITHUB_EVENT_PATH
# Requires GITHUB_TOKEN env and GITHUB_REPOSITORY env (owner/repo)

GITHUB_API=${GITHUB_API:-https://api.github.com}

if [ -z "${GITHUB_TOKEN:-}" ]; then
  echo "GITHUB_TOKEN is required" >&2
  exit 1
fi

if [ -z "${GITHUB_EVENT_PATH:-}" ] || [ ! -f "${GITHUB_EVENT_PATH}" ]; then
  echo "GITHUB_EVENT_PATH not set or file missing: ${GITHUB_EVENT_PATH}" >&2
  exit 1
fi

if [ -z "${GITHUB_REPOSITORY:-}" ]; then
  echo "GITHUB_REPOSITORY is required (owner/repo)" >&2
  exit 1
fi

read_event() {
  jq -r "$1" "${GITHUB_EVENT_PATH}"
}

COMMENT_BODY=$(read_event '.comment.body')
ISSUE_NUMBER=$(read_event '.issue.number')

OWNER=$(echo "${GITHUB_REPOSITORY}" | cut -d/ -f1)
REPO=$(echo "${GITHUB_REPOSITORY}" | cut -d/ -f2)

auth_header() {
  echo "Authorization: token ${GITHUB_TOKEN}"
}

api() {
  local method=$1 path=$2 data=${3:-}
  if [ -n "${data}" ]; then
    curl -sS -X "${method}" -H "$(auth_header)" -H "Accept: application/vnd.github+json" \
      -H "Content-Type: application/json" --data "${data}" "${GITHUB_API}${path}"
  else
    curl -sS -X "${method}" -H "$(auth_header)" -H "Accept: application/vnd.github+json" "${GITHUB_API}${path}"
  fi
}

users_json_from_bash_array() {
  # read users from args and output compact JSON array like ["user1","user2"]
  if [ $# -eq 0 ]; then
    echo '[]'
    return
  fi
  printf "%s\n" "$@" | sed 's/^@//' | jq -R -s -c 'split("\n") | map(select(length>0))'
}

add_label() {
  local label=$1
  api POST "/repos/${OWNER}/${REPO}/issues/${ISSUE_NUMBER}/labels" "$(jq -nc --arg l "$label" '{labels:[$l]}')" >/dev/null || true
}

remove_label() {
  local label=$1
  encoded=$(printf "%s" "$label" | jq -s -R -r @uri)
  api DELETE "/repos/${OWNER}/${REPO}/issues/${ISSUE_NUMBER}/labels/${encoded}" >/dev/null || true
}

assign() {
  local users_json
  users_json=$(users_json_from_bash_array "$@")
  api POST "/repos/${OWNER}/${REPO}/issues/${ISSUE_NUMBER}/assignees" "$(jq -nc --argjson a "$users_json" '{assignees:$a}')" >/dev/null || true
}

unassign() {
  local users_json
  users_json=$(users_json_from_bash_array "$@")
  api DELETE "/repos/${OWNER}/${REPO}/issues/${ISSUE_NUMBER}/assignees" "$(jq -nc --argjson a "$users_json" '{assignees:$a}')" >/dev/null || true
}

close_issue() {
  api PATCH "/repos/${OWNER}/${REPO}/issues/${ISSUE_NUMBER}" '{"state":"closed"}' >/dev/null || true
}

reopen_issue() {
  api PATCH "/repos/${OWNER}/${REPO}/issues/${ISSUE_NUMBER}" '{"state":"open"}' >/dev/null || true
}

lock_issue() {
  api PUT "/repos/${OWNER}/${REPO}/issues/${ISSUE_NUMBER}/lock" '{"lock_reason":"resolved"}' >/dev/null || true
}

unlock_issue() {
  api DELETE "/repos/${OWNER}/${REPO}/issues/${ISSUE_NUMBER}/lock" >/dev/null || true
}

set_milestone() {
  local ms=$1
  if [ -z "$ms" ]; then
    echo "No milestone number provided to /milestone" >&2
    return
  fi
  api PATCH "/repos/${OWNER}/${REPO}/issues/${ISSUE_NUMBER}" "$(jq -nc --arg m "$ms" '{milestone:($m|tonumber)}')" >/dev/null || true
}

comment() {
  local body=$1
  api POST "/repos/${OWNER}/${REPO}/issues/${ISSUE_NUMBER}/comments" "$(jq -nc --arg b "$body" '{body:$b}')" >/dev/null || true
}

normalize_user() {
  local u=$1
  echo "${u#@}"
}

process_command() {
  local line="$1"
  line="$(echo "$line" | sed -e 's/^\s*//' -e 's/\s*$//')"
  case "$line" in
    \/assign*)
      args="${line#\/assign}"; read -ra users <<<"$args"; for i in "${!users[@]}"; do users[$i]=$(normalize_user "${users[$i]}"); done; assign "${users[@]}";;
    \/unassign*)
      args="${line#\/unassign}"; read -ra users <<<"$args"; for i in "${!users[@]}"; do users[$i]=$(normalize_user "${users[$i]}"); done; unassign "${users[@]}";;
    \/lgtm)
      add_label "lgtm";;
    \/approve)
      add_label "approved";;
    \/area*)
      lbl="${line#\/area}"; lbl="$(echo "$lbl" | sed -e 's/^\s*//')"; if [ -n "$lbl" ]; then add_label "area/$lbl"; fi;;
    \/priority*)
      lbl="${line#\/priority}"; lbl="$(echo "$lbl" | sed -e 's/^\s*//')"; if [ -n "$lbl" ]; then add_label "priority/$lbl"; fi;;
    \/remove*)
      lbl="${line#\/remove}"; lbl="$(echo "$lbl" | sed -e 's/^\s*//')"; if [ -n "$lbl" ]; then remove_label "$lbl"; fi;;
    \/close)
      close_issue;;
    \/reopen)
      reopen_issue;;
    \/lock)
      lock_issue;;
    \/milestone*)
      ms="${line#\/milestone}"; ms="$(echo "$ms" | sed -e 's/^\s*//')"; set_milestone "$ms";;
    \/hold)
      add_label "hold";;
    \/cc*)
      users_str="${line#\/cc}"; users_str="$(echo "$users_str" | sed -e 's/^\s*//')"; if [ -n "$users_str" ]; then comment "cc: $users_str"; fi;;
    \/uncc*)
      users_str="${line#\/uncc}"; users_str="$(echo "$users_str" | sed -e 's/^\s*//')"; if [ -n "$users_str" ]; then comment "uncc: $users_str"; fi;;
    *)
      echo "No recognized command: $line" >&2;;
  esac
}

main() {
  while IFS= read -r line; do
    if [[ "$line" =~ ^/ ]]; then
      echo "Processing command: $line"
      process_command "$line"
    fi
  done <<<"$COMMENT_BODY"
}

main
