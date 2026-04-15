#!/usr/bin/env bash

set -euo pipefail

usage() {
  cat <<'EOF'
Usage: package-integrated-test-local-images.sh [options]

Create one tarball containing all local :latest Docker images referenced by the
matrix cases in .github/workflows/integrated-test.yml.

Options:
  --workflow FILE      Workflow to inspect. Default: .github/workflows/integrated-test.yml
  --compose-root DIR   Compose root. Default: shenyu-integrated-test
  --output FILE        Output tar path. Default: ./artifacts/integrated-test-local-images.tar
  --exclude-image IMG  Exclude a specific image. Repeatable.
  --exclude-file FILE  Read excluded images from a file, one per line.
  --image-regex REGEX  Local image regex. Default: ^(apache/shenyu-|shenyu-).+:latest$
  --dry-run            Print the images that would be packaged
  -h, --help           Show this help
EOF
}

die() {
  echo "error: $*" >&2
  exit 1
}

repo_root() {
  git rev-parse --show-toplevel 2>/dev/null || pwd
}

contains_exact() {
  local needle="$1"
  shift || true
  local item
  for item in "$@"; do
    if [ "$item" = "$needle" ]; then
      return 0
    fi
  done
  return 1
}

workflow="$(repo_root)/.github/workflows/integrated-test.yml"
compose_root="$(repo_root)/shenyu-integrated-test"
output="$(repo_root)/artifacts/integrated-test-local-images.tar"
image_regex='^(apache/shenyu-|shenyu-).+:latest$'
dry_run=0
exclude_images=()

while [ "$#" -gt 0 ]; do
  case "$1" in
    --workflow)
      workflow="$2"
      shift 2
      ;;
    --compose-root)
      compose_root="$2"
      shift 2
      ;;
    --output)
      output="$2"
      shift 2
      ;;
    --exclude-image)
      exclude_images+=("$2")
      shift 2
      ;;
    --exclude-file)
      [ -f "$2" ] || die "exclude file not found: $2"
      while IFS= read -r line || [ -n "$line" ]; do
        case "$line" in
          ''|\#*)
            continue
            ;;
          *)
            exclude_images+=("$line")
            ;;
        esac
      done < "$2"
      shift 2
      ;;
    --image-regex)
      image_regex="$2"
      shift 2
      ;;
    --dry-run)
      dry_run=1
      shift
      ;;
    -h|--help)
      usage
      exit 0
      ;;
    *)
      die "unknown argument: $1"
      ;;
  esac
done

command -v docker >/dev/null 2>&1 || die "docker is required"
[ -f "$workflow" ] || die "workflow not found: $workflow"
[ -d "$compose_root" ] || die "compose root not found: $compose_root"

tmp_cases="$(mktemp "${TMPDIR:-/tmp}/shenyu-it-cases.XXXXXX")"
tmp_images="$(mktemp "${TMPDIR:-/tmp}/shenyu-it-images.XXXXXX")"
trap 'rm -f "$tmp_cases" "$tmp_images"' EXIT

awk '
  $1 == "case:" { in_case = 1; next }
  in_case && $1 == "runs-on:" { exit }
  in_case && $1 == "-" { print $2 }
' "$workflow" > "$tmp_cases"

[ -s "$tmp_cases" ] || die "no matrix cases found in $workflow"

while IFS= read -r case_name; do
  compose_file="${compose_root%/}/${case_name}/docker-compose.yml"
  [ -f "$compose_file" ] || die "compose file not found for ${case_name}: ${compose_file}"

  docker compose -f "$compose_file" config --images 2>/dev/null \
    | grep -E "$image_regex" \
    >> "$tmp_images" || true
done < "$tmp_cases"

sort -u "$tmp_images" -o "$tmp_images"

if [ "${#exclude_images[@]}" -gt 0 ]; then
  tmp_filtered="$(mktemp "${TMPDIR:-/tmp}/shenyu-it-images-filtered.XXXXXX")"
  trap 'rm -f "$tmp_cases" "$tmp_images" "$tmp_filtered"' EXIT

  while IFS= read -r image; do
    if contains_exact "$image" "${exclude_images[@]}"; then
      continue
    fi
    printf '%s\n' "$image"
  done < "$tmp_images" > "$tmp_filtered"

  mv "$tmp_filtered" "$tmp_images"
fi

[ -s "$tmp_images" ] || die "no matching local images found"

if [ "$dry_run" -eq 1 ]; then
  cat "$tmp_images"
  exit 0
fi

while IFS= read -r image; do
  docker image inspect "$image" >/dev/null 2>&1 || die "local image not found: $image"
done < "$tmp_images"

mkdir -p "$(dirname "$output")"

images=()
while IFS= read -r image; do
  images+=("$image")
done < "$tmp_images"

docker save -o "$output" "${images[@]}"
cp "$tmp_images" "${output}.manifest.txt"

count="$(wc -l < "$tmp_images" | tr -d ' ')"
echo "packaged ${count} images into ${output}"
echo "manifest written to ${output}.manifest.txt"
