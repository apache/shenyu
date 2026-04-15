#!/usr/bin/env bash

set -euo pipefail

usage() {
  cat <<'EOF'
Usage: package-shenyu-maven-snapshots.sh [options]

Package local org/apache/shenyu SNAPSHOT artifacts from ~/.m2/repository into
one tarball that can be extracted into another runner's Maven repository.

Options:
  --repo DIR       Maven repository root. Default: $HOME/.m2/repository
  --group-path DIR Group path under the repository. Default: org/apache/shenyu
  --version VER    Snapshot version to package. Default: parsed from ./pom.xml
  --output FILE    Output tar.gz path. Default: ./artifacts/shenyu-maven-snapshots.tar.gz
  --include-local-metadata
                   Include _remote.repositories and maven-metadata-local.xml
                   for compatibility fallback. Default: disabled
  --dry-run        Print the files that would be packaged
  -h, --help       Show this help
EOF
}

die() {
  echo "error: $*" >&2
  exit 1
}

repo_root() {
  git rev-parse --show-toplevel 2>/dev/null || pwd
}

parse_version() {
  local root
  root="$(repo_root)"

  perl -0777 -ne '
    if (m{<artifactId>\s*shenyu\s*</artifactId>.*?<version>\s*([^<]+)\s*</version>}s) {
      print $1;
      exit 0;
    }
    exit 1;
  ' "$root/pom.xml"
}

repo="${HOME}/.m2/repository"
group_path="org/apache/shenyu"
version=""
output=""
dry_run=0
include_local_metadata=0

while [ "$#" -gt 0 ]; do
  case "$1" in
    --repo)
      repo="$2"
      shift 2
      ;;
    --group-path)
      group_path="$2"
      shift 2
      ;;
    --version)
      version="$2"
      shift 2
      ;;
    --output)
      output="$2"
      shift 2
      ;;
    --include-local-metadata)
      include_local_metadata=1
      shift
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

[ -d "$repo" ] || die "maven repository not found: $repo"

if [ -z "$version" ]; then
  version="$(parse_version)" || die "failed to parse project version from pom.xml"
fi

if [ -z "$output" ]; then
  output="$(repo_root)/artifacts/shenyu-maven-snapshots.tar.gz"
fi

search_root="${repo%/}/${group_path}"
[ -d "$search_root" ] || die "group path not found in repository: $search_root"

tmp_list="$(mktemp "${TMPDIR:-/tmp}/shenyu-maven-snapshots.XXXXXX")"
trap 'rm -f "$tmp_list"' EXIT

find "$search_root" -type d -path "*/${version}" | while IFS= read -r version_dir; do
  find "$version_dir" -maxdepth 1 -type f \
    \( -name '*.pom' -o -name '*.jar' \) \
    ! -name '*-sources.jar' \
    ! -name '*-javadoc.jar' \
    ! -name '*.lastUpdated' \
    | while IFS= read -r artifact_file; do
      printf '%s\n' "${artifact_file#${repo%/}/}"
    done

  if [ "$include_local_metadata" -eq 1 ]; then
    metadata="${version_dir}/_remote.repositories"
    if [ -f "$metadata" ]; then
      printf '%s\n' "${metadata#${repo%/}/}"
    fi

    artifact_dir="$(dirname "$version_dir")"
    metadata="${artifact_dir}/maven-metadata-local.xml"
    if [ -f "$metadata" ]; then
      printf '%s\n' "${metadata#${repo%/}/}"
    fi
  fi
done | sort -u > "$tmp_list"

if [ ! -s "$tmp_list" ]; then
  die "no ${version} artifacts found under ${search_root}"
fi

if [ "$dry_run" -eq 1 ]; then
  cat "$tmp_list"
  exit 0
fi

mkdir -p "$(dirname "$output")"
tar -C "$repo" -czf "$output" -T "$tmp_list"
cp "$tmp_list" "${output}.manifest.txt"

count="$(wc -l < "$tmp_list" | tr -d ' ')"
echo "packaged ${count} paths into ${output}"
echo "manifest written to ${output}.manifest.txt"
