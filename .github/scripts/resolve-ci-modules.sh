#!/usr/bin/env bash
#
# Licensed to the Apache Software Foundation (ASF) under one or more
# contributor license agreements.  See the NOTICE file distributed with
# this work for additional information regarding copyright ownership.
# The ASF licenses this file to You under the Apache License, Version 2.0
# (the "License"); you may not use this file except in compliance with
# the License.  You may obtain a copy of the License at
#
#     http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.

set -euo pipefail

changed_files_json="${CHANGED_FILES_JSON:-[]}"
max_modules="${MAX_CI_MODULES:-8}"
has_code_changes=false
full_build_required=false
modules=()

if ! command -v jq >/dev/null 2>&1; then
  echo "jq is required to resolve changed Maven modules." >&2
  exit 1
fi

add_module() {
  local module="$1"
  local existing

  for existing in "${modules[@]}"; do
    if [[ "${existing}" == "${module}" ]]; then
      return
    fi
  done

  modules+=("${module}")
}

find_module() {
  local path="$1"
  local dir

  if [[ -d "${path}" ]]; then
    dir="${path}"
  else
    dir="$(dirname "${path}")"
  fi

  while [[ "${dir}" != "." && "${dir}" != "/" ]]; do
    if [[ -f "${dir}/pom.xml" ]]; then
      printf '%s\n' "${dir#./}"
      return
    fi
    dir="$(dirname "${dir}")"
  done
}

is_ignored_change() {
  local file="$1"

  case "${file}" in
    .github/*|*.md|*.txt|resources/static/*|.asf.yaml|.gitignore|.licenserc.yaml|LICENSE|NOTICE|*/LICENSE|*/NOTICE)
      return 0
      ;;
  esac

  return 1
}

while IFS= read -r file; do
  [[ -n "${file}" ]] || continue

  if is_ignored_change "${file}"; then
    continue
  fi

  has_code_changes=true

  case "${file}" in
    pom.xml|mvnw|mvnw.cmd|.mvn/*|actions/*)
      full_build_required=true
      ;;
  esac

  if [[ "$(basename "${file}")" == "pom.xml" && -f "${file}" ]] && grep -q "<modules>" "${file}"; then
    full_build_required=true
  fi

  if [[ "${file}" == *.java || "${file}" == *.xml || "${file}" == *.yml || "${file}" == *.yaml || "${file}" == *.properties || "${file}" == *.sh ]]; then
    module="$(find_module "${file}")"
    if [[ -n "${module:-}" ]]; then
      add_module "${module}"
    fi
  fi
done < <(printf '%s' "${changed_files_json}" | jq -r '.[]')

if [[ "${has_code_changes}" == "true" && ("${#modules[@]}" -eq 0 || "${#modules[@]}" -gt "${max_modules}") ]]; then
  full_build_required=true
fi

modules_csv="$(IFS=,; printf '%s' "${modules[*]}")"

{
  echo "has_code_changes=${has_code_changes}"
  echo "modules=${modules_csv}"
  echo "full_build_required=${full_build_required}"
} >> "${GITHUB_OUTPUT}"

echo "Has code changes: ${has_code_changes}"
echo "Resolved modules: ${modules_csv:-<none>}"
echo "Full build required: ${full_build_required}"
