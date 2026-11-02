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
module_count=0

if ! command -v jq >/dev/null 2>&1; then
  echo "jq is required to resolve changed Maven modules." >&2
  exit 1
fi

add_module() {
  local module="$1"
  local existing

  for existing in ${modules[@]+"${modules[@]}"}; do
    if [[ "${existing}" == "${module}" ]]; then
      return
    fi
  done

  modules+=("${module}")
  module_count=$((module_count + 1))
}

is_root_reactor_module() {
  local module="$1"
  local candidate="${module}"

  while [[ "${candidate}" == */* ]]; do
    if grep -Fq "<module>${candidate}</module>" pom.xml; then
      return 0
    fi
    candidate="${candidate%/*}"
  done

  grep -Fq "<module>${candidate}</module>" pom.xml
}

find_module() {
  local path="$1"
  local dir
  local module

  if [[ -d "${path}" ]]; then
    dir="${path}"
  else
    dir="$(dirname "${path}")"
  fi

  while [[ "${dir}" != "." && "${dir}" != "/" ]]; do
    if [[ -f "${dir}/pom.xml" ]]; then
      module="${dir#./}"
      if is_root_reactor_module "${module}"; then
        printf '%s\n' "${module}"
      fi
      return
    fi
    dir="$(dirname "${dir}")"
  done
}

is_ignored_change() {
  local file="$1"

  case "${file}" in
    .github/workflows/ci.yml|.github/scripts/*|actions/*)
      return 1
      ;;
    # The opt-in reactor is packaged and smoke-tested by minimal-http.yml.
    shenyu-bootstrap-http/*|script/performance/minimal-http/*)
      return 0
      ;;
    *.png|*.jpg|*.jpeg|*.gif|*.svg|*.ico|*.pdf|*.doc|*.docx|DISCLAIMER|*/resources/static/*)
      return 0
      ;;
    .github/*|shenyu-e2e/*|shenyu-integrated-test/*|*.md|*.txt|resources/static/*|.asf.yaml|.gitignore|.licenserc.yaml|LICENSE|NOTICE|*/LICENSE|*/NOTICE)
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
    pom.xml|mvnw|mvnw.cmd|.mvn/*|actions/*|.github/workflows/ci.yml|.github/scripts/*)
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
    else
      full_build_required=true
    fi
  fi
done < <(printf '%s' "${changed_files_json}" | jq -r '.[]')

# SPI changes can affect modules that consume shared classes without declaring a
# direct Maven dependency on every transitive module. Build the full reactor so
# those modules cannot silently use a stale SNAPSHOT from the Maven cache.
for module in ${modules[@]+"${modules[@]}"}; do
  if [[ "${module}" == "shenyu-spi" ]]; then
    full_build_required=true
    break
  fi
done

if [[ "${has_code_changes}" == "true" && ("${module_count}" -eq 0 || "${module_count}" -gt "${max_modules}") ]]; then
  full_build_required=true
fi

# Bash 3.2 treats even an initialized empty array as unset with nounset.
# Only expand the joined array after at least one module has been recorded.
modules_csv=""
if [[ "${module_count}" -gt 0 ]]; then
  modules_csv="$(IFS=,; printf '%s' "${modules[*]}")"
fi

{
  echo "has_code_changes=${has_code_changes}"
  echo "modules=${modules_csv}"
  echo "full_build_required=${full_build_required}"
} >> "${GITHUB_OUTPUT}"

echo "Has code changes: ${has_code_changes}"
echo "Resolved modules: ${modules_csv:-<none>}"
echo "Full build required: ${full_build_required}"
