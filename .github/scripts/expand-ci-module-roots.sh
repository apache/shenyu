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

group_roots="${GROUP_ROOTS:-}"
modules=()
excludes=()

add_module() {
  local module="$1"
  local existing
  local excluded

  for excluded in ${excludes[@]+"${excludes[@]}"}; do
    if [[ "${module}" == "${excluded}" || "${module}" == "${excluded}/"* ]]; then
      return
    fi
  done

  for existing in "${modules[@]}"; do
    if [[ "${existing}" == "${module}" ]]; then
      return
    fi
  done

  modules+=("${module}")
}

IFS=',' read -ra roots <<< "${group_roots}"
# Move a slow subtree into its own shard without duplicating the remaining roots.
for root in "${roots[@]}"; do
  if [[ "${root}" == \!* ]]; then
    excluded="${root#\!}"
    if [[ ! -f "${excluded}/pom.xml" ]]; then
      echo "Excluded Maven module root is missing: ${excluded}" >&2
      exit 1
    fi
    excludes+=("${excluded}")
  fi
done
for root in "${roots[@]}"; do
  [[ -n "${root}" ]] || continue
  [[ "${root}" != \!* ]] || continue

  if [[ ! -f "${root}/pom.xml" ]]; then
    echo "Maven module root is missing: ${root}" >&2
    exit 1
  fi

  if grep -q "<modules>" "${root}/pom.xml"; then
    while IFS= read -r pom; do
      add_module "$(dirname "${pom}")"
    done < <(find "${root}" -name pom.xml | sort)
  else
    add_module "${root}"
  fi
done

if [[ "${#modules[@]}" -eq 0 ]]; then
  echo "No Maven modules were resolved from GROUP_ROOTS." >&2
  exit 1
fi

IFS=,
printf '%s\n' "${modules[*]}"
