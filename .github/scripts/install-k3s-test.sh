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

readonly SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
readonly INSTALL_SCRIPT="${SCRIPT_DIR}/install-k3s.sh"
tmp_dir="$(mktemp -d)"
trap 'rm -rf -- "${tmp_dir}"' EXIT
mkdir -p "${tmp_dir}/bin"

cat > "${tmp_dir}/bin/curl" <<'STUB'
#!/usr/bin/env bash
set -euo pipefail
printf '%s\n' attempt >> "${K3S_TEST_ATTEMPTS_FILE}"
case "${K3S_TEST_CASE}" in
  download_failure)
    echo 'curl: (22) simulated download failure' >&2
    exit 22
    ;;
  missing_kubeconfig)
    printf '%s\n' '#!/bin/sh' 'exit 0'
    ;;
  empty_kubeconfig)
    printf '%s\n' '#!/bin/sh' \
      'mkdir -p "$(dirname "${SHENYU_K3S_KUBECONFIG_FILE}")"' \
      ': > "${SHENYU_K3S_KUBECONFIG_FILE}"'
    ;;
  success)
    printf '%s\n' '#!/bin/sh' \
      'mkdir -p "$(dirname "${SHENYU_K3S_KUBECONFIG_FILE}")"' \
      'printf "test-kubeconfig\n" > "${SHENYU_K3S_KUBECONFIG_FILE}"'
    ;;
esac
STUB

cat > "${tmp_dir}/bin/sleep" <<'STUB'
#!/usr/bin/env bash
set -euo pipefail
printf '%s\n' "$1" >> "${K3S_TEST_SLEEPS_FILE}"
STUB
chmod +x "${tmp_dir}/bin/curl" "${tmp_dir}/bin/sleep"

run_case() {
  local scenario="$1"
  local expected_status="$2"
  local expected_attempts="$3"
  local expected_sleeps="$4"
  local case_dir="${tmp_dir}/${scenario}"
  local output status=0 attempts=0 sleeps=0
  mkdir -p "${case_dir}/home"

  output="$(
    K3S_TEST_CASE="${scenario}" \
    K3S_TEST_ATTEMPTS_FILE="${case_dir}/attempts" \
    K3S_TEST_SLEEPS_FILE="${case_dir}/sleeps" \
    SHENYU_K3S_KUBECONFIG_FILE="${case_dir}/k3s.yaml" \
    HOME="${case_dir}/home" PATH="${tmp_dir}/bin:${PATH}" \
    bash "${INSTALL_SCRIPT}" 2>&1
  )" || status=$?

  [[ ! -f "${case_dir}/attempts" ]] || attempts="$(wc -l < "${case_dir}/attempts")"
  [[ ! -f "${case_dir}/sleeps" ]] || sleeps="$(wc -l < "${case_dir}/sleeps")"
  if [[ "${status}" != "${expected_status}" || "${attempts}" != "${expected_attempts}" || "${sleeps}" != "${expected_sleeps}" ]]; then
    printf 'Unexpected result for %s: status=%s attempts=%s sleeps=%s\n%s\n' \
      "${scenario}" "${status}" "${attempts}" "${sleeps}" "${output}" >&2
    exit 1
  fi

  if [[ "${scenario}" == success ]]; then
    [[ "$(cat "${case_dir}/home/.kube/config")" == test-kubeconfig ]] || {
      echo "Successful installation did not copy the kubeconfig" >&2
      exit 1
    }
    [[ "$(stat -c %a "${case_dir}/home/.kube/config")" == 600 ]] || {
      echo "Copied kubeconfig has incorrect permissions" >&2
      exit 1
    }
  else
    [[ "${output}" == *"k3s install failed after 3 attempts"* ]] || {
      printf 'Missing final failure message for %s:\n%s\n' "${scenario}" "${output}" >&2
      exit 1
    }
    [[ ! -e "${case_dir}/home/.kube/config" ]] || {
      echo "Failed installation copied a kubeconfig" >&2
      exit 1
    }
  fi
  printf '%s case passed\n' "${scenario}"
}

run_case download_failure 1 3 2
run_case missing_kubeconfig 1 3 2
run_case empty_kubeconfig 1 3 2
run_case success 0 1 0
