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

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
readonly SCRIPT_DIR

read_output() {
  local output_file="$1"
  local name="$2"

  awk -F= -v name="${name}" '$1 == name { sub(/^[^=]*=/, ""); print }' "${output_file}"
}

resolve_cases() {
  local mode="$1"
  local changed_file="$2"
  local output_file="$3"

  CI_CASE_MODE="${mode}" \
    CHANGED_FILES_JSON="$(jq -cn --arg file "${changed_file}" '[ $file ]')" \
    GITHUB_OUTPUT="${output_file}" \
    bash "${SCRIPT_DIR}/resolve-test-case-matrix.sh" >/dev/null
}

assert_output() {
  local mode="$1"
  local changed_file="$2"
  local output_name="$3"
  local expected="$4"
  local output_file
  local actual

  output_file="$(mktemp)"
  resolve_cases "${mode}" "${changed_file}" "${output_file}"
  actual="$(read_output "${output_file}" "${output_name}")"
  rm -f "${output_file}"

  if [[ "${actual}" != "${expected}" ]]; then
    echo "Expected ${output_name}=${expected} for ${mode}:${changed_file}, got ${actual}" >&2
    return 1
  fi
}

assert_ci_ignored() {
  local changed_file="$1"
  local output_file
  local actual

  output_file="$(mktemp)"
  CHANGED_FILES_JSON="$(jq -cn --arg file "${changed_file}" '[ $file ]')" \
    GITHUB_OUTPUT="${output_file}" \
    bash "${SCRIPT_DIR}/resolve-ci-modules.sh" >/dev/null
  actual="$(read_output "${output_file}" "has_code_changes")"
  rm -f "${output_file}"

  if [[ "${actual}" != "false" ]]; then
    echo "Expected main CI to ignore ${changed_file}, got has_code_changes=${actual}" >&2
    return 1
  fi
}

readonly E2E_GRPC="shenyu-e2e/shenyu-e2e-case/shenyu-e2e-case-grpc/compose/script/e2e-grpc-sync-compose.sh"
readonly IT_GRPC="shenyu-integrated-test/shenyu-integrated-test-grpc/src/test/java/GrpcPluginTest.java"
readonly IT_K8S_GRPC="shenyu-integrated-test/shenyu-integrated-test-k8s-ingress-grpc/script/healthcheck.sh"
readonly PROD_GRPC="shenyu-plugin/shenyu-plugin-proxy/shenyu-plugin-rpc/shenyu-plugin-grpc/pom.xml"

assert_ci_ignored "${E2E_GRPC}"
assert_output "e2e" "${E2E_GRPC}" "e2e_matrix" \
  '{"include":[{"script":"e2e-grpc-sync-compose","case":"shenyu-e2e-case-grpc","example_projects":":shenyu-examples-grpc"}]}'
assert_output "integration" "${E2E_GRPC}" "run_integration" "false"
assert_output "k8s-ingress" "${E2E_GRPC}" "run_k8s_ingress" "false"

assert_ci_ignored "${IT_GRPC}"
assert_output "integration" "${IT_GRPC}" "integration_matrix" \
  '{"include":[{"case":"shenyu-integrated-test-grpc"}]}'
assert_output "e2e" "${IT_GRPC}" "run_e2e" "false"
assert_output "k8s-ingress" "${IT_GRPC}" "run_k8s_ingress" "false"

assert_ci_ignored "${IT_K8S_GRPC}"
assert_output "k8s-ingress" "${IT_K8S_GRPC}" "run_k8s_ingress" "true"
assert_output "integration" "${IT_K8S_GRPC}" "run_integration" "false"
assert_output "e2e" "${IT_K8S_GRPC}" "run_e2e" "false"

assert_output "integration" "${PROD_GRPC}" "run_integration" "true"
assert_output "e2e" "${PROD_GRPC}" "run_e2e" "true"
assert_output "k8s-ingress" "${PROD_GRPC}" "run_k8s_ingress" "true"

echo "test workflow domain routing tests passed"
