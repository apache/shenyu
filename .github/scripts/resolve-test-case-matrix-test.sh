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
readonly RESOLVER="${SCRIPT_DIR}/resolve-test-case-matrix.sh"

read_output() {
  local output_file="$1"
  local output_name="$2"

  awk -F= -v name="${output_name}" '$1 == name { sub(/^[^=]*=/, ""); print }' "${output_file}"
}

assert_output() {
  local mode="$1"
  local changed_files_json="$2"
  local output_name="$3"
  local expected="$4"
  local output_file
  local actual

  output_file="$(mktemp)"
  CI_CASE_MODE="${mode}" \
    CHANGED_FILES_JSON="${changed_files_json}" \
    GITHUB_OUTPUT="${output_file}" \
    bash "${RESOLVER}" >/dev/null

  actual="$(read_output "${output_file}" "${output_name}")"
  rm -f "${output_file}"

  if [[ "${actual}" != "${expected}" ]]; then
    echo "Expected ${output_name}=${expected} for ${mode}:${changed_files_json}, got ${actual}" >&2
    return 1
  fi
}

assert_file_output() {
  local mode="$1"
  local changed_file="$2"
  local output_name="$3"
  local expected="$4"
  local changed_files_json

  changed_files_json="$(jq -cn --arg file "${changed_file}" '[ $file ]')"
  assert_output "${mode}" "${changed_files_json}" "${output_name}" "${expected}"
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

assert_file_output "k8s-ingress" \
  "shenyu-client/shenyu-client-mcp/shenyu-client-mcp-common/pom.xml" \
  "run_k8s_ingress" "false"
assert_file_output "k8s-ingress" \
  "shenyu-plugin/shenyu-plugin-ai/shenyu-plugin-ai-common/pom.xml" \
  "run_k8s_ingress" "false"
assert_file_output "k8s-ingress" \
  "shenyu-integrated-test/shenyu-integrated-test-k8s-ingress-grpc/pom.xml" \
  "run_k8s_ingress" "true"
assert_file_output "k8s-ingress" \
  "shenyu-plugin/shenyu-plugin-proxy/shenyu-plugin-rpc/shenyu-plugin-grpc/pom.xml" \
  "run_k8s_ingress" "true"
assert_file_output "k8s-ingress" "shenyu-common/pom.xml" "run_k8s_ingress" "true"
assert_file_output "k8s-ingress" "pom.xml" "run_k8s_ingress" "true"

assert_file_output "k8s-examples-http" \
  "shenyu-client/shenyu-client-mcp/shenyu-client-mcp-common/pom.xml" \
  "run_k8s_examples" "false"
assert_file_output "k8s-examples-http" \
  "shenyu-examples/shenyu-examples-grpc/pom.xml" \
  "run_k8s_examples" "false"
assert_file_output "k8s-examples-http" \
  "shenyu-examples/shenyu-examples-http/pom.xml" \
  "run_k8s_examples" "true"
assert_file_output "k8s-examples-http" \
  "shenyu-plugin/shenyu-plugin-proxy/shenyu-plugin-divide/pom.xml" \
  "run_k8s_examples" "true"
assert_file_output "k8s-examples-http" "shenyu-bootstrap/pom.xml" "run_k8s_examples" "true"
assert_file_output "k8s-examples-http" "pom.xml" "run_k8s_examples" "true"

readonly E2E_GRPC="shenyu-e2e/shenyu-e2e-case/shenyu-e2e-case-grpc/compose/script/e2e-grpc-sync-compose.sh"
readonly IT_GRPC="shenyu-integrated-test/shenyu-integrated-test-grpc/src/test/java/GrpcPluginTest.java"
readonly IT_K8S_GRPC="shenyu-integrated-test/shenyu-integrated-test-k8s-ingress-grpc/script/healthcheck.sh"
readonly PROD_GRPC="shenyu-plugin/shenyu-plugin-proxy/shenyu-plugin-rpc/shenyu-plugin-grpc/pom.xml"
readonly ADMIN_REGISTER="shenyu-admin/src/main/java/org/apache/shenyu/admin/service/register/AbstractShenyuClientRegisterServiceImpl.java"
readonly ADMIN_SERVICE="shenyu-admin/src/main/java/org/apache/shenyu/admin/service/impl/PluginServiceImpl.java"
readonly ALL_K8S_INGRESS_MATRIX='{"include":[{"case":"shenyu-integrated-test-k8s-ingress-http"},{"case":"shenyu-integrated-test-k8s-ingress-apache-dubbo"},{"case":"shenyu-integrated-test-k8s-ingress-websocket"},{"case":"shenyu-integrated-test-k8s-ingress-grpc"}]}'
readonly ADMIN_REGISTER_E2E_MATRIX='{"include":[{"script":"e2e-http-sync-compose","case":"shenyu-e2e-case-http","example_projects":":shenyu-examples-http"},{"script":"e2e-springcloud-sync-compose","case":"shenyu-e2e-case-spring-cloud","example_projects":":shenyu-examples-eureka,:shenyu-examples-springcloud"},{"script":"e2e-apache-dubbo-sync-compose","case":"shenyu-e2e-case-apache-dubbo","example_projects":":shenyu-examples-apache-dubbo-service"},{"script":"e2e-grpc-sync-compose","case":"shenyu-e2e-case-grpc","example_projects":":shenyu-examples-grpc"},{"script":"e2e-websocket-sync-compose","case":"shenyu-e2e-case-websocket","example_projects":":shenyu-example-spring-native-websocket"}]}'
readonly ADMIN_REGISTER_IT_MATRIX='{"include":[{"case":"shenyu-integrated-test-apache-dubbo"},{"case":"shenyu-integrated-test-grpc"},{"case":"shenyu-integrated-test-http"},{"case":"shenyu-integrated-test-https"},{"case":"shenyu-integrated-test-spring-cloud"},{"case":"shenyu-integrated-test-websocket"},{"case":"shenyu-integrated-test-sdk-apache-dubbo"},{"case":"shenyu-integrated-test-sdk-http"}]}'

assert_ci_ignored "${E2E_GRPC}"
assert_file_output "e2e" "${E2E_GRPC}" "e2e_matrix" \
  '{"include":[{"script":"e2e-grpc-sync-compose","case":"shenyu-e2e-case-grpc","example_projects":":shenyu-examples-grpc"}]}'
assert_file_output "integration" "${E2E_GRPC}" "run_integration" "false"
assert_file_output "k8s-ingress" "${E2E_GRPC}" "run_k8s_ingress" "false"

assert_ci_ignored "${IT_GRPC}"
assert_file_output "integration" "${IT_GRPC}" "integration_matrix" \
  '{"include":[{"case":"shenyu-integrated-test-grpc"}]}'
assert_file_output "e2e" "${IT_GRPC}" "run_e2e" "false"
assert_file_output "k8s-ingress" "${IT_GRPC}" "run_k8s_ingress" "false"

assert_ci_ignored "${IT_K8S_GRPC}"
assert_file_output "k8s-ingress" "${IT_K8S_GRPC}" "run_k8s_ingress" "true"
assert_file_output "integration" "${IT_K8S_GRPC}" "run_integration" "false"
assert_file_output "e2e" "${IT_K8S_GRPC}" "run_e2e" "false"

assert_file_output "integration" "${PROD_GRPC}" "run_integration" "true"
assert_file_output "e2e" "${PROD_GRPC}" "run_e2e" "true"
assert_file_output "k8s-ingress" "${PROD_GRPC}" "run_k8s_ingress" "true"

assert_file_output "k8s-ingress" "${PROD_GRPC}" "k8s_ingress_matrix" \
  '{"include":[{"case":"shenyu-integrated-test-k8s-ingress-grpc"}]}'
assert_file_output "k8s-ingress" \
  "shenyu-plugin/shenyu-plugin-proxy/shenyu-plugin-websocket/pom.xml" \
  "k8s_ingress_matrix" \
  '{"include":[{"case":"shenyu-integrated-test-k8s-ingress-websocket"}]}'
assert_file_output "k8s-ingress" \
  "shenyu-plugin/shenyu-plugin-proxy/shenyu-plugin-divide/pom.xml" \
  "k8s_ingress_matrix" \
  '{"include":[{"case":"shenyu-integrated-test-k8s-ingress-http"}]}'
assert_file_output "k8s-ingress" \
  "shenyu-plugin/shenyu-plugin-proxy/shenyu-plugin-rpc/shenyu-plugin-dubbo/shenyu-plugin-apache-dubbo/pom.xml" \
  "k8s_ingress_matrix" \
  '{"include":[{"case":"shenyu-integrated-test-k8s-ingress-apache-dubbo"}]}'
assert_output "k8s-ingress" \
  '["shenyu-plugin/shenyu-plugin-proxy/shenyu-plugin-rpc/shenyu-plugin-grpc/pom.xml","shenyu-plugin/shenyu-plugin-proxy/shenyu-plugin-websocket/pom.xml","shenyu-plugin/shenyu-plugin-proxy/shenyu-plugin-rpc/shenyu-plugin-grpc/src/main/java/GrpcPlugin.java"]' \
  "k8s_ingress_matrix" \
  '{"include":[{"case":"shenyu-integrated-test-k8s-ingress-grpc"},{"case":"shenyu-integrated-test-k8s-ingress-websocket"}]}'
assert_file_output "k8s-ingress" "shenyu-common/pom.xml" "k8s_ingress_matrix" \
  "${ALL_K8S_INGRESS_MATRIX}"
assert_file_output "k8s-ingress" \
  "shenyu-plugin/shenyu-plugin-ai/shenyu-plugin-ai-common/pom.xml" \
  "k8s_ingress_matrix" '{"include":[]}'
assert_file_output "k8s-ingress" ".github/workflows/integrated-test-k8s-ingress.yml" \
  "k8s_ingress_matrix" "${ALL_K8S_INGRESS_MATRIX}"
assert_file_output "k8s-ingress" ".github/scripts/resolve-test-case-matrix-test.sh" \
  "k8s_ingress_matrix" "${ALL_K8S_INGRESS_MATRIX}"

assert_file_output "e2e" "${ADMIN_REGISTER}" "full_required" "false"
assert_file_output "e2e" "${ADMIN_REGISTER}" "run_storage" "false"
assert_file_output "e2e" "${ADMIN_REGISTER}" "e2e_matrix" "${ADMIN_REGISTER_E2E_MATRIX}"
assert_file_output "integration" "${ADMIN_REGISTER}" "full_required" "false"
assert_file_output "integration" "${ADMIN_REGISTER}" "integration_matrix" "${ADMIN_REGISTER_IT_MATRIX}"
assert_file_output "k8s-ingress" "${ADMIN_REGISTER}" "k8s_ingress_matrix" "${ALL_K8S_INGRESS_MATRIX}"
assert_file_output "k8s-examples-http" "${ADMIN_REGISTER}" "run_k8s_examples" "true"
assert_file_output "e2e" "${ADMIN_SERVICE}" "full_required" "true"
assert_file_output "integration" "${ADMIN_SERVICE}" "full_required" "true"

echo "CI test routing tests passed"
