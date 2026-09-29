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

assert_k8s_output() {
  local mode="$1"
  local changed_file="$2"
  local output_name="$3"
  local expected="$4"
  local output_file
  local actual

  output_file="$(mktemp)"
  CI_CASE_MODE="${mode}" \
    CHANGED_FILES_JSON="$(jq -cn --arg file "${changed_file}" '[ $file ]')" \
    GITHUB_OUTPUT="${output_file}" \
    bash "${RESOLVER}" >/dev/null

  actual="$(awk -F= -v name="${output_name}" '$1 == name { print $2 }' "${output_file}")"
  rm -f "${output_file}"

  if [[ "${actual}" != "${expected}" ]]; then
    echo "Expected ${output_name}=${expected} for ${changed_file}, got ${actual}" >&2
    return 1
  fi
}

assert_k8s_output "k8s-ingress" \
  "shenyu-client/shenyu-client-mcp/shenyu-client-mcp-common/pom.xml" \
  "run_k8s_ingress" "false"
assert_k8s_output "k8s-ingress" \
  "shenyu-plugin/shenyu-plugin-ai/shenyu-plugin-ai-common/pom.xml" \
  "run_k8s_ingress" "false"
assert_k8s_output "k8s-ingress" \
  "shenyu-integrated-test/shenyu-integrated-test-k8s-ingress-grpc/pom.xml" \
  "run_k8s_ingress" "true"
assert_k8s_output "k8s-ingress" \
  "shenyu-plugin/shenyu-plugin-proxy/shenyu-plugin-rpc/shenyu-plugin-grpc/pom.xml" \
  "run_k8s_ingress" "true"
assert_k8s_output "k8s-ingress" "shenyu-common/pom.xml" "run_k8s_ingress" "true"
assert_k8s_output "k8s-ingress" "pom.xml" "run_k8s_ingress" "true"

assert_k8s_output "k8s-examples-http" \
  "shenyu-client/shenyu-client-mcp/shenyu-client-mcp-common/pom.xml" \
  "run_k8s_examples" "false"
assert_k8s_output "k8s-examples-http" \
  "shenyu-examples/shenyu-examples-grpc/pom.xml" \
  "run_k8s_examples" "false"
assert_k8s_output "k8s-examples-http" \
  "shenyu-examples/shenyu-examples-http/pom.xml" \
  "run_k8s_examples" "true"
assert_k8s_output "k8s-examples-http" \
  "shenyu-plugin/shenyu-plugin-proxy/shenyu-plugin-divide/pom.xml" \
  "run_k8s_examples" "true"
assert_k8s_output "k8s-examples-http" "shenyu-bootstrap/pom.xml" "run_k8s_examples" "true"
assert_k8s_output "k8s-examples-http" "pom.xml" "run_k8s_examples" "true"

echo "resolve-test-case-matrix k8s tests passed"
