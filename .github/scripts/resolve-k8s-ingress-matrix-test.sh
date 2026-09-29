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

resolve_matrix() {
  local changed_files_json="$1"
  local output_file

  output_file="$(mktemp)"
  CI_CASE_MODE="k8s-ingress" \
    CHANGED_FILES_JSON="${changed_files_json}" \
    GITHUB_OUTPUT="${output_file}" \
    bash "${RESOLVER}" >/dev/null
  awk -F= '$1 == "k8s_ingress_matrix" { sub(/^[^=]*=/, ""); print }' "${output_file}"
  rm -f "${output_file}"
}

assert_matrix() {
  local changed_files_json="$1"
  local expected="$2"
  local actual

  actual="$(resolve_matrix "${changed_files_json}")"
  if [[ "${actual}" != "${expected}" ]]; then
    echo "Expected ${expected} for ${changed_files_json}, got ${actual:-<empty>}" >&2
    return 1
  fi
}

assert_matrix '["shenyu-plugin/shenyu-plugin-proxy/shenyu-plugin-rpc/shenyu-plugin-grpc/pom.xml"]' \
  '{"include":[{"case":"shenyu-integrated-test-k8s-ingress-grpc"}]}'
assert_matrix '["shenyu-plugin/shenyu-plugin-proxy/shenyu-plugin-websocket/pom.xml"]' \
  '{"include":[{"case":"shenyu-integrated-test-k8s-ingress-websocket"}]}'
assert_matrix '["shenyu-plugin/shenyu-plugin-proxy/shenyu-plugin-divide/pom.xml"]' \
  '{"include":[{"case":"shenyu-integrated-test-k8s-ingress-http"}]}'
assert_matrix '["shenyu-plugin/shenyu-plugin-proxy/shenyu-plugin-rpc/shenyu-plugin-dubbo/shenyu-plugin-apache-dubbo/pom.xml"]' \
  '{"include":[{"case":"shenyu-integrated-test-k8s-ingress-apache-dubbo"}]}'
assert_matrix '["shenyu-plugin/shenyu-plugin-proxy/shenyu-plugin-rpc/shenyu-plugin-grpc/pom.xml","shenyu-plugin/shenyu-plugin-proxy/shenyu-plugin-websocket/pom.xml","shenyu-plugin/shenyu-plugin-proxy/shenyu-plugin-rpc/shenyu-plugin-grpc/src/main/java/GrpcPlugin.java"]' \
  '{"include":[{"case":"shenyu-integrated-test-k8s-ingress-grpc"},{"case":"shenyu-integrated-test-k8s-ingress-websocket"}]}'
assert_matrix '["shenyu-common/pom.xml"]' \
  '{"include":[{"case":"shenyu-integrated-test-k8s-ingress-http"},{"case":"shenyu-integrated-test-k8s-ingress-apache-dubbo"},{"case":"shenyu-integrated-test-k8s-ingress-websocket"},{"case":"shenyu-integrated-test-k8s-ingress-grpc"}]}'
assert_matrix '["shenyu-plugin/shenyu-plugin-ai/shenyu-plugin-ai-common/pom.xml"]' \
  '{"include":[]}'
assert_matrix '[".github/workflows/integrated-test-k8s-ingress.yml"]' \
  '{"include":[{"case":"shenyu-integrated-test-k8s-ingress-http"},{"case":"shenyu-integrated-test-k8s-ingress-apache-dubbo"},{"case":"shenyu-integrated-test-k8s-ingress-websocket"},{"case":"shenyu-integrated-test-k8s-ingress-grpc"}]}'

echo "IT-K8s matrix resolver tests passed"
