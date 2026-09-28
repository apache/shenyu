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
readonly RESOLVER="${SCRIPT_DIR}/resolve-it-k8s-example-project.sh"

assert_project() {
  local test_case="$1"
  local expected="$2"
  local actual

  actual="$(bash "${RESOLVER}" "${test_case}")"
  if [[ "${actual}" != "${expected}" ]]; then
    echo "Expected ${expected} for ${test_case}, got ${actual}" >&2
    return 1
  fi
}

assert_project "shenyu-integrated-test-k8s-ingress-http" ":shenyu-examples-http"
assert_project "shenyu-integrated-test-k8s-ingress-apache-dubbo" \
  ":shenyu-examples-apache-dubbo-service"
assert_project "shenyu-integrated-test-k8s-ingress-websocket" \
  ":shenyu-example-spring-annotation-websocket"
assert_project "shenyu-integrated-test-k8s-ingress-grpc" ":shenyu-examples-grpc"

if bash "${RESOLVER}" "unknown-case" >/dev/null 2>&1; then
  echo "Expected unknown case resolution to fail" >&2
  exit 1
fi

echo "IT-K8s example project mapping tests passed"
