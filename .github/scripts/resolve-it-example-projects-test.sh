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
readonly RESOLVER="${SCRIPT_DIR}/resolve-it-example-projects.sh"

assert_projects() {
  local test_case="$1"
  local expected="$2"
  local actual

  actual="$(bash "${RESOLVER}" "${test_case}")"
  if [[ "${actual}" != "${expected}" ]]; then
    echo "Expected ${expected} for ${test_case}, got ${actual}" >&2
    return 1
  fi
}

assert_projects "shenyu-integrated-test-apache-dubbo" \
  ":shenyu-examples-apache-dubbo-service"
assert_projects "shenyu-integrated-test-grpc" ":shenyu-examples-grpc"
assert_projects "shenyu-integrated-test-http" ":shenyu-examples-http"
assert_projects "shenyu-integrated-test-https" ":shenyu-examples-https"
assert_projects "shenyu-integrated-test-spring-cloud" \
  ":shenyu-examples-eureka,:shenyu-examples-springcloud"
assert_projects "shenyu-integrated-test-websocket" \
  ":shenyu-example-spring-native-websocket"
assert_projects "shenyu-integrated-test-rewrite" \
  ":shenyu-examples-http,:shenyu-examples-apache-dubbo-service"
assert_projects "shenyu-integrated-test-combination" \
  ":shenyu-examples-http,:shenyu-examples-apache-dubbo-service,:shenyu-examples-grpc,:shenyu-examples-sofa-service"
assert_projects "shenyu-integrated-test-sdk-apache-dubbo" \
  ":shenyu-examples-sdk-apache-dubbo-provider,:shenyu-examples-sdk-apache-dubbo-consumer"
assert_projects "shenyu-integrated-test-sdk-http" \
  ":shenyu-examples-sdk-http,:shenyu-examples-sdk-feign"

if bash "${RESOLVER}" "unknown-case" >/dev/null 2>&1; then
  echo "Expected unknown case resolution to fail" >&2
  exit 1
fi

echo "integrated-test example project mapping tests passed"
