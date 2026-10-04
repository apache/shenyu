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
#

set -euo pipefail

readonly SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
readonly BUILD_SCRIPT="${1:-${SCRIPT_DIR}/build_k8s_cluster.sh}"
readonly K8S_TEST_LOG_FILE="$(mktemp)"
export K8S_TEST_LOG_FILE
trap 'rm -f -- "${K8S_TEST_LOG_FILE}"' EXIT

kind() {
  printf 'kind %s\n' "$*" >> "${K8S_TEST_LOG_FILE}"
  if [[ "${K8S_TEST_CASE}" == load_failure ]]; then
    return 17
  fi
}

kubectl() {
  printf 'kubectl %s\n' "$*" >> "${K8S_TEST_LOG_FILE}"
  case "$*" in
    'apply -f ./shenyu-examples/shenyu-examples-dubbo/shenyu-examples-apache-dubbo-service/k8s/shenyu-zookeeper.yml')
      if [[ "${K8S_TEST_CASE}" == apply_failure ]]; then
        return 19
      fi
      ;;
    'wait --for=condition=Ready pod -l app=shenyu-zk -n shenyu-ingress')
      # A Deployment can exist before its Pod. The old command fails immediately.
      echo 'error: no matching resources found' >&2
      return 1
      ;;
    'rollout status deployment/shenyu-zk -n shenyu-ingress --timeout=120s')
      if [[ "${K8S_TEST_CASE}" == rollout_timeout ]]; then
        echo 'error: timed out waiting for the condition' >&2
        return 1
      elif [[ "${K8S_TEST_CASE}" == rollout_forbidden ]]; then
        echo 'Error from server (Forbidden): deployments.apps is forbidden' >&2
        return 13
      fi
      ;;
    *)
      [[ "$1" == apply ]] || return 23
      ;;
  esac
}
export -f kind kubectl

readonly EXPECTED_COMMANDS="$(printf '%s\n' \
  'kind load docker-image shenyu-examples-apache-dubbo-service:latest' \
  'kind load docker-image apache/shenyu-integrated-test-k8s-ingress-apache-dubbo:latest' \
  'kubectl apply -f ./shenyu-examples/shenyu-examples-dubbo/shenyu-examples-apache-dubbo-service/k8s/shenyu-zookeeper.yml' \
  'kubectl rollout status deployment/shenyu-zk -n shenyu-ingress --timeout=120s' \
  'kubectl apply -f ./shenyu-examples/shenyu-examples-dubbo/shenyu-examples-apache-dubbo-service/k8s/shenyu-examples-dubbo.yml' \
  'kubectl apply -f ./shenyu-integrated-test/shenyu-integrated-test-k8s-ingress-apache-dubbo/deploy/deploy-shenyu.yaml' \
  'kubectl apply -f ./shenyu-examples/shenyu-examples-dubbo/shenyu-examples-apache-dubbo-service/k8s/ingress.yml')"

run_case() {
  local scenario="$1" expected_status="$2" expected_count="$3"
  local status=0 actual expected output
  : > "${K8S_TEST_LOG_FILE}"
  output="$(K8S_TEST_CASE="${scenario}" bash "${BUILD_SCRIPT}" 2>&1)" || status=$?
  actual="$(< "${K8S_TEST_LOG_FILE}")"
  expected="$(printf '%s\n' "${EXPECTED_COMMANDS}" | head -n "${expected_count}")"
  if [[ "${status}" != "${expected_status}" || "${actual}" != "${expected}" ]]; then
    printf 'Unexpected result for %s: exit=%s, expected=%s\n%s\n%s\n' \
      "${scenario}" "${status}" "${expected_status}" "${output}" "${actual}" >&2
    exit 1
  fi
  printf '%s case passed\n' "${scenario}"
}

run_case delayed_pod_success 0 7
run_case rollout_timeout 1 4
run_case rollout_forbidden 13 4
run_case apply_failure 19 3
run_case load_failure 17 1
