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
readonly TEST_DIR="$(mktemp -d)"
export K8S_TEST_LOG_FILE="${TEST_DIR}/commands" K8S_TEST_IMAGE_FILE="${TEST_DIR}/image"
trap 'rm -rf -- "${TEST_DIR}"' EXIT

cat > "${TEST_DIR}/mvnw" <<'STUB'
#!/usr/bin/env bash
set -euo pipefail
printf 'mvnw %s\n' "$*" >> "${K8S_TEST_LOG_FILE}"
[[ "${K8S_TEST_CASE}" != build_failure ]] || exit 21
if [[ "${K8S_TEST_CASE}" != image_still_missing ]]; then
  touch "${K8S_TEST_IMAGE_FILE}"
fi
STUB
chmod +x "${TEST_DIR}/mvnw"

docker() {
  printf 'docker %s\n' "$*" >> "${K8S_TEST_LOG_FILE}"
  [[ "$*" == 'image inspect shenyu-examples-http:latest' ]] || return 29
  [[ -e "${K8S_TEST_IMAGE_FILE}" ]] || return 1
}

kind() {
  printf 'kind %s\n' "$*" >> "${K8S_TEST_LOG_FILE}"
  if [[ "$*" == "load docker-image ${K8S_TEST_FAILURE}" ]]; then
    return 17
  fi
}

kubectl() {
  printf 'kubectl %s\n' "$*" >> "${K8S_TEST_LOG_FILE}"
  if [[ "$*" == "apply -f ${K8S_TEST_FAILURE}" ]]; then
    return 19
  fi
}
export -f docker kind kubectl

readonly BUILD_COMMAND='mvnw -B -ntp clean install -Pexample -Dmaven.javadoc.skip=true -Dmaven.test.skip=true -pl shenyu-examples-http -am -f ./shenyu-examples/pom.xml'
readonly INSPECT_COMMAND='docker image inspect shenyu-examples-http:latest'
readonly CLUSTER_COMMANDS="$(printf '%s\n' \
  'kind load docker-image shenyu-examples-http:latest' \
  'kind load docker-image apache/shenyu-integrated-test-k8s-ingress-http:latest' \
  'kubectl apply -f ./shenyu-examples/shenyu-examples-http/k8s/shenyu-examples-http.yml' \
  'kubectl apply -f ./shenyu-integrated-test/shenyu-integrated-test-k8s-ingress-http/deploy/deploy-shenyu.yaml' \
  'kubectl apply -f ./shenyu-examples/shenyu-examples-http/k8s/ingress.yml')"

run_case() {
  local scenario="$1" failure="$2" expected_status="$3" expected_count="$4"
  local status=0 actual expected output
  : > "${K8S_TEST_LOG_FILE}"
  rm -f -- "${K8S_TEST_IMAGE_FILE}"
  expected="${INSPECT_COMMAND}"
  case "${scenario}" in
    build_failure)
      expected+=$'\n'"${BUILD_COMMAND}"
      ;;
    image_still_missing|missing_image_rebuilt)
      expected+=$'\n'"${BUILD_COMMAND}"$'\n'"${INSPECT_COMMAND}"
      ;;
    *) touch "${K8S_TEST_IMAGE_FILE}" ;;
  esac
  if [[ "${expected_count}" -gt 0 ]]; then
    expected+=$'\n'"$(printf '%s\n' "${CLUSTER_COMMANDS}" | head -n "${expected_count}")"
  fi
  output="$(cd "${TEST_DIR}" && K8S_TEST_CASE="${scenario}" K8S_TEST_FAILURE="${failure}" bash "${BUILD_SCRIPT}" 2>&1)" || status=$?
  actual="$(< "${K8S_TEST_LOG_FILE}")"
  if [[ "${status}" != "${expected_status}" || "${actual}" != "${expected}" ]]; then
    printf 'Unexpected result for %s: exit=%s, expected=%s\n%s\n%s\n' \
      "${scenario}" "${status}" "${expected_status}" "${output}" "${actual}" >&2
    exit 1
  fi
  printf '%s case passed\n' "${scenario}"
}

run_case existing_image none 0 5
run_case missing_image_rebuilt none 0 5
run_case build_failure none 21 0
run_case image_still_missing none 1 0
run_case example_load_failure shenyu-examples-http:latest 17 1
run_case gateway_load_failure apache/shenyu-integrated-test-k8s-ingress-http:latest 17 2
run_case example_apply_failure ./shenyu-examples/shenyu-examples-http/k8s/shenyu-examples-http.yml 19 3
run_case gateway_apply_failure ./shenyu-integrated-test/shenyu-integrated-test-k8s-ingress-http/deploy/deploy-shenyu.yaml 19 4
run_case ingress_apply_failure ./shenyu-examples/shenyu-examples-http/k8s/ingress.yml 19 5
