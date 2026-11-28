#!/bin/bash
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

set -eo pipefail

PRGDIR=$(cd "$(dirname "$0")/.." && pwd)
SHENYU_TESTCASE_DIR=$(cd "${PRGDIR}/../.." && pwd)
FIXTURE_IMAGE=shenyu-websocket-test-fixture:local

docker build -t "$FIXTURE_IMAGE" "${SHENYU_TESTCASE_DIR}/compose/fixtures/websocket"
docker save "$FIXTURE_IMAGE" | sudo k3s ctr images import -

# init kubernetes for mysql
bash "${SHENYU_TESTCASE_DIR}"/k8s/script/storage/storage_init_mysql.sh

# init register center
kubectl apply -f "${SHENYU_TESTCASE_DIR}"/k8s/sync/shenyu-cm.yml

# init shenyu sync
SYNC_ARRAY=("websocket" "http" "zookeeper")
#SYNC_ARRAY=("websocket" "nacos")
MIDDLEWARE_SYNC_ARRAY=("zookeeper" "nacos")
for sync in ${SYNC_ARRAY[@]}; do
  echo -e "------------------\n"
  kubectl apply -f "$SHENYU_TESTCASE_DIR"/k8s/shenyu-mysql.yml
  sleep 30s
  echo "[Start ${sync} synchronous] deploy Admin, Bootstrap and websocket-fixture"
  # shellcheck disable=SC2199
  # shellcheck disable=SC2076
  if [[ "${MIDDLEWARE_SYNC_ARRAY[@]}" =~ "${sync}" ]]; then
    kubectl apply -f "${SHENYU_TESTCASE_DIR}"/k8s/shenyu-"${sync}".yml
    sleep 10s
  fi
  kubectl apply -f "${SHENYU_TESTCASE_DIR}"/k8s/sync/shenyu-admin-"${sync}".yml
  sh "$SHENYU_TESTCASE_DIR"/k8s/script/healthcheck.sh http://localhost:31095/actuator/health
  kubectl apply -f "${SHENYU_TESTCASE_DIR}"/k8s/sync/shenyu-bootstrap-"${sync}".yml
  sh "$SHENYU_TESTCASE_DIR"/k8s/script/healthcheck.sh http://localhost:31195/actuator/health
  kubectl apply -f "${PRGDIR}"/websocket-fixture.yml
  kubectl rollout status deployment/websocket-fixture --timeout=120s
  kubectl get pod -o wide

  ## run e2e-test
  if ! ./mvnw -B -f ./shenyu-e2e/pom.xml -pl shenyu-e2e-case/shenyu-e2e-case-websocket -am test; then
    echo "${sync}-sync-e2e-test failed"
    kubectl logs deployment/websocket-fixture || true
    echo "shenyu-admin log:"
    echo "------------------"
    kubectl logs "$(kubectl get pod -o wide | grep shenyu-admin | awk '{print $1}')"
    echo "shenyu-bootstrap log:"
    echo "------------------"
    kubectl logs "$(kubectl get pod -o wide | grep shenyu-bootstrap | awk '{print $1}')"
    exit 1
  fi
  kubectl delete -f "${SHENYU_TESTCASE_DIR}"/k8s/shenyu-mysql.yml
  kubectl delete -f "${SHENYU_TESTCASE_DIR}"/k8s/sync/shenyu-admin-"${sync}".yml
  kubectl delete -f "${SHENYU_TESTCASE_DIR}"/k8s/sync/shenyu-bootstrap-"${sync}".yml
  kubectl delete -f "${PRGDIR}"/websocket-fixture.yml
  # shellcheck disable=SC2199
  # shellcheck disable=SC2076
  if [[ "${MIDDLEWARE_SYNC_ARRAY[@]}" =~ "${sync}" ]]; then
    kubectl delete -f "${SHENYU_TESTCASE_DIR}"/k8s/shenyu-"${sync}".yml
  fi
  echo "[Remove ${sync} synchronous] deleted Admin, Bootstrap and websocket-fixture"
done
