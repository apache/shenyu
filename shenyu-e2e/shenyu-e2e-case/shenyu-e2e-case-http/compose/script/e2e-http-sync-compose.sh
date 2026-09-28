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

set -euo pipefail

# init kubernetes for mysql
SHENYU_TESTCASE_DIR=$(dirname "$(dirname "$(dirname "$(dirname "$0")")")")
bash "${SHENYU_TESTCASE_DIR}"/k8s/script/storage/storage_init_mysql.sh

# init register center
CUR_PATH=$(readlink -f "$(dirname "$0")")
PRGDIR=$(dirname "$CUR_PATH")
HEALTHCHECK_SCRIPT="${SHENYU_TESTCASE_DIR}/k8s/script/healthcheck.sh"

log_compose() {
  local sync=$1
  local sync_compose_file="${SHENYU_TESTCASE_DIR}/compose/sync/shenyu-sync-${sync}.yml"

  echo "------------------"
  echo "shenyu-admin log:"
  echo "------------------"
  docker compose -f "$sync_compose_file" logs shenyu-admin || true
  echo "shenyu-bootstrap log:"
  echo "------------------"
  docker compose -f "$sync_compose_file" logs shenyu-bootstrap || true
  echo "shenyu-examples-http log:"
  echo "------------------"
  docker compose -f "${PRGDIR}"/shenyu-examples-http-compose.yml logs shenyu-examples-http || true
}

start_sync_compose() {
  local sync=$1
  local sync_compose_file="${SHENYU_TESTCASE_DIR}/compose/sync/shenyu-sync-${sync}.yml"

  if ! docker compose -f "$sync_compose_file" up -d --quiet-pull; then
    echo "docker compose returned before all services became healthy; continuing with explicit readiness checks"
    docker compose -f "$sync_compose_file" ps || true
  fi

  sh "$HEALTHCHECK_SCRIPT" http://localhost:31095/actuator/health
  docker compose -f "$sync_compose_file" up -d --quiet-pull shenyu-bootstrap
  sh "$HEALTHCHECK_SCRIPT" http://localhost:31195/actuator/health
}

start_examples() {
  docker compose -f "${PRGDIR}"/shenyu-examples-http-compose.yml up -d --quiet-pull
  sh "$HEALTHCHECK_SCRIPT" http://localhost:31189/actuator/health
}

# init shenyu sync
SYNC_ARRAY=("websocket" "http" "zookeeper")
#SYNC_ARRAY=("websocket" "nacos")
#MIDDLEWARE_SYNC_ARRAY=("zookeeper" "etcd" "nacos")

docker network create -d bridge shenyu || true

for sync in "${SYNC_ARRAY[@]}"; do
  echo -e "------------------\n"
  echo "[Start ${sync} synchronous] create shenyu-admin-${sync}.yml shenyu-bootstrap-${sync}.yml "
  if ! start_sync_compose "$sync"; then
    echo "${sync}-sync-compose startup failed"
    log_compose "$sync"
    exit 1
  fi
  if ! start_examples; then
    echo "shenyu-examples-http startup failed"
    log_compose "$sync"
    exit 1
  fi
  sleep 10s
  docker ps -a
  ## run e2e-test
  if ! ./mvnw -B -f ./shenyu-e2e/pom.xml -pl shenyu-e2e-case/shenyu-e2e-case-http -am test; then
    echo "${sync}-sync-e2e-test failed"
    log_compose "$sync"
    exit 1
  fi
  docker compose -f "$SHENYU_TESTCASE_DIR"/compose/sync/shenyu-sync-"${sync}".yml down
  docker compose -f "${PRGDIR}"/shenyu-examples-http-compose.yml down
  echo "[Remove ${sync} synchronous] delete shenyu-admin-${sync}.yml shenyu-bootstrap-${sync}.yml "
done
