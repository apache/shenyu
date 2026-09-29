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

# init kubernetes for mysql
SHENYU_TESTCASE_DIR=$(dirname "$(dirname "$(dirname "$(dirname "$0")")")")
bash "${SHENYU_TESTCASE_DIR}"/k8s/script/storage/storage_init_mysql.sh

# init register center
CUR_PATH=$(readlink -f "$(dirname "$0")")
PRGDIR=$(dirname "$CUR_PATH")
# init shenyu sync
if [[ -n "${E2E_SYNC_TYPE:-}" ]]; then
  SYNC_ARRAY=("${E2E_SYNC_TYPE}")
else
  SYNC_ARRAY=("websocket" "http" "zookeeper")
fi
#SYNC_ARRAY=("websocket" "nacos")
#MIDDLEWARE_SYNC_ARRAY=("zookeeper" "etcd" "nacos")

wait_for_port() {
  local host="$1"
  local port="$2"

  for attempt in $(seq 1 30); do
    if (echo >"/dev/tcp/${host}/${port}") 2>/dev/null; then
      echo "${host}:${port} is ready"
      return 0
    fi
    echo "${attempt} waiting for ${host}:${port}"
    sleep 2
  done

  echo "Timed out waiting for ${host}:${port}"
  return 1
}

docker network create -d bridge shenyu

for sync in "${SYNC_ARRAY[@]}"; do
  sync_compose_file="$SHENYU_TESTCASE_DIR"/compose/sync/shenyu-sync-"${sync}".yml
  echo -e "------------------\n"
  echo "[Start ${sync} synchronous] create shenyu-admin-${sync}.yml shenyu-bootstrap-${sync}.yml "
  docker compose -f "${sync_compose_file}" up -d --quiet-pull || true
  sh "$SHENYU_TESTCASE_DIR"/k8s/script/healthcheck.sh http://localhost:31095/actuator/health || exit 1
  docker compose -f "${sync_compose_file}" up -d shenyu-bootstrap
  sh "$SHENYU_TESTCASE_DIR"/k8s/script/healthcheck.sh http://localhost:31195/actuator/health || exit 1
  docker compose -f "${PRGDIR}"/shenyu-rocketmq-compose.yml up -d --quiet-pull
  wait_for_port localhost 31876 || exit 1
  wait_for_port localhost 10911 || exit 1
  docker compose -f "${PRGDIR}"/shenyu-examples-http-compose.yml up -d --quiet-pull
  sh "$SHENYU_TESTCASE_DIR"/k8s/script/healthcheck.sh http://localhost:31189/actuator/health || exit 1
  sleep 10s
  docker ps -a
  ## run e2e-test
  ./mvnw -B -f ./shenyu-e2e/pom.xml -pl shenyu-e2e-case/shenyu-e2e-case-logging-rocketmq -am test
  # shellcheck disable=SC2181
  if (($?)); then
    echo "${sync}-sync-e2e-test failed"
    echo "------------------"
    echo "shenyu-admin log:"
    echo "------------------"
    docker compose -f "$SHENYU_TESTCASE_DIR"/compose/sync/shenyu-sync-"${sync}".yml logs shenyu-admin
    echo "shenyu-bootstrap log:"
    echo "------------------"
    docker compose -f "$SHENYU_TESTCASE_DIR"/compose/sync/shenyu-sync-"${sync}".yml logs shenyu-bootstrap
    echo "shenyu-rocketmq log:"
    echo "------------------"
    docker compose -f "${PRGDIR}"/shenyu-rocketmq-compose.yml logs
    exit 1
  fi
  docker compose -f "${sync_compose_file}" down
  docker compose -f "${PRGDIR}"/shenyu-rocketmq-compose.yml down
  docker compose -f "${PRGDIR}"/shenyu-examples-http-compose.yml down
  echo "[Remove ${sync} synchronous] delete shenyu-admin-${sync}.yml shenyu-bootstrap-${sync}.yml "
done
