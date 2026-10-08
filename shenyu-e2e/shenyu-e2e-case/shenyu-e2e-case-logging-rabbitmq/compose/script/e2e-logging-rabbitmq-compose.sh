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

SHENYU_TESTCASE_DIR=$(dirname "$(dirname "$(dirname "$(dirname "$0")")")")
CUR_PATH=$(readlink -f "$(dirname "$0")")
PRGDIR=$(dirname "$CUR_PATH")
SYNC_ARRAY=("websocket" "http" "zookeeper")

cleanup() {
  local sync
  for sync in "${SYNC_ARRAY[@]}"; do
    docker compose -f "${SHENYU_TESTCASE_DIR}/compose/sync/shenyu-sync-${sync}.yml" down --remove-orphans || true
  done
  docker compose -f "${PRGDIR}/shenyu-rabbitmq-compose.yml" down --remove-orphans || true
  docker compose -f "${PRGDIR}/shenyu-examples-http-compose.yml" down --remove-orphans || true
}

trap cleanup EXIT

wait_for_port() {
  local host="$1"
  local port="$2"

  for attempt in $(seq 1 60); do
    if (echo >"/dev/tcp/${host}/${port}") 2>/dev/null; then
      echo "${host}:${port} is ready"
      return 0
    fi
    sleep 2
  done

  echo "Timed out waiting for ${host}:${port}" >&2
  return 1
}

docker network create -d bridge shenyu >/dev/null 2>&1 || true
bash "${SHENYU_TESTCASE_DIR}/k8s/script/storage/storage_init_mysql.sh"

for sync in "${SYNC_ARRAY[@]}"; do
  sync_compose_file="${SHENYU_TESTCASE_DIR}/compose/sync/shenyu-sync-${sync}.yml"
  rabbitmq_compose_file="${PRGDIR}/shenyu-rabbitmq-compose.yml"
  examples_compose_file="${PRGDIR}/shenyu-examples-http-compose.yml"

  echo "Starting ${sync} sync and RabbitMQ logging E2E environment"
  docker compose -f "${sync_compose_file}" up -d --quiet-pull
  bash "${SHENYU_TESTCASE_DIR}/k8s/script/healthcheck.sh" http://localhost:31095/actuator/health
  bash "${SHENYU_TESTCASE_DIR}/k8s/script/healthcheck.sh" http://localhost:31195/actuator/health
  docker compose -f "${rabbitmq_compose_file}" up -d --quiet-pull --wait
  wait_for_port localhost 5672
  docker compose -f "${examples_compose_file}" up -d --quiet-pull
  bash "${SHENYU_TESTCASE_DIR}/k8s/script/healthcheck.sh" http://localhost:31189/actuator/health

  if ! ./mvnw -B -f ./shenyu-e2e/pom.xml -pl shenyu-e2e-case/shenyu-e2e-case-logging-rabbitmq -am test; then
    echo "${sync}-sync RabbitMQ E2E test failed" >&2
    docker compose -f "${sync_compose_file}" logs --tail=all shenyu-admin shenyu-bootstrap || true
    docker compose -f "${rabbitmq_compose_file}" logs --tail=all || true
    exit 1
  fi

  docker compose -f "${sync_compose_file}" down
  docker compose -f "${rabbitmq_compose_file}" down
  docker compose -f "${examples_compose_file}" down
done
