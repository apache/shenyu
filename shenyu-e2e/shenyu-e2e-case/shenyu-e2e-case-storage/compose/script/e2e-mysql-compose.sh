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

# init kubernetes for h2
SHENYU_TESTCASE_DIR=$(dirname "$(dirname "$(dirname "$(dirname "$0")")")")
bash "${SHENYU_TESTCASE_DIR}"/k8s/script/storage/storage_init_mysql.sh
curPath=$(readlink -f "$(dirname "$0")")
PRGDIR=$(dirname "$curPath")
COMPOSE_FILE="$SHENYU_TESTCASE_DIR/compose/storage/shenyu-storage-mysql.yml"

# Start services and wait for their healthchecks.
docker network create -d bridge shenyu || true
trap 'docker compose -f "$COMPOSE_FILE" down || true' EXIT
dump_logs() {
  echo "shenyu-mysql log:"
  echo "------------------"
  docker compose -f "$COMPOSE_FILE" logs shenyu-mysql || true
  echo "shenyu-admin log:"
  echo "------------------"
  docker compose -f "$COMPOSE_FILE" logs shenyu-admin || true
  echo "shenyu-bootstrap log:"
  echo "------------------"
  docker compose -f "$COMPOSE_FILE" logs shenyu-bootstrap || true
}
if ! docker compose -f "$COMPOSE_FILE" up -d --quiet-pull --wait --wait-timeout 300; then
  dump_logs
  exit 1
fi
## run e2e-test

if ! ./mvnw -B -f ./shenyu-e2e/pom.xml -pl shenyu-e2e-case/shenyu-e2e-case-storage -am test; then
  dump_logs
  exit 1
fi

dump_logs
