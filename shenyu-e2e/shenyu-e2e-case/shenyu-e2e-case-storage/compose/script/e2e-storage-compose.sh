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
curPath=$(readlink -f "$(dirname "$0")")
PRGDIR=$(dirname "$curPath")

docker network create -d bridge shenyu || true

COMPOSE_FILE=""
cleanup() {
  if [ -n "$COMPOSE_FILE" ]; then
    docker compose -f "$COMPOSE_FILE" down || true
  fi
}
trap cleanup EXIT

STORAGE_ARRAY=("h2" "mysql" "opengauss" "postgres")
for storage in "${STORAGE_ARRAY[@]}"; do
  if [ "$storage" != "h2" ]; then
    bash "${SHENYU_TESTCASE_DIR}"/k8s/script/storage/storage_init_"${storage}".sh
  fi

  COMPOSE_FILE="$SHENYU_TESTCASE_DIR/compose/storage/shenyu-storage-$storage.yml"
  docker compose -f "$COMPOSE_FILE" up -d --quiet-pull --wait
  ## run e2e-test
  ./mvnw -B -f ./shenyu-e2e/pom.xml -pl shenyu-e2e-case/shenyu-e2e-case-storage -am test

  echo "shenyu-admin log:"
  echo "------------------"
  docker compose -f "$COMPOSE_FILE" logs shenyu-admin
  echo "shenyu-bootstrap log:"
  echo "------------------"
  docker compose -f "$COMPOSE_FILE" logs shenyu-bootstrap
  docker compose -f "$COMPOSE_FILE" down
  COMPOSE_FILE=""
done
