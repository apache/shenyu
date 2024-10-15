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

#docker pull shenyu-examples-apache-dubbo-service:latest

# init kubernetes for mysql
SHENYU_TESTCASE_DIR=$(dirname "$(dirname "$(dirname "$(dirname "$0")")")")
bash "${SHENYU_TESTCASE_DIR}"/k8s/script/storage/storage_init_mysql.sh

# init register center
CUR_PATH=$(readlink -f "$(dirname "$0")")
PRGDIR=$(dirname "$CUR_PATH")
#kubectl apply -f "${SHENYU_TESTCASE_DIR}"/k8s/sync/shenyu-cm.yml

# init shenyu sync
SYNC_ARRAY=("websocket" "http" "zookeeper" "etcd")
#SYNC_ARRAY=("websocket" "nacos")
#MIDDLEWARE_SYNC_ARRAY=("zookeeper" "etcd" "nacos")

docker network create -d bridge shenyu

for sync in "${SYNC_ARRAY[@]}"; do
  echo -e "------------------\n"
#  kubectl apply -f "$SHENYU_TESTCASE_DIR"/k8s/shenyu-mysql.yml
#  docker compose -f "$SHENYU_TESTCASE_DIR"/compose/shenyu-mysql.yml up -d
#  kubectl apply -f "${SHENYU_TESTCASE_DIR}"/k8s/shenyu-zookeeper.yml
#  docker compose -f "$SHENYU_TESTCASE_DIR"/compose/shenyu-zookeeper.yml up -d
#  sleep 30s
  echo "[Start ${sync} synchronous] create shenyu-admin-${sync}.yml shenyu-bootstrap-${sync}.yml "
  # shellcheck disable=SC2199
  # shellcheck disable=SC2076
  # shellcheck disable=SC2154
#  if [[ "${MIDDLEWARE_SYNC_ARRAY[@]}" =~ "${sync}" ]]; then
##    kubectl apply -f "${SHENYU_TESTCASE_DIR}"/k8s/shenyu-"${sync}".yml
#    docker compose -f "$SHENYU_TESTCASE_DIR"/compose/shenyu-"${sync}".yml up -d
#    sleep 10s
#  fi
#  kubectl apply -f "${SHENYU_TESTCASE_DIR}"/k8s/sync/shenyu-admin-"${sync}".yml
  docker compose -f "$SHENYU_TESTCASE_DIR"/compose/sync/shenyu-"${sync}".yml up -d
  sh "$SHENYU_TESTCASE_DIR"/k8s/script/healthcheck.sh http://localhost:31095/actuator/health
#  kubectl apply -f "${SHENYU_TESTCASE_DIR}"/k8s/sync/shenyu-bootstrap-"${sync}".yml
#  docker compose -f "$SHENYU_TESTCASE_DIR"/compose/sync/shenyu-bootstrap-"${sync}".yml up -d
  sh "$SHENYU_TESTCASE_DIR"/k8s/script/healthcheck.sh http://localhost:31195/actuator/health
#  kubectl apply -f "${PRGDIR}"/shenyu-examples-dubbo.yml
#  docker compose -f "${PRGDIR}"/shenyu-examples-dubbo-compose.yml up -d
  sh "$SHENYU_TESTCASE_DIR"/k8s/script/healthcheck.sh http://localhost:31187/actuator/health
  sleep 10s
#  kubectl get pod -o wide
  docker ps -a
  echo -e "------------------\n"
  ## run e2e-test
  ./mvnw -B -f ./shenyu-e2e/pom.xml -pl shenyu-e2e-case/shenyu-e2e-case-apache-dubbo -am test
  # shellcheck disable=SC2181
  if (($?)); then
    echo "${sync}-sync-e2e-test failed"
#    echo "shenyu-examples-dubbo log:"
    echo "------------------"
#    kubectl logs "$(kubectl get pod -o wide | grep shenyu-examples-dubbo-deployment | awk '{print $1}')"
#    docker compose -f "${PRGDIR}"/shenyu-examples-dubbo-compose.yml logs
#    echo "shenyu-admin log:"
#    echo "------------------"
#    kubectl logs "$(kubectl get pod -o wide | grep shenyu-admin | awk '{print $1}')"
    docker compose -f "$SHENYU_TESTCASE_DIR"/compose/sync/shenyu-"${sync}".yml logs
#    echo "shenyu-bootstrap log:"
#    echo "------------------"
#    kubectl logs "$(kubectl get pod -o wide | grep shenyu-bootstrap | awk '{print $1}')"
#    docker compose -f "$SHENYU_TESTCASE_DIR"/compose/sync/shenyu-bootstrap-"${sync}".yml logs
    exit 1
  fi
#  kubectl delete -f "${SHENYU_TESTCASE_DIR}"/k8s/shenyu-mysql.yml
#  docker compose -f "$SHENYU_TESTCASE_DIR"/compose/shenyu-mysql.yml rm -f
#  kubectl delete -f "${SHENYU_TESTCASE_DIR}"/k8s/sync/shenyu-admin-"${sync}".yml
  docker compose -f "$SHENYU_TESTCASE_DIR"/compose/sync/shenyu-"${sync}".yml rm -f
#  kubectl delete -f "${SHENYU_TESTCASE_DIR}"/k8s/sync/shenyu-bootstrap-"${sync}".yml
#  docker compose -f "$SHENYU_TESTCASE_DIR"/compose/sync/shenyu-bootstrap-"${sync}".yml rm -f
#  kubectl delete -f "${PRGDIR}"/shenyu-examples-dubbo.yml
#  docker compose -f "${PRGDIR}"/shenyu-examples-dubbo-compose.yml rm -f
  # shellcheck disable=SC2199
  # shellcheck disable=SC2076
#  if [[ "${MIDDLEWARE_SYNC_ARRAY[@]}" =~ "${sync}" ]]; then
##    kubectl delete -f "${SHENYU_TESTCASE_DIR}"/k8s/shenyu-"${sync}".yml
#    docker compose -f "$SHENYU_TESTCASE_DIR"/compose/shenyu-"${sync}".yml rm -f
#  fi
  echo "[Remove ${sync} synchronous] delete shenyu-admin-${sync}.yml shenyu-bootstrap-${sync}.yml "
done
