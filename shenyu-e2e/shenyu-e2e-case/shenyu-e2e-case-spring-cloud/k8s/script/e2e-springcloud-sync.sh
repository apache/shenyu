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

docker save shenyu-examples-eureka:latest shenyu-examples-springcloud:latest | sudo k3s ctr images import -

# init kubernetes for mysql
shenyuTestCaseDir=$(dirname "$(dirname "$(dirname "$(dirname "$0")")")")
echo "$shenyuTestCaseDir"
bash "$shenyuTestCaseDir"/k8s/script/init/mysql_container_init.sh

# init register center
curPath=$(readlink -f "$(dirname "$0")")
PRGDIR=$(dirname "$curPath")
echo "$PRGDIR"
kubectl apply -f "${PRGDIR}"/shenyu-examples-eureka.yml
kubectl apply -f "${PRGDIR}"/shenyu-cm.yml
sleep 10s

# init shenyu sync
syncArray=("websocket" "http" "zookeeper" "nacos" "etcd")
middlewareSyncArray=("zookeeper" "nacos" "etcd")
for sync in ${syncArray[@]};
do
    # shellcheck disable=SC2199
    # shellcheck disable=SC2076
    if [[ "${middlewareSyncArray[@]}" =~ " ${sync} " ]]; then
        kubectl apply -f "$shenyuTestCaseDir"/k8s/shenyu-"${sync}".yml
        sleep 10s
    fi
    kubectl apply -f "${PRGDIR}"/shenyu-admin-"${sync}".yml
    kubectl apply -f "${PRGDIR}"/shenyu-bootstrap-"${sync}".yml
    kubectl apply -f "${PRGDIR}"/shenyu-examples-springcloud.yml
    sleep 30s

    kubectl get pod -o wide
    sh "${curPath}"/healthcheck.sh mysql http://localhost:31095/actuator/health http://localhost:31195/actuator/health

    ## run e2e-test
    ./mvnw -B -f ./shenyu-e2e/pom.xml -pl shenyu-e2e-case/shenyu-e2e-case-spring-cloud -am test

    kubectl delete -f "${PRGDIR}"/shenyu-admin-"${sync}".yml
    kubectl delete -f "${PRGDIR}"/shenyu-bootstrap-"${sync}".yml
    kubectl delete -f "${PRGDIR}"/shenyu-examples-springcloud.yml
    sleep 10s
done

#kubectl apply -f "${PRGDIR}"/shenyu-admin-websocket.yml
#kubectl apply -f "${PRGDIR}"/shenyu-bootstrap-websocket.yml
#
#
#kubectl get pod -o wide
#
#sleep 60s
#
#kubectl get pod -o wide
#
#chmod +x "${curPath}"/healthcheck.sh
#sh "${curPath}"/healthcheck.sh mysql http://localhost:31095/actuator/health http://localhost:31195/actuator/health
#
### run e2e-test
#
#curl -S "http://localhost:31195/actuator/pluginData"
#
#./mvnw -B -f ./shenyu-e2e/pom.xml -pl shenyu-e2e-case/shenyu-e2e-case-spring-cloud -am test

