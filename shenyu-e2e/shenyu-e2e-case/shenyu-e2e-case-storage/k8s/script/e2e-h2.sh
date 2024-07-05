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

# init kubernetes for h2
curPath=$(readlink -f "$(dirname "$0")")
PRGDIR=$(dirname "$curPath")
echo "$PRGDIR"
kubectl apply -f "${PRGDIR}"/shenyu-deployment-h2.yml
kubectl apply -f "${PRGDIR}"/shenyu-app-service-h2.yml

sleep 30s

kubectl get pod -o wide

# execute healthcheck.sh
chmod +x "${curPath}"/healthcheck.sh
sh "${curPath}"/healthcheck.sh h2 http://localhost:31095/actuator/health http://localhost:31195/actuator/health

kubectl get pod -o wide

kubectl logs "$(kubectl get pod -o wide | grep shenyu-admin | awk '{print $1}')"

kubectl logs "$(kubectl get pod -o wide | grep shenyu-bootstrap | awk '{print $1}')"
## run e2e-test
sleep 60s
curl -S "http://localhost:31195/actuator/pluginData"

./mvnw -B -f ./shenyu-e2e/pom.xml -pl shenyu-e2e-case/shenyu-e2e-case-storage -am test
