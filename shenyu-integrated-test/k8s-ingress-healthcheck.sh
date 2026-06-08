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

SERVICES_FILE=$1
STARTUP_DELAY_SECONDS=${2:-3}
MAX_RETRIES=${MAX_RETRIES:-30}
CURL_CONNECT_TIMEOUT=${CURL_CONNECT_TIMEOUT:-5}
CURL_MAX_TIME=${CURL_MAX_TIME:-10}
K8S_WAIT_TIMEOUT=${K8S_WAIT_TIMEOUT:-10m}

wait_deployments() {
    local namespace=$1

    echo "waiting for ${namespace} deployments to become available, timeout ${K8S_WAIT_TIMEOUT}"
    deployments=$(kubectl get deployment -n "${namespace}" -o name)
    if [ -z "${deployments}" ]; then
        echo "no deployments found in namespace ${namespace}"
        exit 1
    fi

    for deployment in ${deployments}; do
        kubectl wait --for=condition=Available "${deployment}" -n "${namespace}" --timeout="${K8S_WAIT_TIMEOUT}"
    done
}

wait_deployments shenyu-ingress
wait_deployments default

failed=0
while IFS= read -r service || [ -n "$service" ]; do
    if [[ -z "$service" || "$service" =~ ^# ]]; then
        continue
    fi

    ready=0
    for loop in $(seq 1 "${MAX_RETRIES}"); do
        status=$(curl --connect-timeout "${CURL_CONNECT_TIMEOUT}" --max-time "${CURL_MAX_TIME}" -o /dev/null -s -w "%{http_code}" "$service" || true)
        echo -e "curl $service response $status, attempt $loop/${MAX_RETRIES}"

        if [ "$status" = "200" ]; then
            ready=1
            break
        fi

        sleep 2
    done

    if [ "$ready" -ne 1 ]; then
        echo "service $service failed healthcheck after ${MAX_RETRIES} attempts"
        failed=1
    fi
done < "${SERVICES_FILE}"

sleep "${STARTUP_DELAY_SECONDS}"
echo -e "\n-------------------"
exit "${failed}"
