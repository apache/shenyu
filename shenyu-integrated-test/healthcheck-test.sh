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

TEST_ROOT=$(cd "$(dirname "$0")" && pwd)
TEST_TMP=$(mktemp -d)
trap 'rm -f "$TEST_TMP/eureka.log"; rmdir "$TEST_TMP"' EXIT
cd "$TEST_TMP"

curl() {
    printf '%s' "${HEALTH_STATUS:-200}"
    return "${HEALTH_EXIT:-0}"
}
sleep() { :; }
docker() { :; }
wget() {
    if [ "${EUREKA_READY:-1}" = 1 ]; then
        printf '<application>first</application>\n<application>second</application>\n'
    fi
}
export -f curl sleep docker wget
export MAX_RETRIES=2

for module in apache-dubbo combination grpc http https rewrite sdk-apache-dubbo sdk-http sofa spring-cloud websocket; do
    script="$TEST_ROOT/shenyu-integrated-test-$module/script/healthcheck.sh"
    bash -n "$script"
    HEALTH_STATUS=200 HEALTH_EXIT=0 bash "$script" > /dev/null
    if HEALTH_STATUS=503 HEALTH_EXIT=0 bash "$script" > /dev/null 2>&1; then
        echo "$module accepted an unhealthy service" >&2
        exit 1
    fi
    if HEALTH_STATUS=200 HEALTH_EXIT=28 bash "$script" > /dev/null 2>&1; then
        echo "$module accepted a transport timeout" >&2
        exit 1
    fi
done

if EUREKA_READY=0 bash "$TEST_ROOT/shenyu-integrated-test-spring-cloud/script/healthcheck.sh" > /dev/null 2>&1; then
    echo "Spring Cloud accepted missing Eureka registrations" >&2
    exit 1
fi
echo "All 11 healthcheck scripts passed success, HTTP failure and transport failure checks."

