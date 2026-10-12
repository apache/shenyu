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

SCRIPT_DIR=$(cd "$(dirname "$0")" && pwd)
TEST_TMP=$(mktemp -d)
trap 'rm -f "$TEST_TMP/mvnw" "$TEST_TMP/trace"; rmdir "$TEST_TMP"' EXIT
export RUN_TRACE="$TEST_TMP/trace"
printf '#!/bin/bash\necho maven >> "$RUN_TRACE"\n' > "$TEST_TMP/mvnw"
chmod +x "$TEST_TMP/mvnw"
cd "$TEST_TMP"

sleep() { :; }
docker() { return "${DOCKER_EXIT:-0}"; }
bash() {
    case "$1" in
        */storage_init_*.sh) return "${INIT_EXIT:-0}" ;;
        */healthcheck.sh)
            test -f "$1" || return 99
            test -f "$(dirname "$1")/services-$2.list" || return 98
            printf 'health:%s\n' "$2" >> "$RUN_TRACE"
            return "${HEALTH_EXIT:-0}"
            ;;
        *) command bash "$@" ;;
    esac
}
export -f sleep docker bash

for storage in h2 mysql postgres opengauss storage; do
    script="$SCRIPT_DIR/e2e-$storage-compose.sh"
    command bash -n "$script"
    : > "$RUN_TRACE"
    command bash "$script" > /dev/null
    expected=1
    if [ "$storage" = storage ]; then expected=4; fi
    test "$(grep -c '^maven$' "$RUN_TRACE")" -eq "$expected"
    if [ "$storage" != storage ]; then
        grep -qx "health:$storage" "$RUN_TRACE"
    fi

    : > "$RUN_TRACE"
    if HEALTH_EXIT=1 command bash "$script" > /dev/null 2>&1; then
        echo "$storage continued after a failed healthcheck" >&2
        exit 1
    fi
    if grep -q '^maven$' "$RUN_TRACE"; then exit 1; fi

    : > "$RUN_TRACE"
    if DOCKER_EXIT=1 command bash "$script" > /dev/null 2>&1; then
        echo "$storage continued after a failed Compose command" >&2
        exit 1
    fi
    test ! -s "$RUN_TRACE"

    if [ "$storage" != h2 ] && [ "$storage" != storage ]; then
        if INIT_EXIT=1 command bash "$script" > /dev/null 2>&1; then
            echo "$storage continued after a failed storage initialization" >&2
            exit 1
        fi
    fi
done
echo "All five Compose runners resolve their healthcheck and stop on failures."

