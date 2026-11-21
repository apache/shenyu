#!/usr/bin/env bash
# Licensed to the Apache Software Foundation (ASF) under one or more
# contributor license agreements. See the NOTICE file distributed with
# this work for additional information regarding copyright ownership.
# The ASF licenses this file to You under the Apache License, Version 2.0
# (the "License"); you may not use this file except in compliance with
# the License. You may obtain a copy of the License at
#
#     http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.

set -euo pipefail
output=$(mktemp)
trap 'rm -f "$output"' EXIT

assert_routing() {
  CHANGED_FILES_JSON="$1" GITHUB_OUTPUT="$output" "${BASH}" .github/scripts/resolve-ci-modules.sh > /dev/null
  grep -Fxq "has_code_changes=$2" "$output"
  grep -Fxq "modules=$3" "$output"
  grep -Fxq "full_build_required=$4" "$output"
  : > "$output"
}

assert_routing '[]' false '' false
assert_routing '["README.md"]' false '' false
assert_routing '["shenyu-bootstrap-http/pom.xml"]' false '' false
assert_routing '["shenyu-bootstrap-http/src/main/java/org/apache/shenyu/bootstrap/http/ShenyuHttpApplication.java"]' false '' false
assert_routing '["script/performance/minimal-http/fixture.py"]' false '' false
assert_routing '["shenyu-bootstrap-http/pom.xml","shenyu-common/pom.xml"]' true 'shenyu-common' false
assert_routing '["pom.xml"]' true '' true
assert_routing '["shenyu-common/pom.xml","shenyu-common/pom.xml"]' true 'shenyu-common' false
assert_routing '["shenyu-spi/pom.xml"]' true 'shenyu-spi' true
MAX_CI_MODULES=0 assert_routing '["shenyu-common/pom.xml"]' true 'shenyu-common' true
echo 'Minimal HTTP CI routing tests passed (10 cases)'
