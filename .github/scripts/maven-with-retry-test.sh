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

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
readonly SCRIPT_DIR
test_dir="$(mktemp -d)"
trap 'rm -f "${test_dir}/"*; rmdir "${test_dir}"' EXIT
export TEST_DIR="${test_dir}"
export PATH="${test_dir}:${PATH}"
export CANCEL_SLEEP=0

cat > "${test_dir}/maven" <<'BASH'
#!/usr/bin/env bash
set -euo pipefail
attempt=$(($(cat "${TEST_DIR}/count") + 1))
echo "${attempt}" > "${TEST_DIR}/count"
printf '%s\0' "$@" > "${TEST_DIR}/arguments"
if [[ "${attempt}" -gt "${FAILURES}" ]]; then
  echo '[INFO] BUILD SUCCESS'
  exit 0
fi
cat "${TEST_DIR}/failure-${attempt}"
exit 1
BASH

cat > "${test_dir}/sleep" <<'BASH'
#!/usr/bin/env bash
echo "$1" >> "${TEST_DIR}/delays"
if [[ "${CANCEL_SLEEP}" -eq 1 ]]; then
  kill -TERM "${PPID}"
fi
BASH
chmod +x "${test_dir}/maven" "${test_dir}/sleep"

cat > "${test_dir}/reset" <<'LOG'
[ERROR] Failed to execute goal on project shenyu-examples-eureka: Could not collect dependencies
[ERROR] Failed to read artifact descriptor for org.glassfish.jersey.core:jersey-client:jar:3.0.5
[ERROR] Caused by: Could not transfer artifact org.glassfish.jersey.core:jersey-client:pom:3.0.5 from/to central (https://repo.maven.apache.org/maven2): Connection reset
[ERROR] -> [Help 1]
[ERROR] [Help 1] http://cwiki.apache.org/confluence/display/MAVEN/DependencyResolutionException
LOG

cat > "${test_dir}/parent" <<'LOG'
[ERROR] The build could not read 1 project -> [Help 1]
[ERROR] The project org.apache.shenyu:shenyu:2.7.2-SNAPSHOT (/workspace/pom.xml) has 1 error
[ERROR] Non-resolvable parent POM for org.apache.shenyu:shenyu:2.7.2-SNAPSHOT: Could not transfer artifact org.apache:apache:pom:21 from/to central (https://repo.maven.apache.org/maven2): status code: 429, reason phrase: Too Many Requests (429) -> [Help 2]
[ERROR] [Help 1] http://cwiki.apache.org/confluence/display/MAVEN/ProjectBuildingException
[ERROR] [Help 2] http://cwiki.apache.org/confluence/display/MAVEN/UnresolvableModelException
LOG

cat > "${test_dir}/compile" <<'LOG'
[ERROR] Failed to execute goal org.apache.maven.plugins:maven-compiler-plugin:compile on project shenyu-common: Compilation failure
[ERROR] -> [Help 1]
[ERROR] [Help 1] http://cwiki.apache.org/confluence/display/MAVEN/MojoFailureException
LOG

cat > "${test_dir}/tests" <<'LOG'
[ERROR] Tests run: 8, Failures: 1, Errors: 0, Skipped: 0
[ERROR] DividePluginTest.testDivide: expected 200 but received 502 Bad Gateway
[ERROR] Failed to execute goal org.apache.maven.plugins:maven-surefire-plugin:test on project shenyu-common: There are test failures.
[ERROR] -> [Help 1]
[ERROR] [Help 1] http://cwiki.apache.org/confluence/display/MAVEN/MojoFailureException
LOG

cat > "${test_dir}/missing" <<'LOG'
[ERROR] Failed to execute goal on project shenyu-spring-boot-starter-client-beat: Could not resolve dependencies
[ERROR] Could not find artifact org.apache.shenyu:shenyu-register-client-beat:jar:2.7.2-SNAPSHOT in apache.snapshots (https://repository.apache.org/snapshots)
[ERROR] -> [Help 1]
[ERROR] [Help 1] http://cwiki.apache.org/confluence/display/MAVEN/DependencyResolutionException
LOG

run_case() {
  local name="$1" fixture="$2" failures="$3" expected_count="$4" expected_status="$5"
  local status=0
  local attempt
  echo 0 > "${test_dir}/count"
  : > "${test_dir}/delays"
  for attempt in 1 2 3; do
    cp "${fixture}" "${test_dir}/failure-${attempt}"
  done
  FAILURES="${failures}" bash "${SCRIPT_DIR}/maven-with-retry.sh" "${test_dir}/maven" \
    '-Dmessage=two words' 'literal $(command); *' > "${test_dir}/output" 2>&1 || status=$?
  if [[ "${status}" -ne "${expected_status}" || "$(cat "${test_dir}/count")" -ne "${expected_count}" ]]; then
    echo "${name}: expected status/count ${expected_status}/${expected_count}, got ${status}/$(cat "${test_dir}/count")" >&2
    cat "${test_dir}/output" >&2
    exit 1
  fi
  echo "Passed: ${name}"
}

run_case success "${test_dir}/reset" 0 1 0
# shellcheck disable=SC2016 # Command substitution must remain a literal argument.
printf '%s\0' '-Dmessage=two words' 'literal $(command); *' > "${test_dir}/expected-arguments"
cmp "${test_dir}/expected-arguments" "${test_dir}/arguments"
[[ ! -s "${test_dir}/delays" ]]

run_case reset "${test_dir}/reset" 1 2 0
grep -Fq 'Connection reset' "${test_dir}/output"
grep -Fq 'BUILD SUCCESS' "${test_dir}/output"
run_case parent-429 "${test_dir}/parent" 1 2 0

for reason in 'status code: 502, reason phrase: Bad Gateway (502)' 'status: 502 Bad Gateway' 'Return code is: 502, ReasonPhrase: Bad Gateway'; do
  sed "s/Connection reset/${reason}/" "${test_dir}/reset" > "${test_dir}/http"
  run_case "${reason}" "${test_dir}/http" 1 2 0
done

# mvnd prefixes module output, and Maven can emit ANSI colors and CRLF.
sed 's/^/[shenyu-examples-eureka] /' "${test_dir}/reset" > "${test_dir}/prefixed"
while IFS= read -r line; do
  printf '\033[1;31m%s\033[0m\r\n' "${line}"
done < "${test_dir}/prefixed" > "${test_dir}/colored"
run_case colored-mvnd "${test_dir}/colored" 1 2 0

run_case exhausted "${test_dir}/reset" 3 3 1
printf '10\n20\n' > "${test_dir}/expected-delays"
cmp "${test_dir}/expected-delays" "${test_dir}/delays"

for fixture in compile tests missing; do
  run_case "${fixture}" "${test_dir}/${fixture}" 3 1 1
done
sed 's/Connection reset/status code: 404, reason phrase: Not Found (404)/' "${test_dir}/reset" > "${test_dir}/not-found"
run_case http-404 "${test_dir}/not-found" 3 1 1
sed 's/from\/to central (https:\/\/repo.maven.apache.org\/maven2)/from\/to apache.snapshots (https:\/\/repository.apache.org\/snapshots)/' \
  "${test_dir}/reset" > "${test_dir}/other-repository"
run_case other-repository "${test_dir}/other-repository" 3 1 1

# A reset in test output must not make a compiler failure retryable.
cat "${test_dir}/reset" "${test_dir}/compile" > "${test_dir}/mixed"
run_case mixed-failures "${test_dir}/mixed" 3 1 1
cat "${test_dir}/tests" "${test_dir}/reset" > "${test_dir}/mixed"
run_case mixed-failures-reversed "${test_dir}/mixed" 3 1 1

# Multiple missing dependencies can belong to one terminal Maven error.
sed '/^\[ERROR\] ->/i\[ERROR] Could not find artifact org.apache.shenyu:missing:jar:2.7.2-SNAPSHOT' \
  "${test_dir}/reset" > "${test_dir}/mixed-dependencies"
run_case mixed-dependencies "${test_dir}/mixed-dependencies" 3 1 1
sed '/^\[ERROR\] ->/i\[ERROR] Could not transfer artifact org.example:other:pom:1 from/to central (https://repo.maven.apache.org/maven2): status code: 404' \
  "${test_dir}/reset" > "${test_dir}/mixed-transfers"
run_case mixed-transfers "${test_dir}/mixed-transfers" 3 1 1

# ProjectBuildingException also covers unrelated POM errors.
sed 's/has 1 error/has 2 errors/' "${test_dir}/parent" > "${test_dir}/invalid-model"
echo '[ERROR] Unknown packaging: invalid' >> "${test_dir}/invalid-model"
run_case invalid-model "${test_dir}/invalid-model" 3 1 1

# A later attempt must not reuse the preceding attempt's transfer error.
echo 0 > "${test_dir}/count"
cp "${test_dir}/reset" "${test_dir}/failure-1"
cp "${test_dir}/compile" "${test_dir}/failure-2"
status=0
FAILURES=3 bash "${SCRIPT_DIR}/maven-with-retry.sh" "${test_dir}/maven" > "${test_dir}/output" 2>&1 || status=$?
[[ "${status}" -eq 1 && "$(cat "${test_dir}/count")" -eq 2 ]]
echo 'Passed: current-attempt-only'

export CANCEL_SLEEP=1
run_case cancellation "${test_dir}/reset" 3 1 143

echo 'Maven transfer retry tests passed'
