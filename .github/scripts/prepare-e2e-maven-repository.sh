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

script_dir="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
artifact_dir="${1:-/tmp/shenyu-m2}"
source_repo="${2:-${HOME}/.m2/repository}"
mkdir -p "${artifact_dir}"
artifact_dir="$(cd "${artifact_dir}" && pwd)"
if [[ -e "${artifact_dir}/repository" || -e "${artifact_dir}/repository.tar.gz" ]]; then
  echo "E2E Maven artifact directory must be empty: ${artifact_dir}" >&2
  exit 1
fi

python3 "${script_dir}/e2e-maven-repository.py" plan "${artifact_dir}" "${source_repo}"
example_projects="$(cat "${artifact_dir}/example-projects")"
test_projects="$(cat "${artifact_dir}/test-projects")"
settings="${artifact_dir}/settings.xml"
repository="${artifact_dir}/repository"
common=(-B -ntp -s "${settings}" "-Dmaven.repo.local=${repository}")
examples=(-f shenyu-examples/pom.xml -pl "${example_projects}" -am -Pexample
  -Dmaven.javadoc.skip=true -Dmaven.test.skip=true -Ddocker.skip=true)
tests=(-f shenyu-e2e/pom.xml -pl "${test_projects}" -am)
resolver=org.apache.maven.plugins:maven-dependency-plugin:3.9.0

# Resolve inputs without compiling the selected scenarios. Source failures belong
# to their matrix jobs, while dependency transfer failures belong to this job.
if [[ -n "${example_projects}" ]]; then
  bash "${script_dir}/maven-with-retry.sh" ./mvnw "${common[@]}" "${examples[@]}" \
    "${resolver}:go-offline" "${resolver}:tree" -DoutputType=json -DappendOutput=true \
    "-DoutputFile=${artifact_dir}/example-dependencies.json"
fi

bash "${script_dir}/maven-with-retry.sh" ./mvnw "${common[@]}" "${tests[@]}" \
  "${resolver}:go-offline" "${resolver}:tree" -DoutputType=json -DappendOutput=true \
  "-DoutputFile=${artifact_dir}/test-dependencies.json"

# Collect Maven's mediated graph without requiring unbuilt reactor JARs. Resolve
# only external artifacts, with separate versions for each selected project.
python3 "${script_dir}/e2e-maven-repository.py" dependencies "${artifact_dir}"
inputs=(-f "${artifact_dir}/dependencies/pom.xml")
bash "${script_dir}/maven-with-retry.sh" ./mvnw "${common[@]}" "${inputs[@]}" "${resolver}:resolve"

# Protobuf downloads its configured executables when generating sources. Resolve
# those coordinates from the effective POM without running source generation.
for generator in \
    shenyu-examples-grpc:shenyu-examples/shenyu-examples-grpc/pom.xml \
    shenyu-examples-apache-dubbo-service:shenyu-examples/shenyu-examples-dubbo/shenyu-examples-dubbo-api/pom.xml; do
  project="${generator%%:*}"
  if [[ ",${example_projects}," == *,:"${project}",* ]]; then
    generator_pom="${generator#*:}"
    bash "${script_dir}/maven-with-retry.sh" ./mvnw "${common[@]}" -f "${generator_pom}" \
      org.apache.maven.plugins:maven-help-plugin:3.5.1:effective-pom "-Doutput=${artifact_dir}/generator-pom.xml"
    python3 "${script_dir}/e2e-maven-repository.py" build-tools "${artifact_dir}" "${artifact_dir}/generator-pom.xml"
    while IFS= read -r artifact; do
      # Keep the project's OS-classifier expression and resolve compiler JAR dependencies too.
      bash "${script_dir}/maven-with-retry.sh" ./mvnw "${common[@]}" -f "${generator_pom}" \
        "${resolver}:get" "-Dartifact=${artifact}"
    done < "${artifact_dir}/build-tools"
  fi
done

# Execute only the shared helper to resolve Surefire's dynamic provider. Selected
# E2E scenario sources are never compiled or executed by this prerequisite.
bash "${script_dir}/maven-with-retry.sh" ./mvnw "${common[@]}" \
  -f shenyu-e2e/pom.xml -pl shenyu-e2e-engine -am \
  clean test -Dtest=WaitForHelperTest -Dsurefire.failIfNoSpecifiedTests=false

# Verify the actual build-selected external artifacts, not go-offline's graph.
./mvnw "${common[@]}" -o "${inputs[@]}" "${resolver}:resolve"

# A fresh matrix runner also needs the wrapper's launcher and exact Maven
# distribution. Include only this wrapper version, not the user's Maven cache.
./mvnw -Dstyle.color=never --version > "${artifact_dir}/maven-version.txt"
python3 "${script_dir}/e2e-maven-repository.py" pack "${artifact_dir}" "${MAVEN_USER_HOME:-${HOME}/.m2}"
