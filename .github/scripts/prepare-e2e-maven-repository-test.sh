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
test_dir="$(mktemp -d)"
trap 'rm -rf "${test_dir}"' EXIT
mkdir -p "${test_dir}/project" "${test_dir}/source"
export INVOCATIONS="${test_dir}/calls"
export MAVEN_USER_HOME="${test_dir}/maven-user-home"
mkdir -p "${MAVEN_USER_HOME}/wrapper/dists/maven-bin/hash/apache-maven/bin" "${test_dir}/project/.mvn/wrapper"
printf 'maven' > "${MAVEN_USER_HOME}/wrapper/dists/maven-bin/hash/apache-maven/bin/mvn"
printf 'zip' > "${MAVEN_USER_HOME}/wrapper/dists/maven-bin/hash/maven-bin.zip"
printf 'launcher' > "${test_dir}/project/.mvn/wrapper/maven-wrapper.jar"
cat > "${test_dir}/project/mvnw" <<'WRAPPER'
#!/usr/bin/env bash
set -euo pipefail
printf '%s\n' "$*" >> "${INVOCATIONS}"
if [[ "$*" == *--version* ]]; then
  echo "Maven home: ${MAVEN_USER_HOME}/wrapper/dists/maven-bin/hash/apache-maven"
  exit 0
fi
if [[ "${FAIL_PREPARATION:-false}" == true ]]; then
  echo '[ERROR] Could not transfer artifact fixture:fixture:jar:1.0 from/to central (https://repo.maven.apache.org/maven2): status code: 403' >&2
  exit 1
fi
if [[ "${FAIL_OFFLINE_RESOLUTION:-false}" == true && " $* " == *" -o "* && "$*" == *:resolve ]]; then
  echo '[ERROR] A build dependency is missing from the offline repository' >&2
  exit 1
fi
if [[ " $* " == *" clean install "* || (" $* " == *" clean test "* && " $* " != *" -pl shenyu-e2e-engine -am "*) ]]; then
  echo 'Scenario compilation must run in its own matrix job' >&2
  exit 1
fi
for arg in "$@"; do
  if [[ "$arg" == -Dmaven.repo.local=* ]]; then
    repository="${arg#*=}"
  fi
  if [[ "$arg" == -Doutput=* ]]; then
    output="${arg#*=}"
  fi
  if [[ "$arg" == -DoutputFile=* ]]; then
    output="${arg#*=}"
  fi
done
if [[ "$*" == *:tree* ]]; then
  python3 - "${output}" <<'TREES'
import json
from pathlib import Path
import sys

def artifact(name, version="1", children=(), classifier=""):
    return dict(groupId="fixture", artifactId=name, version=version, type="jar", classifier=classifier, children=children)

projects = [
    artifact("common", children=[artifact("external", "4.2.1")]),
    artifact("scenario", children=[artifact("common", children=[artifact("external", "4.2.2")]),
                                   artifact("built-input"), artifact("native", classifier="linux-x86_64")]),
]
with Path(sys.argv[1]).open("a", encoding="utf-8") as output:
    for project in projects:
        output.write(json.dumps(project) + "\n")
TREES
fi
if [[ "$*" == *:effective-pom* ]]; then
  cat > "${output}" <<'POM'
<project xmlns="http://maven.apache.org/POM/4.0.0"><build><plugins><plugin>
  <artifactId>protobuf-maven-plugin</artifactId><configuration>
    <protocArtifact>com.google.protobuf:protoc:1.0:exe:linux-x86_64</protocArtifact>
    <pluginArtifact>io.grpc:protoc-gen-grpc-java:2.0:exe:linux-x86_64</pluginArtifact>
    <protocPlugins><protocPlugin>
      <groupId>org.apache.dubbo</groupId><artifactId>dubbo-compiler</artifactId><version>3.0</version>
    </protocPlugin></protocPlugins>
  </configuration>
</plugin></plugins></build></project>
POM
fi
mkdir -p "${repository}/fixture/fixture/1.0"
printf 'fixture' > "${repository}/fixture/fixture/1.0/fixture-1.0.jar"
printf 'metadata' > "${repository}/fixture/fixture/1.0/_remote.repositories"
WRAPPER
chmod +x "${test_dir}/project/mvnw"
cd "${test_dir}/project"

export E2E_MATRIX='{"include":[{"case":"shenyu-e2e-case-http","example_projects":":shenyu-examples-http"},{"case":"shenyu-e2e-case-cluster","example_projects":":shenyu-examples-http"},{"case":"shenyu-e2e-case-grpc","example_projects":":shenyu-examples-grpc"}]}'
export STORAGE_MATRIX='{"include":[{"case":"shenyu-e2e-case-storage"}]}'
bash "${script_dir}/prepare-e2e-maven-repository.sh" "${test_dir}/artifact" "${test_dir}/source"
[[ "$(cat "${test_dir}/artifact/example-projects")" == :shenyu-examples-grpc,:shenyu-examples-http ]]
[[ "$(cat "${test_dir}/artifact/test-projects")" == 'shenyu-e2e-case/shenyu-e2e-case-cluster,shenyu-e2e-case/shenyu-e2e-case-grpc,shenyu-e2e-case/shenyu-e2e-case-http,shenyu-e2e-case/shenyu-e2e-case-storage' ]]
grep -q -- '-o ' "${INVOCATIONS}"
[[ "$(grep -c -- ':go-offline .*:tree ' "${INVOCATIONS}")" -eq 2 ]]
[[ "$(grep -c -- '/dependencies/pom.xml .*:resolve$' "${INVOCATIONS}")" -eq 2 ]]
[[ "$(grep -c -- ' -o .*:resolve$' "${INVOCATIONS}")" -eq 1 ]]
! grep -q -- ' -o .*:go-offline' "${INVOCATIONS}"
grep -q -- '-Ddocker.skip=true' "${INVOCATIONS}"
grep -q -- '-Pexample' "${INVOCATIONS}"
grep -q -- '-Dtest=WaitForHelperTest' "${INVOCATIONS}"
[[ "$(grep -c -- ' clean test ' "${INVOCATIONS}")" -eq 1 ]]
grep -q -- '-Dartifact=com.google.protobuf:protoc:1.0:exe:linux-x86_64' "${INVOCATIONS}"
grep -q -- '-Dartifact=io.grpc:protoc-gen-grpc-java:2.0:exe:linux-x86_64' "${INVOCATIONS}"
grep -q -- '-Dartifact=org.apache.dubbo:dubbo-compiler:3.0:jar' "${INVOCATIONS}"
! grep -q -- '-Prelease,docker\|mvnd' "${INVOCATIONS}"
python3 - "${test_dir}/artifact/dependencies" <<'DEPENDENCIES'
from pathlib import Path
import sys
import xml.etree.ElementTree as ET

ns = {"m": "http://maven.apache.org/POM/4.0.0"}
versions = set()
for pom in Path(sys.argv[1]).glob("project-*/pom.xml"):
    dependencies = ET.parse(pom).findall("m:dependencies/m:dependency", ns)
    by_name = {item.findtext("m:artifactId", namespaces=ns): item for item in dependencies}
    assert "common" not in by_name and "scenario" not in by_name
    version = by_name["external"].findtext("m:version", namespaces=ns)
    versions.add(version)
    if version == "4.2.2":
        assert "built-input" in by_name
        assert by_name["native"].findtext("m:classifier", namespaces=ns) == "linux-x86_64"
    for item in dependencies:
        exclusion = item.find("m:exclusions/m:exclusion", ns)
        assert exclusion.findtext("m:groupId", namespaces=ns) == "*"
        assert exclusion.findtext("m:artifactId", namespaces=ns) == "*"
assert versions == {"4.2.1", "4.2.2"}
DEPENDENCIES
python3 "${script_dir}/e2e-maven-repository.py" restore "${test_dir}/artifact" "${test_dir}/restored"
cmp "${test_dir}/artifact/repository/fixture/fixture/1.0/_remote.repositories" \
  "${test_dir}/restored/repository/fixture/fixture/1.0/_remote.repositories"
cmp "${test_dir}/project/.mvn/wrapper/maven-wrapper.jar" "${test_dir}/restored/wrapper/maven-wrapper.jar"
cmp "${MAVEN_USER_HOME}/wrapper/dists/maven-bin/hash/maven-bin.zip" \
  "${test_dir}/restored/maven-user-home/wrapper/dists/maven-bin/hash/maven-bin.zip"
python3 - "${test_dir}/restored" <<'SETTINGS'
from pathlib import Path
import sys
import xml.etree.ElementTree as ET

directory = Path(sys.argv[1])
settings = ET.parse(directory / "settings.xml").getroot()
ns = {"s": "http://maven.apache.org/SETTINGS/1.0.0"}
assert settings.find("s:offline", ns).text == "true"
assert settings.find("s:localRepository", ns).text == str(directory / "repository")
repositories = settings.findall(".//s:repository", ns) + settings.findall(".//s:pluginRepository", ns)
urls = [item.find("s:url", ns).text for item in repositories if item.find("s:id", ns).text == "e2e-built-inputs"]
assert urls == [(directory / "repository").as_uri()] * 2
SETTINGS

# A 403 remains a real prerequisite failure, without retry or a usable artifact.
: > "${INVOCATIONS}"
if FAIL_PREPARATION=true bash "${script_dir}/prepare-e2e-maven-repository.sh" \
    "${test_dir}/failed" "${test_dir}/source" >"${test_dir}/error" 2>&1; then
  echo 'A failed dependency preparation was accepted' >&2
  exit 1
fi
[[ "$(wc -l < "${INVOCATIONS}")" -eq 1 ]]
[[ ! -e "${test_dir}/failed/repository.tar.gz" ]]

# Missing build-selected dependencies must fail offline validation before upload.
if FAIL_OFFLINE_RESOLUTION=true bash "${script_dir}/prepare-e2e-maven-repository.sh" \
    "${test_dir}/incomplete" "${test_dir}/source" >"${test_dir}/offline-error" 2>&1; then
  echo 'An incomplete offline repository was accepted' >&2
  exit 1
fi
grep -q 'A build dependency is missing' "${test_dir}/offline-error"
[[ ! -e "${test_dir}/incomplete/repository.tar.gz" ]]

# Storage-only changes do not prepare unrelated examples.
: > "${INVOCATIONS}"
E2E_MATRIX='{"include":[]}' bash "${script_dir}/prepare-e2e-maven-repository.sh" \
  "${test_dir}/storage" "${test_dir}/source"
[[ ! -s "${test_dir}/storage/example-projects" ]]
! grep -q -- '-Pexample' "${INVOCATIONS}"

python3 - "${script_dir}/../workflows/e2e-k8s.yml" <<'AGGREGATOR'
from pathlib import Path
import subprocess
import sys
import textwrap

workflow = Path(sys.argv[1]).read_text(encoding="utf-8")
block = workflow.split("      - name: checking job status\n        run: |\n", 1)[1]
template = textwrap.dedent(block)
defaults = {
    "needs.changes.result": "success",
    "needs.changes.outputs.run_e2e": "true",
    "needs.changes.outputs.run_storage": "true",
    "needs.changes.outputs.run_e2e_cases": "true",
    "needs.build-docker-images.result": "success",
    "needs.e2e-storage.result": "success",
    "needs.e2e-case.result": "success",
}
scenarios = [
    ({}, 0, ""),
    ({"needs.changes.result": "failure"}, 1, "prerequisite failed: case selection"),
    ({"needs.build-docker-images.result": "failure", "needs.e2e-storage.result": "skipped",
      "needs.e2e-case.result": "skipped"}, 1, "prerequisite failed: image"),
    ({"needs.e2e-storage.result": "failure"}, 1, "storage matrix failed"),
    ({"needs.e2e-case.result": "cancelled"}, 1, "case matrix failed"),
    ({"needs.changes.outputs.run_e2e": "false", "needs.changes.outputs.run_storage": "false",
      "needs.changes.outputs.run_e2e_cases": "false", "needs.build-docker-images.result": "skipped",
      "needs.e2e-storage.result": "skipped", "needs.e2e-case.result": "skipped"}, 0, ""),
]
for changes, expected, message in scenarios:
    script = template
    for name, value in (defaults | changes).items():
        script = script.replace("${{ " + name + " }}", value)
    result = subprocess.run(["bash", "-e", "-c", script], capture_output=True, text=True)
    assert result.returncode == expected, result.stdout + result.stderr
    assert message in result.stdout, result.stdout
    if "prerequisite failed" in message:
        assert "matrix failed" not in result.stdout, result.stdout
AGGREGATOR

echo 'E2E Maven repository handoff tests passed'
