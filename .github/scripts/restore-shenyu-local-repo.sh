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

artifact_dir="${1:-/tmp/shenyu-m2}"
target_dir="${2:-${HOME}/.m2/repository}/org/apache"
script_root="$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")/../.." && pwd)"
project_dir="${3:-${script_root}}"

if [[ -f "${artifact_dir}/repository.tar.gz" ]]; then
  tar -xzf "${artifact_dir}/repository.tar.gz" -C "${artifact_dir}"
fi

if [[ ! -d "${artifact_dir}/org/apache/shenyu" ]]; then
  echo "ShenYu local repository artifact is missing: ${artifact_dir}/org/apache/shenyu" >&2
  exit 1
fi

if [[ -f "${artifact_dir}/maven-wrapper.jar" ]]; then
  # Reuse the launcher's validated build input instead of downloading it in every shard.
  python3 - "${artifact_dir}/maven-wrapper.jar" "${project_dir}/.mvn/wrapper/maven-wrapper.jar" <<'PYTHON'
from pathlib import Path
import sys
import zipfile

source, destination = map(Path, sys.argv[1:])
try:
    with zipfile.ZipFile(source) as archive:
        if 'org/apache/maven/wrapper/MavenWrapperMain.class' not in archive.namelist() or archive.testzip():
            raise SystemExit('Maven wrapper artifact has a missing or corrupt launcher')
except zipfile.BadZipFile:
    raise SystemExit('Maven wrapper artifact is not a valid JAR')
destination.parent.mkdir(parents=True, exist_ok=True)
destination.write_bytes(source.read_bytes())
PYTHON
fi

mkdir -p "${target_dir}"
if [[ -d "${target_dir}/shenyu" ]]; then
  # Backups under ~/.m2/repository grow every saved dependency cache indefinitely.
  backup_dir="$(mktemp -d "${RUNNER_TEMP:-${TMPDIR:-/tmp}}/shenyu-m2-previous.XXXXXX")"
  mv "${target_dir}/shenyu" "${backup_dir}/shenyu"
fi
cp -R "${artifact_dir}/org/apache/shenyu" "${target_dir}/shenyu"

echo "Restored ShenYu local Maven repository."
