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
target_dir="${HOME}/.m2/repository/org/apache"

if [[ ! -d "${artifact_dir}/org/apache/shenyu" ]]; then
  echo "ShenYu local repository artifact is missing: ${artifact_dir}/org/apache/shenyu" >&2
  exit 1
fi

mkdir -p "${target_dir}"
if [[ -d "${target_dir}/shenyu" ]]; then
  mv "${target_dir}/shenyu" "${target_dir}/shenyu.backup.${GITHUB_RUN_ID:-$$}"
fi
cp -R "${artifact_dir}/org/apache/shenyu" "${target_dir}/shenyu"

echo "Restored ShenYu local Maven repository."
