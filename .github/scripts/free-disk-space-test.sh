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
readonly CLEANUP_SCRIPT="${SCRIPT_DIR}/free-disk-space.sh"

skip_output="$(AVAILABLE_DISK_KB=31457280 MIN_FREE_DISK_KB=20971520 DRY_RUN=true bash "${CLEANUP_SCRIPT}")"
if [[ "${skip_output}" != *"Skipping disk cleanup"* ]]; then
  echo "Expected cleanup to be skipped when 30 GiB is available" >&2
  exit 1
fi

cleanup_output="$(AVAILABLE_DISK_KB=10485760 MIN_FREE_DISK_KB=20971520 DRY_RUN=true bash "${CLEANUP_SCRIPT}")"
if [[ "${cleanup_output}" != *"Disk cleanup required"* ]]; then
  echo "Expected cleanup to be required when only 10 GiB is available" >&2
  exit 1
fi
if [[ "${cleanup_output}" != *"DRY_RUN enabled"* ]]; then
  echo "Expected dry-run cleanup to avoid destructive commands" >&2
  exit 1
fi

echo "free disk space tests passed"
