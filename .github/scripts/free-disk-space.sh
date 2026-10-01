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

readonly MIN_FREE_DISK_KB="${MIN_FREE_DISK_KB:-20971520}"
readonly DRY_RUN="${DRY_RUN:-false}"

if [[ -n "${AVAILABLE_DISK_KB:-}" ]]; then
  available_disk_kb="${AVAILABLE_DISK_KB}"
else
  available_disk_kb="$(df --output=avail -k / | tail -n 1 | tr -d ' ')"
fi
readonly available_disk_kb

if [[ ! "${MIN_FREE_DISK_KB}" =~ ^[0-9]+$ || ! "${available_disk_kb}" =~ ^[0-9]+$ ]]; then
  echo "Disk thresholds must be integer values in KiB" >&2
  exit 1
fi

echo "Available disk: ${available_disk_kb} KiB; required: ${MIN_FREE_DISK_KB} KiB"
if ((available_disk_kb >= MIN_FREE_DISK_KB)); then
  echo "Skipping disk cleanup because sufficient space is available"
  exit 0
fi

echo "Disk cleanup required because available space is below the threshold"
if [[ "${DRY_RUN}" == "true" ]]; then
  echo "DRY_RUN enabled; cleanup commands were not executed"
  exit 0
fi

df --human-readable
sudo apt clean
while IFS= read -r image; do
  [[ -n "${image}" ]] || continue
  docker rmi "${image}" || true
done < <(docker image ls --all --quiet)

case "${AGENT_TOOLSDIRECTORY:-}" in
  /opt/hostedtoolcache|/opt/hostedtoolcache/*)
    sudo rm -rf -- "${AGENT_TOOLSDIRECTORY}"
    ;;
  "")
    echo "AGENT_TOOLSDIRECTORY is not set; skipping tool cache cleanup"
    ;;
  *)
    echo "Refusing to remove unexpected AGENT_TOOLSDIRECTORY: ${AGENT_TOOLSDIRECTORY}" >&2
    exit 1
    ;;
esac

df --human-readable
