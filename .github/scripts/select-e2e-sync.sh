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

# shellcheck disable=SC2034
# SYNC_ARRAY is consumed by the sourcing protocol runners; manual runs keep all channels.
SYNC_ARRAY=("websocket" "http" "zookeeper")
if [[ -n "${SHENYU_E2E_SYNC:-}" ]]; then
  case "${SHENYU_E2E_SYNC}" in
    websocket|http|zookeeper) SYNC_ARRAY=("${SHENYU_E2E_SYNC}") ;;
    *)
      echo "Unsupported E2E sync channel: ${SHENYU_E2E_SYNC}" >&2
      exit 1
      ;;
  esac
fi
