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

readonly kubeconfig_file="${SHENYU_K3S_KUBECONFIG_FILE:-/etc/rancher/k3s/k3s.yaml}"

install_k3s() {
  curl -sSfL https://get.k3s.io | INSTALL_K3S_VERSION=v1.29.6+k3s2 K3S_KUBECONFIG_MODE=777 sh -
}

for attempt in 1 2 3; do
  if install_k3s && [[ -s "${kubeconfig_file}" ]]; then
    break
  fi
  if [[ "${attempt}" == 3 ]]; then
    echo "k3s install failed after ${attempt} attempts" >&2
    exit 1
  fi
  echo "k3s install failed on attempt ${attempt}" >&2
  sleep "$((attempt * 15))"
done

mkdir -p "${HOME}/.kube"
install -m 600 "${kubeconfig_file}" "${HOME}/.kube/config"
