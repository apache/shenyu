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

mode="${CI_CASE_MODE:-${1:-e2e}}"
changed_files_json="${CHANGED_FILES_JSON:-[]}"
full_required=false
storage_cases=()
e2e_cases=()
integration_cases=()
run_k8s_ingress=false
run_k8s_examples=false

if ! command -v jq >/dev/null 2>&1; then
  echo "jq is required to resolve test case matrices." >&2
  exit 1
fi

add_unique() {
  local target="$1"
  local value="$2"
  local existing
  local -n values="${target}"

  for existing in "${values[@]}"; do
    if [[ "${existing}" == "${value}" ]]; then
      return
    fi
  done

  values+=("${value}")
}

add_storage_all() {
  add_unique storage_cases "e2e-h2-compose"
  add_unique storage_cases "e2e-mysql-compose"
  add_unique storage_cases "e2e-postgres-compose"
  add_unique storage_cases "e2e-opengauss-compose"
}

add_e2e_all() {
  add_storage_all
  add_unique e2e_cases "e2e-http-sync-compose"
  add_unique e2e_cases "e2e-springcloud-sync-compose"
  add_unique e2e_cases "e2e-apache-dubbo-sync-compose"
  add_unique e2e_cases "e2e-grpc-sync-compose"
  add_unique e2e_cases "e2e-websocket-sync-compose"
  add_unique e2e_cases "e2e-logging-rocketmq-compose"
}

add_integration_all() {
  add_unique integration_cases "shenyu-integrated-test-apache-dubbo"
  add_unique integration_cases "shenyu-integrated-test-grpc"
  add_unique integration_cases "shenyu-integrated-test-http"
  add_unique integration_cases "shenyu-integrated-test-https"
  add_unique integration_cases "shenyu-integrated-test-spring-cloud"
  add_unique integration_cases "shenyu-integrated-test-websocket"
  add_unique integration_cases "shenyu-integrated-test-rewrite"
  add_unique integration_cases "shenyu-integrated-test-combination"
  add_unique integration_cases "shenyu-integrated-test-sdk-apache-dubbo"
  add_unique integration_cases "shenyu-integrated-test-sdk-http"
}

is_ignored_change() {
  local file="$1"

  case "${file}" in
    .github/*|*.md|*.txt|resources/static/*|.asf.yaml|.gitignore|.licenserc.yaml|LICENSE|NOTICE)
      return 0
      ;;
  esac

  return 1
}

resolve_k8s_change() {
  local file="$1"

  if [[ "${mode}" == "k8s-ingress" ]]; then
    case "${file}" in
      shenyu-integrated-test-k8s-ingress*/*|shenyu-*/*|pom.xml|*/pom.xml|shenyu-examples/*)
        run_k8s_ingress=true
        ;;
    esac
    return 0
  fi

  if [[ "${mode}" == "k8s-examples-http" ]]; then
    case "${file}" in
      shenyu-examples/*|shenyu-*/*|pom.xml|*/pom.xml)
        run_k8s_examples=true
        ;;
    esac
    return 0
  fi

  return 1
}

mark_full_if_shared() {
  local file="$1"

  case "${file}" in
    pom.xml|mvnw|mvnw.cmd|.mvn/*|actions/*)
      full_required=true
      ;;
    shenyu-common/*|shenyu-web/*|shenyu-bootstrap/*|shenyu-admin/*|shenyu-admin-listener/*|shenyu-sync-data-center/*)
      full_required=true
      ;;
    shenyu-plugin/pom.xml|shenyu-plugin/shenyu-plugin-api/*|shenyu-plugin/shenyu-plugin-base/*)
      full_required=true
      ;;
    shenyu-spring-boot-starter/pom.xml|shenyu-spring-boot-starter/shenyu-spring-boot-starter-plugin/pom.xml)
      full_required=true
      ;;
  esac
}

map_direct_case_path() {
  local file="$1"

  if [[ "${mode}" == "e2e" ]]; then
    case "${file}" in
      shenyu-e2e/pom.xml|shenyu-e2e/shenyu-e2e-common/*|shenyu-e2e/shenyu-e2e-client/*|shenyu-e2e/shenyu-e2e-engine/*|shenyu-e2e/shenyu-e2e-case/pom.xml|shenyu-e2e/shenyu-e2e-case/compose/*|shenyu-e2e/shenyu-e2e-case/k8s/*)
        full_required=true
        ;;
      shenyu-e2e/shenyu-e2e-case/shenyu-e2e-case-storage/*)
        add_storage_all
        ;;
      shenyu-e2e/shenyu-e2e-case/shenyu-e2e-case-http/*)
        add_unique e2e_cases "e2e-http-sync-compose"
        ;;
      shenyu-e2e/shenyu-e2e-case/shenyu-e2e-case-spring-cloud/*)
        add_unique e2e_cases "e2e-springcloud-sync-compose"
        ;;
      shenyu-e2e/shenyu-e2e-case/shenyu-e2e-case-apache-dubbo/*)
        add_unique e2e_cases "e2e-apache-dubbo-sync-compose"
        ;;
      shenyu-e2e/shenyu-e2e-case/shenyu-e2e-case-grpc/*)
        add_unique e2e_cases "e2e-grpc-sync-compose"
        ;;
      shenyu-e2e/shenyu-e2e-case/shenyu-e2e-case-websocket/*)
        add_unique e2e_cases "e2e-websocket-sync-compose"
        ;;
      shenyu-e2e/shenyu-e2e-case/shenyu-e2e-case-logging-rocketmq/*)
        add_unique e2e_cases "e2e-logging-rocketmq-compose"
        ;;
    esac
  else
    case "${file}" in
      shenyu-integrated-test/pom.xml|shenyu-integrated-test/shenyu-integrated-test-common/*)
        full_required=true
        ;;
      shenyu-integrated-test/shenyu-integrated-test-apache-dubbo/*)
        add_unique integration_cases "shenyu-integrated-test-apache-dubbo"
        ;;
      shenyu-integrated-test/shenyu-integrated-test-grpc/*)
        add_unique integration_cases "shenyu-integrated-test-grpc"
        ;;
      shenyu-integrated-test/shenyu-integrated-test-http/*)
        add_unique integration_cases "shenyu-integrated-test-http"
        ;;
      shenyu-integrated-test/shenyu-integrated-test-https/*)
        add_unique integration_cases "shenyu-integrated-test-https"
        ;;
      shenyu-integrated-test/shenyu-integrated-test-spring-cloud/*)
        add_unique integration_cases "shenyu-integrated-test-spring-cloud"
        ;;
      shenyu-integrated-test/shenyu-integrated-test-websocket/*)
        add_unique integration_cases "shenyu-integrated-test-websocket"
        ;;
      shenyu-integrated-test/shenyu-integrated-test-rewrite/*)
        add_unique integration_cases "shenyu-integrated-test-rewrite"
        ;;
      shenyu-integrated-test/shenyu-integrated-test-combination/*)
        add_unique integration_cases "shenyu-integrated-test-combination"
        ;;
      shenyu-integrated-test/shenyu-integrated-test-sdk-apache-dubbo/*)
        add_unique integration_cases "shenyu-integrated-test-sdk-apache-dubbo"
        ;;
      shenyu-integrated-test/shenyu-integrated-test-sdk-http/*)
        add_unique integration_cases "shenyu-integrated-test-sdk-http"
        ;;
    esac
  fi
}

map_domain_path() {
  local file="$1"

  case "${file}" in
    db/*)
      if [[ "${mode}" == "e2e" ]]; then
        add_storage_all
      else
        add_unique integration_cases "shenyu-integrated-test-http"
      fi
      ;;
    *apache-dubbo*|*shenyu-plugin-dubbo*|*shenyu-client-dubbo*|*shenyu-examples-dubbo*)
      if [[ "${mode}" == "e2e" ]]; then
        add_unique e2e_cases "e2e-apache-dubbo-sync-compose"
      else
        add_unique integration_cases "shenyu-integrated-test-apache-dubbo"
        add_unique integration_cases "shenyu-integrated-test-sdk-apache-dubbo"
      fi
      ;;
    *grpc*)
      if [[ "${mode}" == "e2e" ]]; then
        add_unique e2e_cases "e2e-grpc-sync-compose"
      else
        add_unique integration_cases "shenyu-integrated-test-grpc"
      fi
      ;;
    *spring-cloud*|*springcloud*)
      if [[ "${mode}" == "e2e" ]]; then
        add_unique e2e_cases "e2e-springcloud-sync-compose"
      else
        add_unique integration_cases "shenyu-integrated-test-spring-cloud"
      fi
      ;;
    *websocket*)
      if [[ "${mode}" == "e2e" ]]; then
        add_unique e2e_cases "e2e-websocket-sync-compose"
      else
        add_unique integration_cases "shenyu-integrated-test-websocket"
      fi
      ;;
    *rocketmq*)
      if [[ "${mode}" == "e2e" ]]; then
        add_unique e2e_cases "e2e-logging-rocketmq-compose"
      else
        add_unique integration_cases "shenyu-integrated-test-http"
      fi
      ;;
    *logging*)
      if [[ "${mode}" == "e2e" ]]; then
        add_unique e2e_cases "e2e-logging-rocketmq-compose"
      else
        add_unique integration_cases "shenyu-integrated-test-http"
      fi
      ;;
    *rewrite*)
      if [[ "${mode}" == "e2e" ]]; then
        add_unique e2e_cases "e2e-http-sync-compose"
      else
        add_unique integration_cases "shenyu-integrated-test-rewrite"
      fi
      ;;
    *https*)
      if [[ "${mode}" == "e2e" ]]; then
        add_unique e2e_cases "e2e-http-sync-compose"
      else
        add_unique integration_cases "shenyu-integrated-test-https"
      fi
      ;;
    *http*|*springmvc*|*divide*)
      if [[ "${mode}" == "e2e" ]]; then
        add_unique e2e_cases "e2e-http-sync-compose"
      else
        add_unique integration_cases "shenyu-integrated-test-http"
        add_unique integration_cases "shenyu-integrated-test-sdk-http"
      fi
      ;;
    shenyu-sdk/*|shenyu-examples/shenyu-examples-sdk/*)
      if [[ "${mode}" == "integration" ]]; then
        add_unique integration_cases "shenyu-integrated-test-sdk-http"
        add_unique integration_cases "shenyu-integrated-test-sdk-apache-dubbo"
      fi
      ;;
  esac
}

while IFS= read -r file; do
  [[ -n "${file}" ]] || continue

  if is_ignored_change "${file}"; then
    continue
  fi

  if resolve_k8s_change "${file}"; then
    continue
  fi

  if [[ "$(basename "${file}")" == "pom.xml" && -f "${file}" ]] && grep -q "<modules>" "${file}"; then
    full_required=true
  fi

  mark_full_if_shared "${file}"
  map_direct_case_path "${file}"
  map_domain_path "${file}"
done < <(printf '%s' "${changed_files_json}" | jq -r '.[]')

if [[ "${full_required}" == "true" ]]; then
  if [[ "${mode}" == "e2e" ]]; then
    add_e2e_all
  else
    add_integration_all
  fi
fi

storage_matrix="$(printf '%s\n' "${storage_cases[@]}" | jq -R . | jq -cs '{include: map(select(length > 0) | {case:"shenyu-e2e-case-storage", script:.})}')"
e2e_matrix="$(printf '%s\n' "${e2e_cases[@]}" | jq -R . | jq -cs '{include: map(select(length > 0) | {case:(if . == "e2e-http-sync-compose" then "shenyu-e2e-case-http" elif . == "e2e-springcloud-sync-compose" then "shenyu-e2e-case-spring-cloud" elif . == "e2e-apache-dubbo-sync-compose" then "shenyu-e2e-case-apache-dubbo" elif . == "e2e-grpc-sync-compose" then "shenyu-e2e-case-grpc" elif . == "e2e-websocket-sync-compose" then "shenyu-e2e-case-websocket" else "shenyu-e2e-case-logging-rocketmq" end), script:.})}')"
integration_matrix="$(printf '%s\n' "${integration_cases[@]}" | jq -R . | jq -cs '{include: map(select(length > 0) | {case:.})}')"

run_storage=$([[ "${#storage_cases[@]}" -gt 0 ]] && echo true || echo false)
run_e2e_cases=$([[ "${#e2e_cases[@]}" -gt 0 ]] && echo true || echo false)
run_e2e=$([[ "${run_storage}" == "true" || "${run_e2e_cases}" == "true" ]] && echo true || echo false)
run_integration=$([[ "${#integration_cases[@]}" -gt 0 ]] && echo true || echo false)

{
  echo "run_storage=${run_storage}"
  echo "run_e2e_cases=${run_e2e_cases}"
  echo "run_e2e=${run_e2e}"
  echo "storage_matrix=${storage_matrix}"
  echo "e2e_matrix=${e2e_matrix}"
  echo "run_integration=${run_integration}"
  echo "integration_matrix=${integration_matrix}"
  echo "full_required=${full_required}"
  echo "run_k8s_ingress=${run_k8s_ingress}"
  echo "run_k8s_examples=${run_k8s_examples}"
} >> "${GITHUB_OUTPUT}"

echo "Full required: ${full_required}"
echo "Storage matrix: ${storage_matrix}"
echo "E2E matrix: ${e2e_matrix}"
echo "Integration matrix: ${integration_matrix}"
echo "Run k8s ingress: ${run_k8s_ingress}"
echo "Run k8s examples: ${run_k8s_examples}"
