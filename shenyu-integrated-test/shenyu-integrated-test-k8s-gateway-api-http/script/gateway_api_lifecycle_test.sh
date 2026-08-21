#!/bin/bash
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
#

# Gateway API lifecycle scenarios: the negative / exception / dynamic flows that the
# happy-path JUnit test cannot cover:
#   - cross-namespace backendRef ReferenceGrant semantics (RefNotPermitted <-> BackendNotFound)
#   - GatewayClass deletion cascade cleanup and restore
#   - HTTPRoute / Gateway object deletion cleanup and restore
#   - wildcard hostname matching and stale selector cleanup
#   - HTTPRoute spec change (path switch, multi backendRef)
#   - backend scale up/down/upstream convergence
#   - multi-parent Gateway partial deletion
# Must run from the repository root after the cluster is up and healthy.

set -euo pipefail

# Resolve the repo root from the script location so the caller's cwd is irrelevant:
# script lives at <repo>/shenyu-integrated-test/shenyu-integrated-test-k8s-gateway-api-http/script/
SCRIPT_DIR=$(cd "$(dirname "$0")" && pwd)
REPO_ROOT=$(cd "$SCRIPT_DIR/../../.." && pwd)
cd "$REPO_ROOT"

GATEWAY=http://localhost:30095
GW_NS=shenyu-gateway-api
ROUTE_MANIFEST=./shenyu-examples/shenyu-examples-http/k8s/gateway-api.yml
BACKEND_DEPLOYMENT=shenyu-examples-http-deployment

info() { echo "[lifecycle] $*"; }
fail() { echo "[LIFECYCLE-FAIL] $*"; exit 1; }

expect_no_selector() {
  local body
  body=$(curl -s -m 15 -X POST "$GATEWAY$1" -H 'Content-Type: application/json' -d '{"id":"1","name":"Tom"}' || true)
  [[ "$body" == *'"code":-107'* ]] || return 1
}

expect_backend() {
  local body
  body=$(curl -s -m 15 -X POST "$GATEWAY$1" -H 'Content-Type: application/json' -d '{"id":"1","name":"Tom"}' || true)
  [[ "$body" == *'hello world save order'* ]] || return 1
}

expect_backend_with_host() {
  local body
  body=$(curl -s -m 15 -X POST "$GATEWAY$1" -H 'Content-Type: application/json' -H "Host: $2" -d '{"id":"1","name":"Tom"}' || true)
  [[ "$body" == *'hello world save order'* ]] || return 1
}

# Poll a verification command until it succeeds or the timeout (seconds) is hit.
wait_for() {
  local desc=$1 timeout=$2
  shift 2
  local waited=0
  until "$@" >/dev/null 2>&1; do
    sleep 3
    waited=$((waited + 3))
    if (( waited >= timeout )); then
      info "timeout after ${timeout}s waiting: $desc; dumping state"
      kubectl get httproute demo-http-route -o jsonpath='{.status.parents}' || true
      echo
      curl -s -m 15 -X POST "$GATEWAY/order/save" -H 'Content-Type: application/json' -d '{"id":"1","name":"Tom"}' || true
      echo
      fail "timeout after ${timeout}s waiting: $desc"
    fi
  done
  info "ok: $desc"
}

resolved_refs_reason_is() {
  kubectl get httproute "$1" -o json \
    | jq -e --arg r "$2" '[.status.parents[]?.conditions[]? | select(.type == "ResolvedRefs")][0].reason == $r' >/dev/null
}

parent_names_are() {
  kubectl get httproute demo-http-route -o json \
    | jq -e --arg expected "$1" '[.status.parents[]?.parentRef.name] | join(" ") == $expected' >/dev/null
}

parents_empty() {
  [[ "$(kubectl get httproute demo-http-route -o jsonpath='{.status.parents}')" == "[]" ]]
}

log_has_stale_selector() {
  kubectl logs -n "$GW_NS" deployment/shenyu-gateway-api-controller --since=2m 2>/dev/null \
    | grep -q "stale selector"
}

log_upstream_count_is() {
  local line
  line=$(kubectl logs -n "$GW_NS" deployment/shenyu-gateway-api-controller --since=2m 2>/dev/null \
    | grep "Resolved .* upstream(s) for selector" | tail -1 || true)
  [[ "$line" =~ Resolved\ ([0-9]+)\ upstream ]] && [[ "${BASH_REMATCH[1]}" == "$1" ]]
}

step15_cross_namespace_grant() {
  info "step15: cross-namespace backendRef ReferenceGrant semantics"
  kubectl apply -f - >/dev/null <<'EOF'
apiVersion: gateway.networking.k8s.io/v1
kind: HTTPRoute
metadata:
  name: demo-cross-ns-route
  namespace: default
spec:
  parentRefs:
    - name: shenyu-gateway
  rules:
    - matches:
        - path:
            type: PathPrefix
            value: /crossns
      backendRefs:
        - name: shenyu-examples-http-service
          namespace: backend-ns
          port: 8189
EOF
  wait_for "no grant -> ResolvedRefs=False/RefNotPermitted" 240 resolved_refs_reason_is demo-cross-ns-route RefNotPermitted
  wait_for "crossns route has no selector" 30 expect_no_selector /crossns/order/save

  kubectl create namespace backend-ns >/dev/null 2>&1 || true
  kubectl apply -f - >/dev/null <<'EOF'
apiVersion: gateway.networking.k8s.io/v1beta1
kind: ReferenceGrant
metadata:
  name: allow-default-routes
  namespace: backend-ns
spec:
  from:
    - group: gateway.networking.k8s.io
      kind: HTTPRoute
      namespace: default
  to:
    - group: ""
      kind: Service
EOF
  wait_for "grant added -> BackendNotFound (no endpoints there)" 240 resolved_refs_reason_is demo-cross-ns-route BackendNotFound

  kubectl delete referencegrant allow-default-routes -n backend-ns >/dev/null
  wait_for "grant removed -> back to RefNotPermitted" 240 resolved_refs_reason_is demo-cross-ns-route RefNotPermitted

  kubectl delete httproute demo-cross-ns-route >/dev/null
  kubectl delete namespace backend-ns >/dev/null
}

step16_gatewayclass_deletion() {
  info "step16: GatewayClass deletion cascade cleanup and restore"
  kubectl delete gatewayclass shenyu >/dev/null
  wait_for "class deleted -> no selector" 60 expect_no_selector /order/save
  wait_for "route resync -> status.parents emptied" 240 parents_empty
  if kubectl logs -n "$GW_NS" deployment/shenyu-gateway-api-controller --since=2m 2>/dev/null | grep -q ConcurrentModificationException; then
    fail "ConcurrentModificationException during cascade cleanup"
  fi

  info "step16: CME check passed, restoring manifest"
  kubectl apply -f "$ROUTE_MANIFEST"
  info "step16: manifest re-applied, waiting for route recovery"
  wait_for "class restored -> route back" 240 expect_backend /order/save
  wait_for "status restored" 240 parent_names_are "shenyu-gateway"
}

step17_object_deletion() {
  info "step17: HTTPRoute and Gateway object deletion"
  kubectl delete httproute demo-http-route >/dev/null
  wait_for "route deleted -> no selector" 60 expect_no_selector /order/save
  kubectl apply -f "$ROUTE_MANIFEST" >/dev/null
  wait_for "route restored" 60 expect_backend /order/save

  kubectl delete gateway shenyu-gateway >/dev/null
  wait_for "gateway deleted -> cascade cleanup" 60 expect_no_selector /order/save
  wait_for "detach -> status.parents emptied" 240 parents_empty
  kubectl apply -f "$ROUTE_MANIFEST" >/dev/null
  wait_for "gateway restored -> accepted transition requeues route" 60 expect_backend /order/save
}

step18_wildcard_hostname() {
  info "step18: wildcard hostname matching and stale cleanup"
  kubectl patch httproute demo-http-route --type=merge -p '{"spec":{"hostnames":["*.wild.test"]}}' >/dev/null
  wait_for "wildcard subdomain matches" 60 expect_backend_with_host /order/save a.wild.test
  wait_for "another single-label subdomain matches" 30 expect_backend_with_host /order/save b.wild.test
  wait_for "bare domain rejected" 30 expect_no_selector_with_host /order/save wild.test
  wait_for "multi-label subdomain matches (suffix semantics)" 30 expect_backend_with_host /order/save x.a.wild.test
  wait_for "default host rejected while wildcard set" 30 expect_no_selector /order/save

  kubectl patch httproute demo-http-route --type=merge -p '{"spec":{"hostnames":null}}' >/dev/null
  sleep 5
  wait_for "stale wildcard selector deleted" 60 log_has_stale_selector
  wait_for "hostname restriction lifted" 30 expect_backend /order/save
}

expect_no_selector_with_host() {
  local body
  body=$(curl -s -m 15 -X POST "$GATEWAY$1" -H 'Content-Type: application/json' -H "Host: $2" -d '{"id":"1","name":"Tom"}' || true)
  [[ "$body" == *'"code":-107'* ]] || return 1
}

step19_spec_change() {
  info "step19: HTTPRoute spec change"
  kubectl patch httproute demo-http-route --type=merge \
    -p '{"spec":{"rules":[{"matches":[{"path":{"type":"PathPrefix","value":"/orders"}}],"backendRefs":[{"name":"shenyu-examples-http-service","port":8189}]}]}}' >/dev/null
  wait_for "new path active (backend 404, not gateway -107)" 60 bash -c \
    'code=$(curl -s -o /dev/null -w "%{http_code}" -m 15 -X POST http://localhost:30095/orders/save -H "Content-Type: application/json" -d "{}"); [[ "$code" == "404" ]]'
  wait_for "old path rejected" 30 expect_no_selector /order/save
  kubectl patch httproute demo-http-route --type=merge \
    -p '{"spec":{"rules":[{"matches":[{"path":{"type":"PathPrefix","value":"/order"}}],"backendRefs":[{"name":"shenyu-examples-http-service","port":8189}]}]}}' >/dev/null
  wait_for "path restored" 60 expect_backend /order/save

  kubectl patch httproute demo-http-route --type=merge \
    -p '{"spec":{"rules":[{"matches":[{"path":{"type":"PathPrefix","value":"/order"}}],"backendRefs":[{"name":"shenyu-examples-http-service","port":8189,"weight":70},{"name":"shenyu-examples-http-service","port":8189,"weight":30}]}]}}' >/dev/null
  wait_for "multi backendRef merged" 60 expect_backend /order/save
  kubectl apply -f "$ROUTE_MANIFEST" >/dev/null
  wait_for "single backendRef restored" 60 expect_backend /order/save
}

step20_backend_scaling() {
  info "step20: backend scale up/down convergence"
  kubectl scale deployment "$BACKEND_DEPLOYMENT" --replicas=2 >/dev/null
  kubectl wait --for=condition=available "deployment/$BACKEND_DEPLOYMENT" --timeout=120s >/dev/null
  wait_for "scale 2 -> two upstreams" 240 log_upstream_count_is 2

  kubectl scale deployment "$BACKEND_DEPLOYMENT" --replicas=1 >/dev/null
  wait_for "scale 1 -> one upstream (stale address shrunk)" 240 log_upstream_count_is 1
  wait_for "route still served after shrink" 30 expect_backend /order/save

  kubectl scale deployment "$BACKEND_DEPLOYMENT" --replicas=0 >/dev/null
  wait_for "scale 0 -> BackendNotFound" 240 resolved_refs_reason_is demo-http-route BackendNotFound
  wait_for "scale 0 -> no selector" 60 expect_no_selector /order/save

  info "step20: scaling back to 1"
  kubectl scale deployment "$BACKEND_DEPLOYMENT" --replicas=1 >/dev/null
  info "step20: waiting for backend availability"
  kubectl wait --for=condition=available "deployment/$BACKEND_DEPLOYMENT" --timeout=180s >/dev/null
  info "step20: backend available, waiting for route recovery"
  wait_for "restored -> route back" 240 expect_backend /order/save
}

step21_multi_parent() {
  info "step21: multi-parent Gateway partial deletion"
  kubectl apply -f - >/dev/null <<'EOF'
apiVersion: gateway.networking.k8s.io/v1
kind: Gateway
metadata:
  name: shenyu-gateway-2
  namespace: default
spec:
  gatewayClassName: shenyu
  listeners:
    - name: http
      port: 9195
      protocol: HTTP
      allowedRoutes:
        namespaces:
          from: Same
EOF
  kubectl patch httproute demo-http-route --type=merge \
    -p '{"spec":{"parentRefs":[{"name":"shenyu-gateway"},{"name":"shenyu-gateway-2"}]}}' >/dev/null
  wait_for "dual parents in status" 60 parent_names_are "shenyu-gateway shenyu-gateway-2"
  wait_for "route served with dual parents" 60 expect_backend /order/save

  kubectl delete gateway shenyu-gateway >/dev/null
  wait_for "gw1 deleted -> config kept by gw2" 60 expect_backend /order/save
  wait_for "status converged to remaining parent" 240 parent_names_are "shenyu-gateway-2"

  kubectl delete gateway shenyu-gateway-2 >/dev/null
  wait_for "all parents deleted -> cleanup" 60 expect_no_selector /order/save

  kubectl apply -f "$ROUTE_MANIFEST" >/dev/null
  wait_for "restored" 240 expect_backend /order/save
  wait_for "single parent restored" 240 parent_names_are "shenyu-gateway"
}

# Ensure the baseline is healthy so the script is rerunnable after a partial failure
# (e.g. a previous run exited after deleting the GatewayClass).
preflight() {
  info "preflight: ensure baseline healthy"
  kubectl apply -f "$ROUTE_MANIFEST" >/dev/null
  kubectl scale deployment "$BACKEND_DEPLOYMENT" --replicas=1 >/dev/null 2>&1 || true
  kubectl wait --for=condition=available "deployment/$BACKEND_DEPLOYMENT" --timeout=120s >/dev/null
  wait_for "baseline route healthy" 240 expect_backend /order/save
}

main() {
  preflight
  step15_cross_namespace_grant
  step16_gatewayclass_deletion
  step17_object_deletion
  step18_wildcard_hostname
  step19_spec_change
  step20_backend_scaling
  step21_multi_parent
  info "all lifecycle scenarios passed"
}

main "$@"
