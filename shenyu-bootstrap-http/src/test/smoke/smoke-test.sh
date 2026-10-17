#!/usr/bin/env bash
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
# Smoke test for shenyu-bootstrap-http (issue #7649).
#
# Covers the acceptance scenarios:
#   T1 create : selector+rule created in admin, upstream attached via the client
#               register-uri endpoint -> synced via websocket -> HTTP proxy succeeds
#   T2 offline: upstream offlined via register-uri (eventType=OFFLINE) -> synced ->
#               gateway answers CANNOT_FIND_HEALTHY_UPSTREAM_URL (-119)
#   T3 online : upstream re-registered -> synced -> HTTP proxy succeeds again
#   T5 failure: stub backend killed while registered -> connect refused -> HTTP 500
#   T4 delete : rule+selector deleted -> synced -> gateway answers SELECTOR_NOT_FOUND (-107)
#
# Why register-uri for upstreams: in current master the gateway only learns divide
# upstreams from DISCOVER_UPSTREAM sync events (DivideUpstreamDataHandler); a
# manually maintained selector handle is never parsed gateway-side. The admin only
# publishes DISCOVER_UPSTREAM for client registrations (/shenyu-client/register-uri)
# and discovery-bound selectors, so the smoke test drives upstream lifecycle through
# the register API exactly like a real Spring MVC client would.
#
# Prerequisites (all local):
#   - shenyu-admin running on $ADMIN_URL (default H2 profile, websocket sync enabled)
#   - shenyu-bootstrap-http running on $GATEWAY_URL
#   - a JDK on PATH (for the stub backend and to have built the jars)
#   - curl (Git Bash bundled curl is fine); no jq/python needed
#
# Usage:
#   bash shenyu-bootstrap-http/src/test/smoke/smoke-test.sh
#   ADMIN_URL=http://localhost:9095 GATEWAY_URL=http://localhost:9195 \
#       ADMIN_USER=admin ADMIN_PASSWORD=123456 SYNC_WAIT=3 bash smoke-test.sh
#
# Exit code: 0 when all assertions pass, 1 otherwise, 2 on preflight failure.

set -euo pipefail

ADMIN_URL="${ADMIN_URL:-http://localhost:9095}"
GATEWAY_URL="${GATEWAY_URL:-http://localhost:9195}"
ADMIN_USER="${ADMIN_USER:-admin}"
ADMIN_PASSWORD="${ADMIN_PASSWORD:-123456}"
NAMESPACE_ID="${NAMESPACE_ID:-649330b6-c2d7-4edc-be8e-8a54df9eb385}"
BACKEND_PORT="${BACKEND_PORT:-18080}"
SYNC_WAIT="${SYNC_WAIT:-3}"
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"

MARKER="smoke-backend-ok"
# selector name doubles as the register-uri contextPath: doRegisterURI looks the
# selector up by (contextPath, plugin=divide, namespaceId), so they must match
SELECTOR_NAME="/smoke"
RULE_NAME="smoke-divide-rule"
SMOKE_PATH="/smoke/hello"
APP_NAME="smoke-app"
CONTEXT_PATH="/smoke"

PASSED=0
FAILED=0
TOKEN=""
SEL_ID=""
RULE_ID=""
BACKEND_PID=""
DIVIDE_PLUGIN_ID=""
REG_DONE=""

pass()    { PASSED=$((PASSED + 1)); echo "  [PASS] $*"; }
fail()    { FAILED=$((FAILED + 1)); echo "  [FAIL] $*"; }
section() { echo; echo "== $* =="; }

# first "key":"value" occurrence on stdin -> value (never fails under set -e)
json_field() { grep -o "\"$1\":\"[^\"]*\"" | head -1 | cut -d'"' -f4 || true; }

# id of the first element of "dataList":[...] on stdin
list_first_id() { sed 's/.*"dataList":\[//' | json_field id; }

# escape a string for embedding inside a JSON string value
escape_json() { local s="$1"; s="${s//\\/\\\\}"; s="${s//\"/\\\"}"; printf '%s' "$s"; }

admin_curl() { curl -s --max-time 15 -H "X-Access-Token: $TOKEN" -H 'Content-Type: application/json' "$@"; }

# selector starts with an empty handle; upstreams are attached via register-uri
selector_body() {
  printf '{"pluginId":"%s","name":"%s","matchMode":0,"type":1,"sort":990,"enabled":true,"loged":false,"continued":true,"handle":"","selectorConditions":[{"paramType":"uri","operator":"match","paramName":"/","paramValue":"/smoke/**"}],"matchRestful":false,"namespaceId":"%s"}' \
    "$DIVIDE_PLUGIN_ID" "$SELECTOR_NAME" "$NAMESPACE_ID"
}

# drive upstream lifecycle through the client register API;
# $1 = REGISTER|OFFLINE, $2 = backend port.
# /shenyu-client/** is not on the shiro white-list, so the admin token is required
register_uri() {
  admin_curl -X POST "$ADMIN_URL/shenyu-client/register-uri" \
    -d "{\"protocol\":\"http://\",\"appName\":\"$APP_NAME\",\"contextPath\":\"$CONTEXT_PATH\",\"rpcType\":\"http\",\"host\":\"localhost\",\"port\":$2,\"eventType\":\"$1\",\"namespaceId\":\"$NAMESPACE_ID\"}"
}

rule_body() {
  # divide rules must carry a handle: the gateway deserializes it into
  # DivideRuleHandle and dereferences it unconditionally (NPE -> HTTP 500
  # when absent). retry=0 keeps the dead-upstream case failing fast.
  local handle
  handle="$(escape_json '{"loadBalance":"random","retry":0,"timeout":3000,"headerMaxSize":10240,"requestMaxSize":102400,"retryStrategy":"current"}')"
  printf '{"selectorId":"%s","name":"%s","matchMode":0,"enabled":true,"loged":false,"sort":1,"handle":"%s","ruleConditions":[{"paramType":"uri","operator":"match","paramName":"/","paramValue":"/smoke/**"}],"matchRestful":false,"namespaceId":"%s"}' \
    "$SEL_ID" "$RULE_NAME" "$handle" "$NAMESPACE_ID"
}

# call the gateway once; sets GW_CODE / GW_BODY
gateway_get() {
  local out
  out="$(curl -s --max-time 20 -w $'\n%{http_code}' "$GATEWAY_URL$SMOKE_PATH")"
  GW_CODE="${out##*$'\n'}"
  GW_BODY="${out%$'\n'*}"
}

delete_rule_and_selector() {
  if [ -n "$RULE_ID" ]; then
    admin_curl -X DELETE "$ADMIN_URL/rule/batch" \
      -d "{\"ids\":[\"$RULE_ID\"],\"namespaceId\":\"$NAMESPACE_ID\"}" >/dev/null 2>&1 || true
    RULE_ID=""
  fi
  if [ -n "$SEL_ID" ]; then
    admin_curl -X DELETE "$ADMIN_URL/selector/batch" \
      -d "{\"ids\":[\"$SEL_ID\"],\"namespaceId\":\"$NAMESPACE_ID\"}" >/dev/null 2>&1 || true
    SEL_ID=""
  fi
}

cleanup() {
  # detach registered upstreams first so the admin drops its health-check state
  # and discovery rows for the smoke selector, then remove the route itself
  if [ -n "$REG_DONE" ]; then
    register_uri OFFLINE "$BACKEND_PORT" >/dev/null 2>&1 || true
  fi
  delete_rule_and_selector
  if [ -n "$BACKEND_PID" ]; then kill "$BACKEND_PID" 2>/dev/null || true; fi
}
trap cleanup EXIT

# ---------------------------------------------------------------- preflight
section "0. preflight"
curl -sf --max-time 10 "$ADMIN_URL/actuator/health" >/dev/null \
  || { echo "admin not reachable at $ADMIN_URL - start shenyu-admin first"; exit 2; }
curl -sf --max-time 10 "$GATEWAY_URL/actuator/health" >/dev/null \
  || { echo "gateway not reachable at $GATEWAY_URL - start shenyu-bootstrap-http first"; exit 2; }
echo "  admin   : $ADMIN_URL OK"
echo "  gateway : $GATEWAY_URL OK"

BACKEND_JAVA="$SCRIPT_DIR/HttpStubBackend.java"
command -v cygpath >/dev/null 2>&1 && BACKEND_JAVA="$(cygpath -w "$BACKEND_JAVA")"
java "$BACKEND_JAVA" "$BACKEND_PORT" &
BACKEND_PID=$!
sleep 2
curl -sf --max-time 5 "http://localhost:$BACKEND_PORT$SMOKE_PATH" | grep -q "$MARKER" \
  || { echo "stub backend failed to start on port $BACKEND_PORT"; exit 2; }
echo "  backend : http://localhost:$BACKEND_PORT OK (pid $BACKEND_PID)"

# ---------------------------------------------------------------- admin login
section "1. admin login"
TOKEN="$(curl -s --max-time 15 -X POST "$ADMIN_URL/platform/login" \
    -H 'Content-Type: application/json' \
    -d "{\"userName\":\"$ADMIN_USER\",\"password\":\"$ADMIN_PASSWORD\"}" | json_field token)"
[ -n "$TOKEN" ] && pass "login, token acquired" || { fail "login failed"; exit 1; }

DIVIDE_PLUGIN_ID="$(admin_curl "$ADMIN_URL/plugin-template?name=divide&currentPage=1&pageSize=10" | list_first_id)"
[ -n "$DIVIDE_PLUGIN_ID" ] && pass "divide plugin id = $DIVIDE_PLUGIN_ID" || { fail "divide plugin not found"; exit 1; }

# sanity: route does not exist yet -> divide has no selector -> -107 (informational only)
gateway_get
echo "  before create: gateway answers HTTP $GW_CODE body=$GW_BODY"

# ---------------------------------------------------------------- T1 create
section "T1. create route + register upstream -> proxy success"
SEL_ID="$(admin_curl -X POST "$ADMIN_URL/selector" -d "$(selector_body)" | json_field data)"
if [ -z "$SEL_ID" ]; then # fallback: fetch by name
  SEL_ID="$(admin_curl "$ADMIN_URL/selector?pluginId=$DIVIDE_PLUGIN_ID&name=%2Fsmoke&currentPage=1&pageSize=10" | list_first_id)"
fi
[ -n "$SEL_ID" ] && pass "selector created, id = $SEL_ID" || { fail "selector create failed"; exit 1; }

admin_curl -X POST "$ADMIN_URL/rule" -d "$(rule_body)" >/dev/null
RULE_ID="$(admin_curl "$ADMIN_URL/rule?selectorId=$SEL_ID&currentPage=1&pageSize=10&namespaceId=$NAMESPACE_ID" | list_first_id)"
[ -n "$RULE_ID" ] && pass "rule created, id = $RULE_ID" || { fail "rule create failed"; exit 1; }

REG_DONE=1
REG_RESULT="$(register_uri REGISTER "$BACKEND_PORT")"
[ "$REG_RESULT" = "success" ] && pass "upstream registered (register-uri accepted)" \
  || fail "register-uri answered: $REG_RESULT"

sleep "$SYNC_WAIT"
gateway_get
if [ "$GW_CODE" = "200" ] && echo "$GW_BODY" | grep -q "$MARKER"; then
  pass "create synced: HTTP 200, proxied body = $GW_BODY"
else
  fail "create sync: expected HTTP 200 with marker, got HTTP $GW_CODE body=$GW_BODY"
fi

# ---------------------------------------------------------------- T2 offline upstream
section "T2. offline upstream -> no healthy upstream (-119)"
register_uri OFFLINE "$BACKEND_PORT" >/dev/null
sleep "$SYNC_WAIT"
gateway_get
if echo "$GW_BODY" | grep -q '"code":-119'; then
  pass "offline synced: gateway answers -119 (no healthy upstream), body=$GW_BODY"
else
  fail "offline sync: expected -119 body, got HTTP $GW_CODE body=$GW_BODY"
fi

# ---------------------------------------------------------------- T3 re-register upstream
section "T3. re-register upstream -> proxy success again"
register_uri REGISTER "$BACKEND_PORT" >/dev/null
sleep "$SYNC_WAIT"
gateway_get
if [ "$GW_CODE" = "200" ] && echo "$GW_BODY" | grep -q "$MARKER"; then
  pass "re-register synced: HTTP 200 with marker again"
else
  fail "re-register: expected HTTP 200 with marker, got HTTP $GW_CODE body=$GW_BODY"
fi

# ---------------------------------------------------------------- T5 dead backend
section "T5. kill registered backend -> connect refused (HTTP 500)"
# the gateway still holds the upstream as healthy until the admin health check
# (~10s) notices; a request right after the kill must hit the dead port and 500
kill "$BACKEND_PID" 2>/dev/null || true
wait "$BACKEND_PID" 2>/dev/null || true
BACKEND_PID=""
gateway_get
if [ "$GW_CODE" = "500" ]; then
  pass "dead backend yields HTTP 500"
else
  fail "dead backend: expected HTTP 500, got HTTP $GW_CODE body=$GW_BODY"
fi

# ---------------------------------------------------------------- T4 delete
section "T4. delete rule+selector -> SELECTOR_NOT_FOUND (-107)"
delete_rule_and_selector
sleep "$SYNC_WAIT"
gateway_get
if echo "$GW_BODY" | grep -q '"code":-107' && ! echo "$GW_BODY" | grep -q "$MARKER"; then
  pass "delete synced: gateway answers -107 (selector not found), body=$GW_BODY"
else
  fail "delete sync: expected -107 body, got HTTP $GW_CODE body=$GW_BODY"
fi

# ---------------------------------------------------------------- summary
section "summary"
echo "  passed: $PASSED, failed: $FAILED"
[ "$FAILED" -eq 0 ]
