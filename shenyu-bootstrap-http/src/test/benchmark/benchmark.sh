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
# Benchmark: full shenyu-bootstrap vs minimal shenyu-bootstrap-http (issue #7649).
#
# Collected per variant, on the same machine, sequentially:
#   - package size (fat jar bytes, bundled jar count)
#   - cold start: time to health endpoint and time to first successful proxied
#     request, RUNS_COLD repetitions (run 1 = cold, runs 2..N = warm)
#   - idle baseline: RSS and CPU right after startup
#   - loaded classes (post-start and post-load, via jstat)
#   - steady-state: closed-loop throughput / latency at WORKERS_LIST concurrency
#     levels, plus RSS / CPU after the highest level
#
# Prerequisites:
#   - shenyu-admin running on $ADMIN_URL
#   - both jars built (see README.md; full bootstrap needs the repackage goal run once)
#   - JDK on PATH (java, jstat), curl, powershell (Windows RSS/CPU sampling)
#
# Usage:
#   bash shenyu-bootstrap-http/src/test/benchmark/benchmark.sh
#   QUICK=1 bash benchmark.sh        # short smoke of the harness itself
#
# Env knobs: ADMIN_URL GATEWAY_PORT BACKEND_PORT RUNS_COLD WORKERS_LIST
#            WARMUP_SEC MEASURE_SEC IDLE_CPU_WINDOW_SEC JAVA_OPTS
#            VARIANTS (default "full minimal"; RUNS_COLD=0 skips cold starts)
#            GATEWAY_SYSPROPS (default -Dshenyu.httpclient.keepAlive=true, see §5)
#            SHENYU_SYNC_WEBSOCKET_TOKEN (inherited by both gateways, must match admin)

set -euo pipefail

ADMIN_URL="${ADMIN_URL:-http://localhost:9095}"
GATEWAY_PORT="${GATEWAY_PORT:-9195}"
BACKEND_PORT="${BACKEND_PORT:-18081}"
ADMIN_USER="${ADMIN_USER:-admin}"
ADMIN_PASSWORD="${ADMIN_PASSWORD:-123456}"
NAMESPACE_ID="${NAMESPACE_ID:-649330b6-c2d7-4edc-be8e-8a54df9eb385}"
FULL_JAR="${FULL_JAR:-shenyu-bootstrap/target/shenyu-bootstrap.jar}"
MIN_JAR="${MIN_JAR:-shenyu-bootstrap-http/target/shenyu-bootstrap-http-exec.jar}"
JAVA_OPTS="${JAVA_OPTS:-}"
# Applied to BOTH gateways. keepAlive=true deviates deliberately from the
# as-shipped default (false): with per-request connections to the backend,
# Windows loopback aborts a rate-dependent share of connections
# (WSAECONNABORTED -> PrematureCloseException -> HTTP 500), poisoning the
# errors column proportionally to each variant's speed (measured: 81% full vs
# 19% minimal at w=8, 0.28% at w=2, 0% with keepAlive=true) and making the
# comparison meaningless. See README section 5.
GATEWAY_SYSPROPS="${GATEWAY_SYSPROPS:--Dshenyu.httpclient.keepAlive=true}"
SYNC_WAIT="${SYNC_WAIT:-3}"

if [ "${QUICK:-0}" = "1" ]; then
  RUNS_COLD=2; WORKERS_LIST="8"; WARMUP_SEC=10; MEASURE_SEC=15; IDLE_CPU_WINDOW_SEC=5
else
  RUNS_COLD="${RUNS_COLD:-5}"
  WORKERS_LIST="${WORKERS_LIST:-8 32 128}"
  WARMUP_SEC="${WARMUP_SEC:-60}"
  MEASURE_SEC="${MEASURE_SEC:-60}"
  IDLE_CPU_WINDOW_SEC="${IDLE_CPU_WINDOW_SEC:-10}"
fi
# space-separated subset of "full minimal"; RUNS_COLD=0 skips the cold-start loop
VARIANTS="${VARIANTS:-full minimal}"

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
RESULTS_DIR="$SCRIPT_DIR/../../../target/benchmark"
mkdir -p "$RESULTS_DIR"
TS="$(date +%Y%m%d-%H%M%S)"
CSV="$RESULTS_DIR/bench-$TS.csv"
META="$RESULTS_DIR/bench-$TS.meta.txt"

MARKER="bench-backend-ok"
# selector name doubles as the register-uri contextPath: doRegisterURI looks the
# selector up by (contextPath, plugin=divide, namespaceId), so they must match
SELECTOR_NAME="/bench"
RULE_NAME="bench-divide-rule"
ROUTE_PATH="/bench/hello"
APP_NAME="bench-app"
CONTEXT_PATH="/bench"

TOKEN=""
SEL_ID=""
RULE_ID=""
DIVIDE_PLUGIN_ID=""
BACKEND_PID=""
GW_PID=""
REG_DONE=""

section() { echo; echo "== $* =="; }
log()     { echo "  $*"; }

# ---------------------------------------------------------------- csv/meta
csv() { echo "$*" >> "$CSV"; }
echo "variant,kind,label,v1,v2,v3,v4,v5,v6,v7,v8" > "$CSV"

# ---------------------------------------------------------------- helpers
json_field() { grep -o "\"$1\":\"[^\"]*\"" | head -1 | cut -d'"' -f4 || true; }
list_first_id() { sed 's/.*"dataList":\[//' | json_field id; }
escape_json() { local s="$1"; s="${s//\\/\\\\}"; s="${s//\"/\\\"}"; printf '%s' "$s"; }
admin_curl() { curl -s --max-time 15 -H "X-Access-Token: $TOKEN" -H 'Content-Type: application/json' "$@"; }

# drive upstream lifecycle through the client register API;
# $1 = REGISTER|OFFLINE, $2 = backend port.
# /shenyu-client/** is not on the shiro white-list, so the admin token is required
register_uri() {
  admin_curl -X POST "$ADMIN_URL/shenyu-client/register-uri" \
    -d "{\"protocol\":\"http://\",\"appName\":\"$APP_NAME\",\"contextPath\":\"$CONTEXT_PATH\",\"rpcType\":\"http\",\"host\":\"localhost\",\"port\":$2,\"eventType\":\"$1\",\"namespaceId\":\"$NAMESPACE_ID\"}"
}

now_ms() {
  local t
  t="$(date +%s%3N 2>/dev/null)"
  case "$t" in *N*) echo $((SECONDS * 1000)) ;; *) echo "$t" ;; esac
}

# MSYS2 hands out its own pids for background jobs; powershell and jstat need
# the real Windows pid. On platforms without /proc/<pid>/winpid fall back to
# the pid unchanged.
winpid() { local p; p="$(cat /proc/"$1"/winpid 2>/dev/null)"; echo "${p:-$1}"; }

ps_rss_mb() { # Windows working set of pid $1, in MB
  powershell -NoProfile -Command "[math]::Round((Get-Process -Id $(winpid "$1")).WorkingSet64/1MB,1)" 2>/dev/null | tr -d '\r' || echo ""
}

ps_cpu_cores() { # cumulative CPU seconds of pid $1
  powershell -NoProfile -Command "(Get-Process -Id $(winpid "$1")).CPU" 2>/dev/null | tr -d '\r' || echo ""
}

# average busy cores over a window of $1 seconds for pid $2
cpu_over_window() {
  local c0 c1
  c0="$(ps_cpu_cores "$2")"; sleep "$1"; c1="$(ps_cpu_cores "$2")"
  awk -v a="$c0" -v b="$c1" -v w="$1" 'BEGIN{printf "%.3f", (b-a)/w}'
}

loaded_classes() { jstat -class "$(winpid "$1")" 2>/dev/null | awk 'NR==2{print $1-$3}' || echo ""; }

median3() { printf '%s\n%s\n%s\n' "$1" "$2" "$3" | sort -n | awk 'NR==2'; }

start_gateway() { # $1 = jar path, $2 = variant name; sets GW_PID
  java $GATEWAY_SYSPROPS $JAVA_OPTS -jar "$1" > "$RESULTS_DIR/gateway-$2-$TS.log" 2>&1 &
  GW_PID=$!
}

wait_health() { # $1 = timeout sec; returns 0 when /actuator/health answers
  local deadline=$(( $(now_ms) + $1 * 1000 ))
  while [ "$(now_ms)" -lt "$deadline" ]; do
    curl -sf --max-time 2 "http://localhost:$GATEWAY_PORT/actuator/health" >/dev/null 2>&1 && return 0
    sleep 0.05
  done
  return 1
}

wait_proxy() { # $1 = timeout sec; returns 0 when proxied response carries the marker
  local deadline=$(( $(now_ms) + $1 * 1000 ))
  while [ "$(now_ms)" -lt "$deadline" ]; do
    curl -s --max-time 2 "http://localhost:$GATEWAY_PORT$ROUTE_PATH" 2>/dev/null | grep -q "$MARKER" && return 0
    sleep 0.05
  done
  return 1
}

stop_gateway() {
  [ -z "$GW_PID" ] && return 0
  kill "$GW_PID" 2>/dev/null || true
  for _ in $(seq 1 40); do kill -0 "$GW_PID" 2>/dev/null || break; sleep 0.5; done
  kill -9 "$GW_PID" 2>/dev/null || true
  wait "$GW_PID" 2>/dev/null || true
  GW_PID=""
  for _ in $(seq 1 40); do
    curl -sf --max-time 2 "http://localhost:$GATEWAY_PORT/actuator/health" >/dev/null 2>&1 || break
    sleep 0.5
  done
}

selector_body() {
  # selector starts with an empty handle; the upstream is attached via
  # register-uri below. In current master the gateway only learns divide
  # upstreams from DISCOVER_UPSTREAM sync events (DivideUpstreamDataHandler);
  # a selector's handle JSON is never parsed gateway-side, and the admin
  # publishes DISCOVER_UPSTREAM only for client registrations and
  # discovery-bound selectors.
  printf '{"pluginId":"%s","name":"%s","matchMode":0,"type":1,"sort":980,"enabled":true,"loged":false,"continued":true,"handle":"","selectorConditions":[{"paramType":"uri","operator":"match","paramName":"/","paramValue":"/bench/**"}],"matchRestful":false,"namespaceId":"%s"}' \
    "$DIVIDE_PLUGIN_ID" "$SELECTOR_NAME" "$NAMESPACE_ID"
}

rule_body() {
  # divide rules must carry a handle: the gateway deserializes it into
  # DivideRuleHandle and dereferences it unconditionally (NPE -> HTTP 500
  # when absent). retry=0 keeps failures fast.
  local handle
  handle="$(escape_json '{"loadBalance":"random","retry":0,"timeout":3000,"headerMaxSize":10240,"requestMaxSize":102400,"retryStrategy":"current"}')"
  printf '{"selectorId":"%s","name":"%s","matchMode":0,"enabled":true,"loged":false,"sort":1,"handle":"%s","ruleConditions":[{"paramType":"uri","operator":"match","paramName":"/","paramValue":"/bench/**"}],"matchRestful":false,"namespaceId":"%s"}' \
    "$SEL_ID" "$RULE_NAME" "$handle" "$NAMESPACE_ID"
}

cleanup() {
  stop_gateway
  # detach the registered upstream first so the admin drops its health-check
  # state and discovery rows for the bench selector, then remove the route
  if [ -n "$REG_DONE" ]; then
    register_uri OFFLINE "$BACKEND_PORT" >/dev/null 2>&1 || true
  fi
  if [ -n "$RULE_ID" ]; then
    admin_curl -X DELETE "$ADMIN_URL/rule/batch" -d "{\"ids\":[\"$RULE_ID\"],\"namespaceId\":\"$NAMESPACE_ID\"}" >/dev/null 2>&1 || true
  fi
  if [ -n "$SEL_ID" ]; then
    admin_curl -X DELETE "$ADMIN_URL/selector/batch" -d "{\"ids\":[\"$SEL_ID\"],\"namespaceId\":\"$NAMESPACE_ID\"}" >/dev/null 2>&1 || true
  fi
  [ -n "$BACKEND_PID" ] && kill "$BACKEND_PID" 2>/dev/null || true
}
trap cleanup EXIT

# ---------------------------------------------------------------- preflight
section "0. preflight"
for j in "$FULL_JAR" "$MIN_JAR"; do
  [ -f "$j" ] || { echo "missing jar: $j (see README.md build instructions)"; exit 2; }
done
curl -sf --max-time 10 "$ADMIN_URL/actuator/health" >/dev/null || { echo "admin not reachable at $ADMIN_URL"; exit 2; }
command -v jstat >/dev/null || { echo "jstat not on PATH (need a JDK, not a JRE)"; exit 2; }
command -v powershell >/dev/null || { echo "powershell not found (needed for RSS/CPU sampling on Windows)"; exit 2; }

BACKEND_JAVA="$SCRIPT_DIR/BenchBackend.java"
DRIVER_JAVA="$SCRIPT_DIR/LoadDriver.java"
command -v cygpath >/dev/null 2>&1 && { BACKEND_JAVA="$(cygpath -w "$BACKEND_JAVA")"; DRIVER_JAVA="$(cygpath -w "$DRIVER_JAVA")"; }
java "$BACKEND_JAVA" "$BACKEND_PORT" &
BACKEND_PID=$!
sleep 2
# the marker probe alone cannot tell our fresh backend from a stale listener
# squatting on the port (the new process dies on bind and the old one answers);
# make sure the process we started is actually alive
kill -0 "$BACKEND_PID" 2>/dev/null || { echo "backend failed to start on $BACKEND_PORT (port already in use?)"; exit 2; }
curl -sf --max-time 5 "http://localhost:$BACKEND_PORT$ROUTE_PATH" | grep -q "$MARKER" || { echo "backend failed on $BACKEND_PORT"; exit 2; }
log "backend up on :$BACKEND_PORT"

TOKEN="$(curl -s --max-time 15 -X POST "$ADMIN_URL/platform/login" -H 'Content-Type: application/json' \
    -d "{\"userName\":\"$ADMIN_USER\",\"password\":\"$ADMIN_PASSWORD\"}" | json_field token)"
[ -n "$TOKEN" ] || { echo "admin login failed"; exit 2; }
DIVIDE_PLUGIN_ID="$(admin_curl "$ADMIN_URL/plugin-template?name=divide&currentPage=1&pageSize=10" | list_first_id)"
[ -n "$DIVIDE_PLUGIN_ID" ] || { echo "divide plugin not found"; exit 2; }

# one route for the whole benchmark; both gateways sync it from the same admin
SEL_ID="$(admin_curl -X POST "$ADMIN_URL/selector" -d "$(selector_body)" | json_field data)"
if [ -z "$SEL_ID" ]; then
  SEL_ID="$(admin_curl "$ADMIN_URL/selector?pluginId=$DIVIDE_PLUGIN_ID&name=%2Fbench&currentPage=1&pageSize=10" | list_first_id)"
fi
admin_curl -X POST "$ADMIN_URL/rule" -d "$(rule_body)" >/dev/null
RULE_ID="$(admin_curl "$ADMIN_URL/rule?selectorId=$SEL_ID&currentPage=1&pageSize=10&namespaceId=$NAMESPACE_ID" | list_first_id)"
[ -n "$SEL_ID" ] && [ -n "$RULE_ID" ] || { echo "route setup failed"; exit 2; }
log "route created: selector=$SEL_ID rule=$RULE_ID"

REG_DONE=1
REG_RESULT="$(register_uri REGISTER "$BACKEND_PORT")"
[ "$REG_RESULT" = "success" ] || { echo "register-uri answered: $REG_RESULT"; exit 2; }
log "upstream registered via register-uri (synced to every gateway start below)"

{
  echo "date:            $(date -Is)"
  echo "os:              $(uname -a)"
  echo "cpu cores:       $(nproc)"
  echo "java:            $(java -version 2>&1 | head -1)"
  echo "JAVA_OPTS:       ${JAVA_OPTS:-<empty>}"
  echo "GATEWAY_SYSPROPS: ${GATEWAY_SYSPROPS:-<empty>}"
  echo "admin:           $ADMIN_URL"
  echo "gateway port:    $GATEWAY_PORT"
  echo "backend port:    $BACKEND_PORT"
  echo "runs cold:       $RUNS_COLD"
  echo "workers:         $WORKERS_LIST"
  echo "warmup/measure:  ${WARMUP_SEC}s / ${MEASURE_SEC}s"
  echo "full jar:        $FULL_JAR"
  echo "minimal jar:     $MIN_JAR"
} | tee "$META"

# ---------------------------------------------------------------- per-variant run
run_variant() { # $1 = variant name, $2 = jar path
  local variant="$1" jar="$2"
  section "variant: $variant"

  local bytes jars
  bytes="$(stat -c %s "$jar")"
  jars="$(unzip -l "$jar" | grep -c 'BOOT-INF/lib/.*\.jar' || true)"
  csv "$variant,package,jar,$bytes,$jars,,,,,,"
  log "package: $bytes bytes, $jars bundled jars"

  local run t0 t_health t_proxy
  for run in $(seq 1 "$RUNS_COLD"); do
    t0="$(now_ms)"
    start_gateway "$jar" "$variant"
    if wait_health 180; then t_health=$(( $(now_ms) - t0 )); else t_health=-1; fi
    if [ "$t_health" -ge 0 ] && wait_proxy 60; then t_proxy=$(( $(now_ms) - t0 )); else t_proxy=-1; fi
    csv "$variant,cold_start,run$run,$t_health,$t_proxy,,,,,,"
    log "cold start run$run: health=${t_health}ms proxy=${t_proxy}ms"
    stop_gateway
  done

  start_gateway "$jar" "$variant"
  wait_health 180 || { echo "  gateway failed to start for steady-state phase"; stop_gateway; return 1; }

  local cls_start rss_i1 rss_i2 rss_i3 idle_rss idle_mcpu
  cls_start="$(loaded_classes "$GW_PID")"
  sleep 5
  rss_i1="$(ps_rss_mb "$GW_PID")"; sleep 2; rss_i2="$(ps_rss_mb "$GW_PID")"; sleep 2; rss_i3="$(ps_rss_mb "$GW_PID")"
  idle_rss="$(median3 "$rss_i1" "$rss_i2" "$rss_i3")"
  idle_mcpu="$(cpu_over_window "$IDLE_CPU_WINDOW_SEC" "$GW_PID")"
  csv "$variant,idle,baseline,$idle_rss,$idle_mcpu,$cls_start,,,,,"
  log "idle: rss=${idle_rss}MB cpu=${idle_mcpu} cores loadedClasses=$cls_start"

  local w line reqs errs rps p50 p90 p99 pmax mcpu_load c0 c1 rss_after
  for w in $WORKERS_LIST; do
    c0="$(ps_cpu_cores "$GW_PID")"
    line="$(java "$DRIVER_JAVA" "http://localhost:$GATEWAY_PORT$ROUTE_PATH" "$w" "$WARMUP_SEC" "$MEASURE_SEC" "$MARKER")"
    c1="$(ps_cpu_cores "$GW_PID")"
    IFS=, read -r reqs errs rps p50 p90 p99 pmax <<< "$line"
    mcpu_load="$(awk -v a="$c0" -v b="$c1" -v t="$((WARMUP_SEC + MEASURE_SEC))" 'BEGIN{printf "%.3f",(b-a)/t}')"
    rss_after="$(ps_rss_mb "$GW_PID")"
    csv "$variant,load,workers=$w,$reqs,$errs,$rps,$p50,$p90,$p99,$pmax,$mcpu_load,$rss_after"
    log "load w=$w: rps=$rps p50=${p50}us p99=${p99}us err=$errs cpu=${mcpu_load}cores rss=${rss_after}MB"
  done

  local cls_load rss_s1 rss_s2 rss_s3 steady_rss
  cls_load="$(loaded_classes "$GW_PID")"
  rss_s1="$(ps_rss_mb "$GW_PID")"; sleep 2; rss_s2="$(ps_rss_mb "$GW_PID")"; sleep 2; rss_s3="$(ps_rss_mb "$GW_PID")"
  steady_rss="$(median3 "$rss_s1" "$rss_s2" "$rss_s3")"
  csv "$variant,steady,post_load,$steady_rss,$cls_load,,,,,,"
  log "steady: rss=${steady_rss}MB loadedClasses=$cls_load"
  stop_gateway
}

for v in $VARIANTS; do
  case "$v" in
    full)    run_variant full "$FULL_JAR" ;;
    minimal) run_variant minimal "$MIN_JAR" ;;
    *) echo "unknown variant: $v"; exit 2 ;;
  esac
done

section "done"
echo "  csv:  $CSV"
echo "  meta: $META"
