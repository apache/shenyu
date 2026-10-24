<!--
  ~ Licensed to the Apache Software Foundation (ASF) under one or more
  ~ contributor license agreements.  See the NOTICE file distributed with
  ~ this work for additional information regarding copyright ownership.
  ~ The ASF licenses this file to You under the Apache License, Version 2.0
  ~ (the "License"); you may not use this file except in compliance with
  ~ the License.  You may obtain a copy of the License at
  ~
  ~     http://www.apache.org/licenses/LICENSE-2.0
  ~
  ~ Unless required by applicable law or agreed to in writing, software
  ~ distributed under the License is distributed on an "AS IS" BASIS,
  ~ WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
  ~ See the License for the specific language governing permissions and
  ~ limitations under the License.
  -->

# Benchmark plan: full `shenyu-bootstrap` vs minimal `shenyu-bootstrap-http` (issue #7649)

## 0. Principles

1. **Same environment**: one machine, one JDK, one set of JVM flags (empty by
   default), one admin, one route, one backend. The two variants run
   **sequentially** (never concurrently) to avoid resource contention.
2. **Do not infer throughput from package size.** Package size only supports
   conclusions like "smaller distribution/image, faster startup, lower resident
   memory"; steady-state QPS/latency must be measured. The a-priori hypothesis is
   that both variants share the identical request path (the divide plugin chain),
   so steady-state performance should be **approximately equal** — the benchmark's
   job is to **confirm or refute** that hypothesis, not to assume it.
3. **Every conclusion comes from collected data**, and every metric names its
   collection tool and sampling point.

## 1. Subjects under test and consistency controls

| Item | full | minimal | Control |
|---|---|---|---|
| Artifact | `shenyu-bootstrap` fat jar | `shenyu-bootstrap-http-exec.jar` fat jar | The full bootstrap needs one explicit `spring-boot:repackage` (its pom does not bind the goal); the minimal module binds repackage with the `exec` classifier, so its main artifact stays thin for the dist and the fat jar is `-exec.jar`. Both sides are fat jars — same basis of comparison |
| Port | 9195 | 9195 | identical |
| JVM | same `java`, `JAVA_OPTS` empty by default | same | recorded in the meta file |
| Gateway sysprops | `-Dshenyu.httpclient.keepAlive=true` (`GATEWAY_SYSPROPS`) | same | deliberate **symmetric** deviation from the as-shipped default (`false`), see §5 |
| Admin | one instance, one namespace | same | route created once, shared by both variants |
| Sync channel | websocket | websocket | identical; token inherited via `SHENYU_SYNC_WEBSOCKET_TOKEN` |
| Route | 1 divide selector + rule (`/bench/**`) + 1 upstream registered via `/shenyu-client/register-uri` | same | exactly identical |
| Backend | `BenchBackend` (256-thread pool, tiny body) | same | backend CPU stays far below the gateway during load — not the bottleneck (see §5) |
| Run mode | `java -jar`, one after another | same | every start is a fresh process |

> Note: the full bootstrap runs with its 36 additional plugin starters on the
> classpath. Those plugins are no-op pass-throughs for non-matching requests but
> still add chain overhead — that is part of "the distribution as shipped" and
> therefore part of what is being measured; nothing is disabled artificially.
> The final distribution-level size comparison (tar.gz / Docker image) should be
> redone once both `shenyu-bootstrap-http-dist` and the full bootstrap dist are
> built in the same environment (the fat jar is the proxy metric for now).

**Why register-uri for the upstream:** in current master the gateway only learns
divide upstreams from `DISCOVER_UPSTREAM` sync events
(`DivideUpstreamDataHandler` -> `UpstreamCacheManager`); a selector's `handle`
JSON is never parsed gateway-side, and the admin publishes `DISCOVER_UPSTREAM`
only for client registrations (`/shenyu-client/register-uri`) and
discovery-bound selectors. A manually maintained selector handle therefore never
becomes a healthy upstream. The registered upstream is served back to every
freshly started gateway by the websocket full sync (admin side:
`AbstractDataChangedListener.refreshLocalCache` ->
`updateDiscoveryUpstreamDataCache`), so one registration covers all cold-start
runs of both variants. `/shenyu-client/**` is not on the admin shiro white-list,
so the script sends the admin `X-Access-Token` on register calls.

## 2. Metric definitions and collection methods

| Metric | Definition | Tool | Sampling / aggregation |
|---|---|---|---|
| Package size | fat jar bytes; bundled jar count | `stat` + `unzip -l` | single value |
| Cold-start readiness | process fork -> `/actuator/health` returns 200 (`T_health`); fork -> first successful proxied response carrying the backend marker (`T_proxy`, includes the initial websocket sync) | bash millisecond timing + 50 ms polling | `RUNS_COLD=5` repetitions; run1=cold (first ever run), run2..5=warm (page cache hot); report min/median/max |
| RSS | process resident memory (Windows `WorkingSet64`) | PowerShell `Get-Process` | 3 samples, median, while idle; 3 more after the highest concurrency level (steady state) |
| Loaded classes | currently loaded class count (Loaded - Unloaded) | `jstat -class <pid>` | once after startup; once after the highest load |
| CPU baseline | average busy cores while idle (mCPU) | PowerShell `.CPU` counter delta over a 10 s window | no-traffic window after startup |
| Steady-state throughput | measured RPS under closed-loop concurrency | `LoadDriver` (JDK HttpClient, keep-alive) | concurrency ladder 8/32/128, 60 s warmup + 60 s measurement per level |
| Steady-state latency | client-side p50/p90/p99/max (microseconds) of **successful** responses only (HTTP 200 **and** body contains the backend marker — ShenYu answers route/upstream failures with HTTP 200 + JSON error code, so status alone cannot detect them) | `LoadDriver` histogram (50 µs buckets) | same window as throughput |
| CPU/RSS under load | average busy cores per level window, RSS at window end | same CPU/RSS method | one row per level |

What the closed-loop model means: it measures the **maximum throughput the
gateway sustains at a given concurrency, and the latency at that saturation
point** — not latency at a fixed externally imposed rate. An open-loop
fixed-RPS mode could be added to `LoadDriver` later, but a closed loop is
sufficient for a relative comparison.

## 3. Execution flow (automated by `benchmark.sh`)

```
preflight: both jars exist -> admin healthy -> start BenchBackend
           -> login, create route + register upstream via register-uri (once)
for variant in [full, minimal]:        # sequential
    package size -> csv
    for run in 1..5: fresh-process cold start -> T_health, T_proxy -> kill -> wait for port release
    start once more:
        loadedClasses after startup, idle RSS x3, idle CPU (10 s)
        for workers in 8 32 128:
            LoadDriver 60 s warmup + 60 s measurement -> RPS/p50/p90/p99/max/errors
            window CPU delta, RSS at window end
        loadedClasses after load, steady RSS x3
    kill
cleanup: offline the upstream (register-uri OFFLINE), delete the route, kill the backend
```

Artifacts: `shenyu-bootstrap-http/target/benchmark/bench-<ts>.csv` (long table,
`variant,kind,label,v1..v8`) + `bench-<ts>.meta.txt` (environment fingerprint:
OS / CPU cores / java version / JAVA_OPTS / parameters). The results directory
lives under `target/` and is not committed.

## 4. How to run

```bash
# 0. build both variants (the full bootstrap needs an explicit repackage into a
#    fat jar; package and repackage must run in the same maven session,
#    otherwise it fails with "Source file must not be null")
#    the minimal module binds repackage itself (exec classifier); after install
#    its fat jar is shenyu-bootstrap-http/target/shenyu-bootstrap-http-exec.jar
./mvnw -pl shenyu-bootstrap,shenyu-bootstrap-http -am clean install \
    -DskipTests -Dmaven.javadoc.skip=true -Drat.skip=true -Djacoco.skip=true
./mvnw -pl shenyu-bootstrap package spring-boot:repackage -DskipTests

# 1. start admin (H2 by default)
java -jar shenyu-admin/target/shenyu-admin.jar

# 2. if the admin is configured with a websocket token (this workspace is),
#    export it for both gateways
export SHENYU_SYNC_WEBSOCKET_TOKEN=<must match admin application.yml>

# 3. full run (~25-35 min)
bash shenyu-bootstrap-http/src/test/benchmark/benchmark.sh

# or a quick self-check of the collection pipeline first (~3 min)
QUICK=1 bash shenyu-bootstrap-http/src/test/benchmark/benchmark.sh
```

## 5. Pitfalls and caveats

- **The backend must not become the bottleneck**: `BenchBackend` uses a
  256-thread pool and returns a fixed tiny body; during load, watch that the
  backend process CPU stays far below the gateway's. If both variants' RPS clamp
  at the same value while the backend CPU is saturated, you are measuring the
  backend ceiling — the relative comparison still holds but absolute numbers are
  meaningless; enlarge the backend pool or run multiple instances.
- **`shenyu.httpclient.keepAlive=false` (the shipped default) is unusable for a
  loopback benchmark on Windows**: with keep-alive disabled the gateway opens a
  fresh TCP connection to the backend per request, and the Windows stack
  aborts a *rate-dependent* share of them (WSAECONNABORTED ->
  `PrematureCloseException` -> HTTP 500). Measured on the reference machine:
  81% errors (full) vs 19% (minimal) at w=8 — the noise scales with each
  variant's connection rate and would bias the comparison — 0.28% at w=2, and
  **0% with `-Dshenyu.httpclient.keepAlive=true`**. Both variants therefore run
  with `GATEWAY_SYSPROPS=-Dshenyu.httpclient.keepAlive=true` (symmetric,
  documented here and in the meta file). The finding itself — the shipped
  default halves effective throughput on Windows loopback — is a legitimate
  benchmark observation and is reported as such. (Direct-to-backend load shows
  0 errors at 24k RPS, so the backend was never the problem; `BenchBackend`
  still binds with an accept backlog of 4096 as defence in depth.
  `HttpClientFactory` maps the property to `HttpClient.keepAlive(...)`; the
  field default in `HttpClientProperties` is `false`.)
- **Windows pid translation**: MSYS2 `$!` is an MSYS pid, not a Windows pid;
  `powershell Get-Process` and `jstat` need the latter. The script translates
  via `/proc/<pid>/winpid` (`winpid()`); without it RSS/CPU/class metrics come
  back empty.
- **JIT warmup**: the 60 s warmup before each load level is excluded from
  statistics; cold start and steady state are strictly separated.
- **Windows measurement equivalents**: `WorkingSet64` ≈ RSS; the PowerShell
  `.CPU` counter is cumulative seconds across all cores — delta/window = average
  busy cores (mCPU). The script reports cores, not "whole-machine percent", to
  avoid ambiguity.
- **Disk page cache**: the difference between cold-start run1 and run2..5 comes
  mainly from the page cache and JIT snapshots; both are reported as-is and
  never averaged together.
- **Sync timing**: `T_proxy` includes websocket connect + the first full config
  pull, so it is typically a few hundred milliseconds to a few seconds later
  than `T_health`. This applies to both variants equally — a fair term.
- **Rule handle is mandatory**: the divide rule is created with a
  `DivideRuleHandle` JSON; without it the gateway NPEs and answers HTTP 500 for
  every request (see `src/test/smoke/README.md` for the same lesson).
- **Failures must be visible**: load rows carry an `errors` column; any level
  with errors>0 must be called out in the conclusion — never report RPS alone.
- **Scope of conclusions**: this benchmark measures single-machine loopback
  relative differences and does not extrapolate to production QPS promises; the
  tar.gz / image size comparison should be redone once both dist assemblies are
  built together (the issue's acceptance criterion).

## 6. Result summary template (fill with measured values)

| Metric | full | minimal | Delta | Conclusion |
|---|---|---|---|---|
| fat jar size | | | | |
| bundled jars | | | | |
| cold start T_health (cold / warm median) | | | | |
| cold start T_proxy (cold / warm median) | | | | |
| idle RSS | | | | |
| steady-state RSS | | | | |
| loaded classes (post-load) | | | | |
| idle CPU (mCPU) | | | | |
| RPS @8/32/128 | | | | |
| p99 @8/32/128 | | | | |
| CPU under load (mCPU @128) | | | | |

Conclusion rules: report size/startup/RSS/class-count savings as measured; QPS
and latency conclusions **may only** be drawn from the load-ladder rows (the
expectation is that the two are close; when |delta| is at noise level <5%, write
"no significant difference").
