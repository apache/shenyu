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

# shenyu-bootstrap-http — Minimal HTTP Gateway

A minimal, independently buildable ShenYu gateway bootstrap: **HTTP proxy (divide)
plus exactly one configuration-sync channel (websocket)** — nothing else.
See [issue #7649](https://github.com/apache/shenyu/issues/7649).

The full `shenyu-bootstrap` ships 36 plugin starters and 5 data-sync starters
(RPC protocols, AI/MCP, log brokers, discovery clients). This module keeps only
what an HTTP gateway needs and is meant to quantify the startup / footprint
savings of that reduction. The full bootstrap is **not** modified in any way.

## 1. Measured facts (this build, 2.7.2-SNAPSHOT)

| Metric | minimal (`shenyu-bootstrap-http`) | full (`shenyu-bootstrap`) |
|---|---|---|
| Direct dependencies in pom | **4** | 40+ |
| Executable fat jar | **50 MB** | 142 MB |
| Bundled jars (`BOOT-INF/lib`) | **106** | 378 |
| ShenYu modules bundled | **25** | 124 |
| `dubbo / grpc / etcd / nacos / kafka / rocketmq / elasticsearch / ai-* / mcp` inside jar | **0 hits** | present |

(Full-bootstrap cells measured in the same benchmark run as §8.1 — package
size alone says nothing about throughput.)

Active gateway plugins (all transitive via `shenyu-spring-boot-starter-gateway`):
`global`, `divide` (HTTP proxy), `httpclient`, `context-path`, `uri`,
`transform`, `response`.

## 2. Module layout

```
shenyu-bootstrap-http/
├── pom.xml                                    # 4 dependencies, repackage bound
├── README.md                                  # this document
└── src/
    ├── main/
    │   ├── java/org/apache/shenyu/bootstrap/http/
    │   │   └── ShenyuHttpBootstrapApplication.java
    │   └── resources/
    │       └── application.yml                # minimal config, secrets via env vars
    └── test/
        ├── smoke/                             # smoke test (issue acceptance scenarios)
        │   ├── smoke-test.sh
        │   ├── HttpStubBackend.java
        │   └── README.md
        └── benchmark/                         # benchmark harness vs full bootstrap
            ├── benchmark.sh
            ├── LoadDriver.java
            ├── BenchBackend.java
            └── README.md
```

The only change to pre-existing files is one `<module>shenyu-bootstrap-http</module>`
line in the root `pom.xml`.

## 3. Dependency set

| Dependency | Purpose |
|---|---|
| `spring-boot-starter-webflux` | reactive runtime (Netty server) |
| `spring-boot-starter-actuator` | `/actuator/health` endpoint |
| `shenyu-spring-boot-starter-gateway` | gateway core; transitively: `shenyu-web`, plugin `divide`, `httpclient`, `global`, `context-path`, `response`, `uri`, `transform` |
| `shenyu-spring-boot-starter-sync-data-websocket` | the single admin→gateway config-sync channel (matches admin default) |

Everything else was deliberately dropped: 36 plugin starters, 4 alternative sync
starters (zookeeper / http / nacos / etcd), `nacos-discovery` +
`spring-cloud-commons`, the Dubbo stack, and `client-beat` (instance heartbeat
to the admin dashboard).

## 4. Build

```bash
# from the repository root (checkstyle + RAT run automatically)
./mvnw -pl shenyu-bootstrap-http -am clean install -DskipTests

# fast local build
./mvnw -pl shenyu-bootstrap-http -am clean install -DskipTests \
    -Dmaven.javadoc.skip=true -Drat.skip=true -Djacoco.skip=true
```

Output: `shenyu-bootstrap-http/target/shenyu-bootstrap-http-exec.jar` (executable).
`spring-boot-maven-plugin` `repackage` is bound with `<classifier>exec</classifier>`,
so the main artifact `shenyu-bootstrap-http.jar` stays a thin jar — that is what
`shenyu-bootstrap-http-dist` ships in `lib/` (dist convention: thin jar + `lib/*`
classpath); the `-exec.jar` is the convenient `java -jar` variant.

## 5. Run

Prerequisite: `shenyu-admin` running (default port 9095, websocket sync enabled —
both are admin defaults).

```bash
java -jar shenyu-bootstrap-http/target/shenyu-bootstrap-http-exec.jar

# verify
curl http://localhost:9195/actuator/health
```

### Configuration

All configuration lives in `src/main/resources/application.yml`. Only what the
HTTP proxy + websocket sync needs is set explicitly; everything else relies on
ShenYu built-in defaults (netty tuning, upstream check, match caches, etc.).

| Environment variable | Default | Meaning |
|---|---|---|
| `SHENYU_SYNC_WEBSOCKET_URLS` | `ws://localhost:9095/websocket` | admin websocket endpoint(s) |
| `SHENYU_SYNC_WEBSOCKET_TOKEN` | *(empty)* | sync token; **must match** admin's `shenyu.sync.websocket.token` when the admin sets one |
| `SHENYU_SYNC_WEBSOCKET_ALLOW_ORIGIN` | `ws://localhost:9195` | origin header for the sync handshake |

The `shenyu.namespace` value must match the namespace the admin seeds
(default `649330b6-c2d7-4edc-be8e-8a54df9eb385`).

## 6. Adding capabilities back

Every removed capability is a one-line pom change away. Add the starter, rebuild,
configure in `application.yml` as documented in the main project docs.

### 6.1 Functional HTTP plugins

| Starter | Capability |
|---|---|
| `shenyu-spring-boot-starter-plugin-param-mapping` | rewrite request parameters |
| `shenyu-spring-boot-starter-plugin-request` | add/remove headers, params, cookies |
| `shenyu-spring-boot-starter-plugin-rewrite` | URI rewriting |
| `shenyu-spring-boot-starter-plugin-redirect` | redirects / custom forwarding |
| `shenyu-spring-boot-starter-plugin-modify-response` | modify response body/headers/status |
| `shenyu-spring-boot-starter-plugin-waf` | web application firewall |
| `shenyu-spring-boot-starter-plugin-ratelimiter` | Redis rate limiting |
| `shenyu-spring-boot-starter-plugin-resilience4j` | circuit breaker / bulkhead (resilience4j) |
| `shenyu-spring-boot-starter-plugin-sentinel` | Sentinel flow control |
| `shenyu-spring-boot-starter-plugin-sign` | signature auth |
| `shenyu-spring-boot-starter-plugin-oauth2` | OAuth2 auth |
| `shenyu-spring-boot-starter-plugin-jwt` | JWT auth |
| `shenyu-spring-boot-starter-plugin-key-auth` | API-key auth |
| `shenyu-spring-boot-starter-plugin-basic-auth` | basic auth |
| `shenyu-spring-boot-starter-plugin-cryptor` | request/response encryption |
| `shenyu-spring-boot-starter-plugin-general-context` | cross-plugin context passing |
| `shenyu-spring-boot-starter-plugin-cache` | response caching |
| `shenyu-spring-boot-starter-plugin-metrics` | Prometheus metrics |
| `shenyu-spring-boot-starter-plugin-tracing` | distributed tracing |
| `shenyu-spring-boot-starter-plugin-mock` | mock responses |

### 6.2 Other protocol proxies

`plugin-websocket`, `plugin-grpc`, `plugin-tcp`, `plugin-apache-dubbo`
(+ `dubbo` and its registry dependencies).

### 6.3 AI plugins

`plugin-ai-proxy`, `plugin-ai-prompt`, `plugin-ai-token-limiter`,
`plugin-ai-sensitive-word`, `plugin-ai-request-transformer`,
`plugin-ai-response-transformer`, `plugin-agent-gateway`, `plugin-mcp-server`.

### 6.4 Logging plugins

`plugin-logging-console`, `plugin-logging-rocketmq`, `plugin-logging-kafka`,
`plugin-logging-elasticsearch`.

### 6.5 Alternative sync channels

Replace (not augment — keep exactly one) the websocket starter with one of
`shenyu-spring-boot-starter-sync-data-{zookeeper,http,nacos,etcd,apollo}` and
set the matching `shenyu.sync.*` properties.

### 6.6 Other optional features

- `shenyu-spring-boot-starter-client-beat` — instance heartbeat shown on the admin dashboard.
- Upstream health checking: `shenyu.upstreamCheck.enabled: true` in `application.yml`.
- Hot-loaded external plugins: `shenyu.extPlugin.*` (drop plugin jars into a directory
  instead of rebuilding).

## 7. Smoke test

`src/test/smoke/` verifies the issue's acceptance scenarios end-to-end
(admin REST → websocket sync → gateway behaviour):

| Case | Action | Expected gateway behaviour |
|---|---|---|
| T1 | create divide selector + rule, attach upstream via `/shenyu-client/register-uri` | **HTTP 200**, body from the backend |
| T2 | offline the upstream (`eventType=OFFLINE`) | **HTTP 200** with `{"code":-119}` (`CANNOT_FIND_HEALTHY_UPSTREAM_URL`) |
| T3 | re-register the upstream | **HTTP 200** + backend marker again |
| T5 | kill the registered backend | **HTTP 500** (connect refused, `GlobalErrorHandler`) |
| T4 | delete rule + selector | **HTTP 200** with `{"code":-107}` (`SELECTOR_NOT_FOUND`) |

```bash
bash shenyu-bootstrap-http/src/test/smoke/smoke-test.sh
```

Requires admin + this gateway running; bash + curl + JDK only. See
`src/test/smoke/README.md` for details.

**Upstream attachment goes through the register API on purpose.** In current
master the gateway only learns divide upstreams from `DISCOVER_UPSTREAM` sync
events (`DivideUpstreamDataHandler` → `UpstreamCacheManager`); the selector
`handle` JSON is never parsed gateway-side, and the admin publishes
`DISCOVER_UPSTREAM` only for client registrations
(`/shenyu-client/register-uri`) and discovery-bound selectors. A manually
maintained selector handle therefore never produces a healthy upstream
(`-119` forever). The smoke test drives the upstream lifecycle exactly like a
real Spring MVC client would. Note `/shenyu-client/**` is not on the shiro
white-list, so the register calls carry the admin `X-Access-Token`.

**Verified live** (2026-10-17, admin @9095 H2 + this gateway @9195, websocket
sync): 10/10 assertions pass — create → `200 {"marker":"smoke-backend-ok",
"path":"/smoke/hello"}`, offline → `-119`, re-register → `200`, backend kill →
`500`, delete → `-107`.

## 8. Benchmark vs full bootstrap

`src/test/benchmark/` collects, in one identical environment, for both variants
run sequentially: package size, cold-start readiness (`T_health`, `T_proxy`,
5 repetitions, cold vs warm), idle/steady RSS, loaded classes (`jstat`),
idle/load CPU, and closed-loop throughput + latency at concurrency 8/32/128.

```bash
# full bootstrap needs its fat jar once (its pom does not bind repackage;
# package and repackage must run in one maven session)
./mvnw -pl shenyu-bootstrap -am clean install -DskipTests
./mvnw -pl shenyu-bootstrap package spring-boot:repackage -DskipTests

QUICK=1 bash shenyu-bootstrap-http/src/test/benchmark/benchmark.sh   # harness self-check
bash shenyu-bootstrap-http/src/test/benchmark/benchmark.sh           # full run (~30 min)
```

**Do not infer QPS from package size.** The a-priori hypothesis is that steady-state
throughput/latency are approximately equal (identical request path); the benchmark
exists to confirm or refute that with measurements.

### 8.1 Measured results (2026-10-17)

From `bench-20261017-122923.csv` (Windows 11, 22 cores, JDK 25.0.4.1, admin on
H2 + websocket sync, 60 s warmup + 60 s measure per level, both gateways run
with `-Dshenyu.httpclient.keepAlive=true` — see benchmark README §5 for why
the shipped default `false` cannot produce a fair comparison on Windows
loopback). The recording template for future runs lives in
`src/test/benchmark/README.md` §6.

| Metric | full | minimal | Δ | Note |
|---|---|---|---|---|
| fat jar size | 148,822,815 B (142 MB) | 51,770,654 B (49 MB) | **−65%** | |
| bundled jars | 378 | 106 | **−72%** | 124 → 25 ShenYu modules |
| cold start `T_health` (run1 / warm median) | 10,720 / 7,869 ms | 2,961 / 2,844 ms | **−64%** warm | |
| cold start `T_proxy` (run1 / warm median) | 11,146 / 8,194 ms | 3,507 / 3,244 ms | **−60%** warm | incl. first websocket full sync |
| idle RSS (MB) | 327.7 | 225.6 | **−31%** | |
| steady RSS after load (MB) | 511.7 | 623.7 | +22% | minimal sustains a higher request rate, so it allocates more; same direction on re-test (531.7 vs 636.1) |
| loaded classes (post-load) | 14,251 | 11,006 | **−23%** | |
| idle CPU (cores) | 0.006 | 0.003 | −50% | both negligible |
| RPS @ 8 / 32 / 128 workers | 5,544 / 6,272 / 6,026 | 7,007 / 7,541 † / 7,112 | **+26% / +20% † / +18%** | |
| p99 latency @ 8 / 32 / 128 workers (µs) | 4,075 / 21,625 / 43,725 | 3,425 / 27,725 † / 36,225 | | |
| CPU under load @128 (cores) | 4.37 | 4.27 | ±0 | |
| errors during load | 0 | 0 | | |

† The main run recorded 23,847 RPS (p99 3,025 µs) for minimal at w=32 — an
outlier that did **not** reproduce on a dedicated re-test with the same
settings (7,541 RPS, p99 27,725 µs). The re-test value is shown above; treat
the spike as a measurement anomaly.

Conclusions, against the §8 hypothesis that both variants share the request
path and should therefore perform approximately equal:

- The footprint claims hold strongly: **~3× faster cold start, −31% idle RSS,
  −23% loaded classes, −65% package size**.
- The throughput hypothesis is **refuted at low/medium concurrency**: minimal
  sustains ~18–26% more RPS at w=8/32 with ~0.6 fewer busy cores — the full
  distribution's extra active components are not free per request. The exact
  source (plugin-chain length vs. other active components) was not profiled.
- At w=128 both variants plateau (~6–7k RPS, p50 ~20 ms) at equal CPU; the
  symmetric latency inflation points to a shared fixture limit (driver or
  backend), so the w=128 row is not a meaningful gateway differentiator.
- Side finding, worth its own issue: the gateway ships with
  `shenyu.httpclient.keepAlive=false`, which on Windows loopback aborts a
  rate-dependent share of backend connections (up to 81% HTTP 500 in this
  harness at w=8; 0% with keep-alive enabled). Real deployments should
  evaluate enabling it.

## 9. Distribution package (`shenyu-bootstrap-http-dist`)

`shenyu-dist/shenyu-bootstrap-http-dist` assembles the binary distribution,
mirroring `shenyu-bootstrap-dist` conventions:

```bash
./mvnw -pl shenyu-bootstrap-http clean install -Prelease -DskipTests
./mvnw -pl shenyu-dist/shenyu-bootstrap-http-dist clean package -Prelease -DskipTests
# → shenyu-dist/shenyu-bootstrap-http-dist/target/apache-shenyu-2.7.2-SNAPSHOT-bootstrap-http-bin.tar.gz (+ .sha512)
```

Layout (same as the full bootstrap dist): `LICENSE` + `NOTICE` + `README.txt` +
`licenses/` at the root, `conf/` (application.yml + logback.xml — **not** embedded
in the jar, thanks to the `release` profile resource exclusion), `bin/`
(start/stop `.sh`/`.bat`, `MAIN_CLASS=org.apache.shenyu.bootstrap.http.ShenyuHttpBootstrapApplication`),
`lib/` (the thin `shenyu-bootstrap-http.jar` + every dependency as a plain jar —
the `-exec` fat jar is explicitly excluded), empty `ext-lib/`. Run with
`bin/start.sh` (classpath `.:conf:lib/*:ext-lib/*`). A `docker` profile builds
`apache/shenyu-bootstrap-http:{latest,2.7.2-SNAPSHOT}` from `docker/Dockerfile`.

The `NOTICE`/`licenses/` inventory is trimmed to what the minimal dependency set
actually bundles (verified against `mvn dependency:tree`): ShenYu header,
commons-codec / commons-collections / commons-lang, Spring Framework and Netty
NOTICE sections, plus third-party license texts for HdrHistogram, LatencyUtils,
Java-WebSocket, asm, bcprov, checker-qual, jakarta.annotation-api,
jcl-over-slf4j, jul-to-slf4j, logback, oshi, re2j, reactive-streams, slf4j.

## 10. Known limitations / follow-ups (issue #7649 scope)

- **Archive/image size comparison**: the fat-jar numbers in §1 are measured;
  tar.gz- and image-level size comparison vs the full bootstrap dist should be
  recorded once both dists are built in the same environment (see §8.1 template).
- **No instance heartbeat**: without `client-beat` this gateway does not appear in
  the admin dashboard's instance list (add it back per §6.6 if wanted).
- **No discovery client**: upstreams come from selector handles / admin API /
  metadata registration, not from Nacos/Eureka discovery.
