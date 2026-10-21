<!--
Licensed to the Apache Software Foundation (ASF) under one or more
contributor license agreements. See the NOTICE file distributed with
this work for additional information regarding copyright ownership.
The ASF licenses this file to You under the Apache License, Version 2.0
(the "License"); you may not use this file except in compliance with
the License. You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.
-->

# Divide gateway HTTP benchmark

This harness sends real HTTP/1.1 requests through the production gateway data
plane: Reactor Netty server -> Spring WebFlux adapter -> ShenyuWebHandler ->
GlobalPlugin -> DividePlugin (including selector matching) -> URIPlugin ->
NettyHttpClientPlugin -> ResponsePlugin/NettyClientMessageWriter -> HTTP client.
Upstreams, each gateway variant and the load generator run in separate JVMs.
Gateway allocation/CPU/GC counters therefore exclude upstream and client work.

Configuration is installed through BaseDataCache and UpstreamCacheManager;
this controlled loopback test does not start the admin service or data-sync
transports. Optional authentication, logging and business plugins are absent.
It measures the complete HTTP proxy path, not production capacity or control
plane synchronization.

## Reproduce on Windows / JDK 17

From the repository root (the repository's Maven wrapper is required):

```powershell
$env:JAVA_HOME = 'C:\Users\yangj\.jdks\ms-17.0.18'
./mvnw.cmd -Pdivide-gateway-benchmark -pl shenyu-plugin/shenyu-plugin-proxy/shenyu-plugin-divide -am install '-DskipTests' '-Dmaven.javadoc.skip=true' '-Djacoco.skip=true' '-Drat.skip=true'
./mvnw.cmd -Pdivide-gateway-benchmark -pl shenyu-plugin/shenyu-plugin-proxy/shenyu-plugin-divide dependency:build-classpath '-Dmdep.outputFile=target/gateway-classpath.txt'
./docs/benchmarks/divide-gateway/run.ps1 -Smoke
./docs/benchmarks/divide-gateway/run.ps1 -SkipVerify
./docs/benchmarks/divide-gateway/run.ps1 -Profile -Samples 1 -Requests 1000 -Rate 500 -SkipVerify
```

`-Smoke` runs both variants on one target, then performs the complete HTTP
correctness scenario. The matrix defaults to three samples, 2,000 requests per
sample and 500 requests/s. `-SkipVerify` avoids rerunning correctness after the
already-passing smoke verification. Each run writes raw JSON, CSV, environment
and process logs to the Divide module's `target/gateway-<timestamp>/` directory.
Profiling runs also dump each gateway's JFR recording before stopping only the
processes started by the script. No externally running service is stopped.
The harness binds only to loopback and uses automatically assigned ports.

## Controlled configuration

- 1, 10 and 128 upstream addresses; healthy, alternating blocked/healthy and
  all-blocked states. One-target mixed is necessarily healthy.
- Each upstream returns a validated 1,024-byte response after a 2 ms timer.
- HTTP keep-alive; equal weights; random load balancing; 128 connections per
  upstream pool; no configured HTTP retries during performance runs.
  Reactor Netty's automatic reconnect retry is also disabled in both variants.
- Four gateway server and four client event-loop threads; four upstream event
  loops. Each JVM uses a 256 MiB fixed heap and ActiveProcessorCount=8.
- Match caches disabled in both variants, so selector matching stays in the
  measured path. Background upstream health checks disabled; configured targets
  explicitly disable those checks. Application logging is OFF in both variants.
- Actual circuit-breaker state is seeded immediately before each sample.
  The sample duration is below its ten-second recovery window, and success
  counts must match exactly, catching unintended state expiry or routing errors.
- Each fresh load JVM warms its connection with 100 requests before measurement.
  Three samples alternate baseline/optimized order. Client-side concurrency is
  capped at 128. Both successful throughput and total throughput are recorded.
- `p99_ms` measures from client send to complete response body. The separate
  `scheduled_p99_ms` includes delay from the intended arrival time, exposing
  load-generator scheduling and queueing instead of hiding coordinated omission.
- Gateway allocated bytes sum live-thread ThreadMXBean counters. Workers are
  long-lived; counter collection and one control response add a small fixed
  cost per sample. CPU is gateway process CPU; GC counters are process-wide.

The baseline compiles the original DividePlugin from master revision 581bee9f1
into an overriding classpath directory. It also compiles the final
UpstreamCircuitBreaker with only buildKey restored to the original uncached
concatenation. Both variants use identical breaker state-transition/outcome
logic and all other gateway libraries. Therefore the steady-state comparison
covers the Divide path's key/list/admission changes;
it does not compare different HTTP stacks or claim to isolate every individual
instruction. Recovery correctness runs only against the final implementation.
The baseline's old probe path is never used in timed samples because their
blocked windows have not expired.

A preliminary request-local-only implementation increased allocation by about
7.5% for 128 healthy targets; its raw results are retained in
[request-local-only.csv](request-local-only.csv). Retaining every freshly built
key can force temporary strings to escape, while lifecycle guards also have a
cost. The final implementation consequently stores one reusable selector/domain
key on each Upstream, invalidated by URL/selector changes. It introduces no
global memoization map. Both the single-slot key and explicit-target lookup
preserve the breaker's domain-only identity when group/version differ.

Results must still be read per scenario; no uniform latency or CPU improvement
is claimed.

## Final measured results (2026-10-19)

The final matrix contains 54 samples and 108,000 measured HTTP requests, plus
connection warmup and control requests. Successful cases completed every
expected response; all-blocked cases completed zero successful upstream
responses. Observed throughput was 496.60–499.96 requests/s against a target of
500/s. Host: Windows 11, AMD Ryzen 9 8945HX; Microsoft JDK 17.0.18+8-LTS.
See [raw samples](results.csv), [environment](environment.txt) and
[HTTP correctness assertions](correctness.json).

Values are medians across three samples. GC count/time are totals over those
three measurement windows. The raw CSV also includes scheduled-arrival p99 and
gateway CPU per request.

| Targets | State | Bytes/request before → after | Response p99 ms before → after | GC count before → after | GC ms before → after |
| ---: | --- | ---: | ---: | ---: | ---: |
| 1 | healthy | 28,465 → 28,421 | 17.354 → 17.275 | 1 → 3 | 3 → 5 |
| 1 | mixed (healthy) | 27,096 → 26,815 | 16.929 → 17.143 | 0 → 1 | 0 → 1 |
| 1 | blocked | 9,908 → 9,767 | 1.394 → 1.375 | 0 → 0 | 0 → 0 |
| 10 | healthy | 28,390 → 27,358 | 16.885 → 16.897 | 2 → 1 | 6 → 1 |
| 10 | mixed | 28,431 → 27,272 | 16.947 → 16.956 | 1 → 1 | 3 → 1 |
| 10 | blocked | 11,933 → 9,798 | 1.251 → 1.378 | 0 → 1 | 0 → 1 |
| 128 | healthy | 41,720 → 29,226 | 17.020 → 16.922 | 2 → 1 | 6 → 1 |
| 128 | mixed | 41,424 → 28,918 | 16.946 → 17.160 | 2 → 1 | 7 → 2 |
| 128 | blocked | 38,834 → 11,211 | 1.342 → 1.306 | 1 → 1 | 3 → 1 |

At 128 targets, allocation falls by 29.9% (healthy), 30.2% (mixed), and 71.1%
(blocked). The single-target gain is small. Latency does not improve uniformly:
10-target blocked and 128-target mixed p99 are slightly worse. Scheduling-aware
p99 is about 31.5 ms for successful traffic and 16 ms for blocked responses;
these values include upstream timer and client scheduling delay. Windows CPU
accounting and JIT/GC scheduling produce noisy CPU rows, so no aggregate CPU or
capacity improvement is claimed.

## Final profiles

The separate profiling run used one 1,000-request sample per variant/case,
at the same 500 requests/s. Baseline and optimized recordings span 79 and
77 seconds. [Baseline CPU stacks](baseline-execution-samples.txt),
[optimized CPU stacks](optimized-execution-samples.txt), and the
[allocation/execution summary](profile-summary.csv) are exported from JFR.
The public exports contain selected execution/allocation events rather than
the recording's host environment metadata.

There were 135 and 125 execution samples respectively; none contained a direct
key-construction frame at the recorded sampling depth. This profile cannot
quantify key CPU cost. Key-related allocation stacks occurred in 159 baseline
samples (byte arrays/strings) and one optimized sample (a byte array).
These sampling counts include warmup/configuration and are qualitative evidence;
the unprofiled ThreadMXBean measurements above are the per-request comparison.

Summary scope is `key` when a stack includes
`UpstreamCircuitBreaker.buildKey` or `Upstream.buildSelectorKey`, otherwise
`divide` when it includes DividePlugin/CircuitBreakerCandidates, otherwise
`other`. Allocation sample weights are summed within variant/type/scope/class.
To inspect the raw local recording after the profiling command:

```powershell
& "$env:JAVA_HOME/bin/jfr.exe" print --events jdk.ExecutionSample --stack-depth 24 '<run-directory>/baseline.jfr'
& "$env:JAVA_HOME/bin/jfr.exe" print --json --events jdk.ExecutionSample,jdk.ObjectAllocationSample --stack-depth 24 '<run-directory>/optimized.jfr'
```

## Validation

The final targeted reactor run passed 100 tests; the complete load-balancer
suite passed 62 tests, with six overlapping Upstream tests (156 distinct tests
in total). Checkstyle passed, and Apache RAT passed for the load-balancer, base,
HTTP-client and Divide modules. The real HTTP verification passed all 11 checks.

```powershell
./mvnw.cmd -Pdivide-gateway-benchmark -pl shenyu-plugin/shenyu-plugin-proxy/shenyu-plugin-divide -am install '-Dtest=Divide*Test,CircuitBreakerCandidatesTest,UpstreamCircuitBreakerTest,*RetryStrategyTest,NettyHttpClientPluginTest,UpstreamTest' '-Dsurefire.failIfNoSpecifiedTests=false' '-Drat.skip=true' '-Djacoco.skip=true' '-Dmaven.javadoc.skip=true'
./mvnw.cmd -pl shenyu-loadbalancer test '-Djacoco.skip=true' '-Drat.skip=true'
./mvnw.cmd -pl shenyu-loadbalancer,shenyu-plugin/shenyu-plugin-base,shenyu-plugin/shenyu-plugin-httpclient,shenyu-plugin/shenyu-plugin-proxy/shenyu-plugin-divide apache-rat:check
```

## HTTP recovery checks

The verification drives actual transport failures and waits for real production
10-second recovery windows; it does not backdate state or mock the HTTP client.
It checks healthy-sibling availability, one held probe among 65 overlapping
requests, cancellation reopening the wait window, a later successful probe,
explicit-target recovery, removal during an in-flight probe, no route resurrection,
and a single failed probe attempt even with retry=2. In addition, unit tests
cover generation/identity isolation for late callbacks and removed state.

The saved correctness JSON lists each passed assertion. All requests considered
successful must have HTTP status 200 and the exact expected response body.
All-blocked success throughput is zero for both variants; it must not be
compared as successful work against the healthy cases.
