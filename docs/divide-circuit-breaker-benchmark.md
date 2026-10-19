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

# Divide circuit-breaker key reuse (#7826)

The final branch also fixes #7760 and includes a real HTTP gateway benchmark.
See [the gateway measurements and recovery checks](benchmarks/divide-gateway/README.md).
The microbenchmark numbers below preserve the initial optimization checkpoint
(826149218), before the independent half-open fix.

## Change and ownership

The original Divide path constructs a key for every candidate, constructs it
again during an all-blocked probe pass, and constructs the selected key again
for outcome recording. It also allocates an allowed-upstream list in addition
to the mutable snapshot returned by `UpstreamCacheManager`.

`CircuitBreakerCandidates` computes each candidate key once per request and
compacts that existing snapshot in place. A parallel key array replaces the
allowed-list backing array. Selected-target lookup uses reference identity,
with value equality for the explicit-target copy. A custom load balancer
returning a target outside the candidate list still gets its own key.
Candidate order, objects, weights and load-balancing statistics are preserved.

The final implementation additionally reuses the key across requests through
`Upstream.buildSelectorKey`. Each upstream owns one transient, volatile cache
entry containing the selector id, URL and resulting key. URL changes invalidate
it; reads also compare the captured URL. Different selectors replace the single
entry instead of growing a map. Group/version/weight changes preserve the
existing domain-based breaker identity. Serialization omits this runtime cache.

Request-local storage is O(candidates in this request), with no global memoization and no
configuration-removal invalidation machinery. No shared cache list is mutated.
Candidate storage remains bounded by the request snapshot; outcome callbacks use the selected key and probe token. New requests recompute keys after
selector, protocol or URL changes.

The final filter uses one pass for ordinary routing and returns an acquired
probe as an exclusive candidate. Only an unavailable explicit target with no
ordinary alternatives needs a fallback probe pass. This ensures that healthy
siblings cannot prevent recovery and that an unselected target never keeps a
probe. Selected-key lookup adds a bounded identity scan.

## Measurement boundary

Baseline: `297ac6f849857bd0c2ad368709013e98494b0330`.
The baseline method in `DivideCircuitBreakerBenchmark` retains that revision's
filter and outcome-key construction; the optimized variant calls the production
`CircuitBreakerCandidates`. Both include the unchanged cache-list copy and
actual `UpstreamCircuitBreaker.recordOutcome(Mono.empty(), key).block()`.

This is an in-process request-path microbenchmark, not an HTTP gateway load
test. Selection cycles through candidate positions, rather than measuring a
particular load-balancing algorithm. Network, response-body and HTTP error
serialization costs are excluded. Successful operations represent completed
synthetic upstream Monos.

The matrix uses 1, 10 and 128 targets, with healthy, alternating healthy/blocked,
and all-blocked cases. One-target "mixed" is necessarily healthy. Breakers use
the production failure threshold and wait duration and are reset immediately
before each sample. Every sample asserts its success count, so accidental
expiry into the known half-open admission defect invalidates the sample.

Each case warms both paths for 50,000 requests and collects three 100,000-request
samples per variant, alternating execution order. Both paths are paced at
50,000 requests/s in batches of 100. Healthy/mixed successful throughput is
therefore matched; all-blocked throughput is zero successes for both variants.
`p99_ns` measures service time, excluding pacing pauses, rather than arrival
queueing latency. ThreadMXBean measures allocation and CPU on the request
thread. GC counters are process-wide.

`key_builds_per_request` is the operation count from the two algorithms:
N + 1 versus N for a successful request; 2N versus N when all targets block.
`key_only_cpu_ns_per_request` separately times that many real key constructions
with a volatile consumer (at least two million constructions per measurement).
It is a diagnostic workload, not CPU attribution within the complete path.
Windows thread CPU accounting is coarse; small CPU differences and occasional
zero readings must not be interpreted as precise speedups.

## Local results (2026-10-19)

Microsoft OpenJDK 17.0.18+8-LTS, Windows 11 amd64, AMD Ryzen 9 8945HX,
32 logical processors, 128 MiB fixed heap, default G1 collector.
Raw [samples](benchmarks/divide-circuit-breaker/results.csv) and
[JVM configuration](benchmarks/divide-circuit-breaker/environment.txt) are saved.
Observed rates were 49,633–49,993 requests/s (the target was 50,000/s).
Both variants completed every healthy/mixed request, and neither completed an
all-blocked request.

Values below are medians of three samples. GC entries are totals across the
three measured samples; setup, warmup and the key-only workload are excluded.

| Targets | State | Bytes/request before → after | p99 µs before → after | GC count before → after | GC ms before → after |
| ---: | --- | ---: | ---: | ---: | ---: |
| 1 | healthy | 480 → 360 | 0.8 → 0.6 | 2 → 2 | 3 → 3 |
| 1 | mixed (healthy) | 480 → 360 | 0.6 → 0.5 | 2 → 2 | 0 → 2 |
| 1 | blocked | 336 → 216 | 0.5 → 0.4 | 2 → 1 | 1 → 1 |
| 10 | healthy | 1,624 → 1,504 | 1.6 → 1.5 | 6 → 6 | 3 → 2 |
| 10 | mixed | 1,624 → 1,504 | 2.1 → 2.0 | 6 → 6 | 3 → 2 |
| 10 | blocked | 2,560 → 1,360 | 3.2 → 2.1 | 9 → 6 | 4 → 1 |
| 128 | healthy | 16,728 → 16,608 | 15.7 → 14.9 | 64 → 63 | 26 → 27 |
| 128 | mixed | 16,728 → 16,608 | 17.8 → 18.2 | 64 → 63 | 26 → 27 |
| 128 | blocked | 31,824 → 16,464 | 31.3 → 21.5 | 123 → 63 | 50 → 27 |

The retained change saves 120 bytes per successful request in this fixture.
All-blocked savings scale with candidate count: 120, 1,200 and 15,360 bytes
(35.7%, 46.9% and 48.3%). This is the strongest evidence for retaining the
request-local representation. The 128-target mixed p99 is slightly worse;
there is no claim of a uniform latency improvement.

The CSV includes request-thread CPU and the separate key-only CPU workload.
For 128 blocked targets, the key-only median is 6,250 → 3,125 ns per simulated
request, corresponding to 256 → 128 key constructions. At small sizes Windows
CPU quantization and scheduling noise dominate; even some request CPU values
are zero. These samples do not establish an overall CPU or throughput gain.
Repeat with longer runs and multiple independent JVMs before drawing production
capacity conclusions.

The separate 27-second JFR run collected 712 execution samples and 7,183
allocation samples. The [profile summary](benchmarks/divide-circuit-breaker/profile-summary.csv)
groups stacks by key-only workload, baseline filter, optimized filter, common
request code, or other code, in that precedence order. Key-construction frames
appear in 46 baseline-filter and 41 optimized-filter CPU samples; allocation
stacks show byte arrays and strings under `buildKey` in both paths. Thus this
change removes duplicate construction, not all construction. Sample counts and
allocation weights cover warmup and all cases together and are not precise
per-request comparisons; use ThreadMXBean allocation rows for those comparisons.
The raw recording is generated at the module's
`target/divide-circuit-breaker.jfr` (2,851,504 bytes in this run).

## Reproduction

Run from the repository root with JDK 17. On Windows use `./mvnw.cmd`; on
Unix use `./mvnw`.

```powershell
$env:JAVA_HOME = 'C:\Users\yangj\.jdks\ms-17.0.18'
./mvnw.cmd -pl shenyu-plugin/shenyu-plugin-proxy/shenyu-plugin-divide -am test '-Dtest=DivideCircuitBreakerBenchmark' '-Dshenyu.divide.benchmark=true' '-Dsurefire.failIfNoSpecifiedTests=false' '-Drat.skip=true' '-Djacoco.skip=true' '-DargLine=-Xms128m -Xmx128m'
```

The opt-in benchmark is excluded from normal test discovery. Output is
`shenyu-plugin/shenyu-plugin-proxy/shenyu-plugin-divide/target/divide-circuit-breaker.csv`,
plus a sibling `.environment.txt`. Properties `shenyu.benchmark.iterations`,
`shenyu.benchmark.warmup`, `shenyu.benchmark.rate` and
`shenyu.benchmark.output` allow repeatable variations.

Collect a separate, shorter JFR profile; do not combine its timings with the
unprofiled comparison:

```powershell
./mvnw.cmd -pl shenyu-plugin/shenyu-plugin-proxy/shenyu-plugin-divide -am test '-Dtest=DivideCircuitBreakerBenchmark' '-Dshenyu.divide.benchmark=true' '-Dsurefire.failIfNoSpecifiedTests=false' '-Drat.skip=true' '-Djacoco.skip=true' '-Dshenyu.benchmark.iterations=20000' '-Dshenyu.benchmark.warmup=20000' '-Dshenyu.benchmark.output=target/divide-circuit-breaker-profile.csv' '-DargLine=-Xms128m -Xmx128m -XX:StartFlightRecording=filename=target/divide-circuit-breaker.jfr,settings=profile,dumponexit=true'
& "$env:JAVA_HOME/bin/jfr.exe" summary shenyu-plugin/shenyu-plugin-proxy/shenyu-plugin-divide/target/divide-circuit-breaker.jfr
& "$env:JAVA_HOME/bin/jfr.exe" print --events jdk.ExecutionSample,jdk.ObjectAllocationSample --stack-depth 20 shenyu-plugin/shenyu-plugin-proxy/shenyu-plugin-divide/target/divide-circuit-breaker.jfr
```

## Correctness and the half-open prerequisite

Initial-checkpoint validation on JDK 17: 67 regression tests passed (35 Divide tests,
12 base-breaker tests, 16 retry tests and 4 shared-model tests). Reactor
Checkstyle reported zero violations. The Divide RAT check approved all 14
included resources; `git diff --check` also passed.

```powershell
./mvnw.cmd -pl shenyu-plugin/shenyu-plugin-proxy/shenyu-plugin-divide,shenyu-plugin/shenyu-plugin-httpclient -am test '-Dtest=Divide*Test,CircuitBreakerCandidatesTest,UpstreamCircuitBreakerTest,RetryStrategyTest' '-Dsurefire.failIfNoSpecifiedTests=false' '-Drat.skip=true' '-Djacoco.skip=true'
./mvnw.cmd -pl shenyu-plugin/shenyu-plugin-proxy/shenyu-plugin-divide apache-rat:check
```

Tests exercise candidate compaction/order/identity, explicit target selection
and blocked-target fallback, selector/protocol/URL isolation, configuration
removal while a snapshot is in flight, error/success/cancellation accounting,
retry attributes, and the existing P2C cancellation test. Existing retry tests
also exercise failover; ordinary requests retain their configured retry behavior.

The half-open issue is now fixed. Ordinary eligibility is CLOSED-only, and each
probe owns a particular state object and generation. Error or cancellation
reopens that probe; completion closes it. Removed/replaced state and old
ordinary/probe outcomes cannot resolve a newer probe. Admission and forwarding
are deferred until subscription, including synchronous-error accounting.

Probes make one HTTP attempt, regardless of configured retries, so a failed
recovering target cannot be declared healthy because a failover target succeeded.
Ordinary requests retain configured retries. The final targeted suite passed
100 targeted tests plus all 62 load-balancer tests (156 distinct tests), and the
real HTTP verification passed all 11 checks, including actual
10-second wait windows, 65 overlapping requests, cancellation, removal and
retry=2. The gateway benchmark README records the final measurement boundary.
