<!--
Licensed to the Apache Software Foundation (ASF) under one
or more contributor license agreements. See the NOTICE file
distributed with this work for additional information
regarding copyright ownership. The ASF licenses this file
to you under the Apache License, Version 2.0 (the
"License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at

  http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing,
software distributed under the License is distributed on an
"AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
KIND, either express or implied. See the License for the
specific language governing permissions and limitations
under the License.
-->

# Optional LEGACY plugin publisher observations

The real legacy chain can emit sampled immutable plugin boundary facts. It is
disabled by default; the original three-argument ShenyuWebHandler constructor
keeps that behavior. Enabling creates one bounded asynchronous exporter owner:

```yaml
shenyu:
  plugin-observation:
    enabled: true
    queue-capacity: 1024
    sample-every: 100
    max-records: 128
    max-bytes: 262144
```

The optional PluginObservationSink bean receives records only on the dedicated
daemon worker, never on request/EventLoop threads. The Metrics starter supplies
an adapter to the existing MetricsReporter/MetricsRegister facilities. A custom
sink can replace it. Without a sink, the no-op consumer exports nothing while
bounded queue/drop diagnostics remain available. Invalid finite bounds fail at
startup; enabling/disabling is a startup setting.

## Boundaries and identity

Records distinguish outer skip/skip-check failure, publisher construction or
construction failure, actual returned-publisher subscription, and completion,
error or cancellation. Construction brackets the existing before/execute/after
calls, including overrides and the original finally semantics. No hook timing,
WASM ABI, continuation, retry policy or request response is changed.

Publisher duration uses System.nanoTime and is **inclusive of its downstream
continuation**. It is not exclusive plugin CPU time, upstream attempt latency,
final HTTP status, session termination, successful matching or authentication.
An escaped chain failure may be rendered later by the outer error handler.
Existing tracing context is reused when available; this facility creates no
additional gateway server span or Java-agent client span.

Each sampled root subscription gets a local request identity; each actual
invocation and each subscription to its returned publisher get distinct local
identities. Repeated roots create fresh chains. Re-subscribing a successor keeps
the existing mutable-cursor behavior: no implicit cache/replay/new execution is
inserted. Optional fork dispatch shares the observation root but gets distinct
invocations. Fork still restarts an independent cursor over the plugin snapshot
and shares the exchange/body/response; callers need their existing recursion
guard and cannot infer parallel isolation.

The common chain reports entry/skip, not selector/rule success. The optional
matching adapter uses the same internal invocation and delivery boundary; it
must report decisions actually made rather than rerun predicates. Plugins outside
that adapter retain unknown matching facts.

## Bounds and failure isolation

Only bounded strings, closed enums, numeric correlation and optional immutable
matching summaries enter the queue. No exchange, mutable DTO, chain control,
body, raw URL/query, credential/header or exception message/cause is exported.
Existing trace correlation is bounded and never a metric label. At most 63
implementation labels plus other are prepared at setup/configuration updates;
no implementation object or ClassLoader is retained by the label catalog.

The queue holds 1..8192 records. Per-root record/byte budgets reserve an explicit
TRUNCATED event. The byte allowance conservatively charges 8192 serialized bytes
per bounded record, including future matching identifiers, rather than serializing
on EventLoops. Exhausted roots stop allocating matching summaries. Saturation
drops rather than queues without bound. Inventory diagnostics expose queue
occupancy, queue/closed drops, root-budget drops and consumer failures. Sampled
records are partial observations, not unsampled traffic estimates.

Exporter exceptions are counted without logging their potentially sensitive
messages or replacing request outcomes. A slow exporter can occupy its one
worker; it cannot execute under the producer's queue operation or block request
completion. Closing clears pending records and signals the daemon to stop;
non-cooperative custom callbacks remain trusted Java code. Resource cleanup
stays attached to the real resource owner, never a best-effort observation sink.

Metrics use only bounded implementation labels and closed boundary/outcome/cache
classifications. Correlation, selector/rule IDs and revisions are excluded from
labels. The histogram is named publisher_inclusive_millis deliberately.

## Validation and measured overhead

Real default-chain fixtures cover controlled asynchronous completion, returned
publisher errors, synchronous construction failure, cancellation, skip,
short-circuit, unchanged construction hooks, repeated roots, supported fork
dispatch, sampling, saturated/throwing exporters and truncation. Existing chain,
gateway starter, diagnostic and MetricsReporter checks are retained. Matching
correctness remains with the actual-matching adapter and existing base tests.

PluginObservationBenchmark is a standalone same-workload root probe with three
pass-through plugins, root timing/hooks and no network or DTO matching. Baseline
is the actual handler built at 2ce0ff53d, compared with the changed handler using
the same dependency/JVM settings. It does not copy the old cursor implementation.

Temurin 21.0.10, Windows, -Xms128m -Xmx128m, 20000 warmups, three repeats of
100000 roots, logging disabled in all modes:

| Mode | Throughput (million roots/s) | p95 (ns) | p99 (ns) | Allocated bytes/root |
| --- | --- | --- | --- | --- |
| Baseline | 2.18–2.63 | 400–500 | 400–1400 | 1176.0–1180.2 |
| Disabled | 2.19–2.66 | 400–500 | 400–1700 | 1176.0–1181.1 |
| 1% sampled, no-op exporter | 1.86–2.31 | 400–600 | 2300–3400 | 1173.8–1189.9 |

Stable disabled repeats had identical 1176-byte allocation and 400ns p95/p99;
throughput differences were within these repeated-run ranges. This is not a
general zero-cost or production forwarding claim. Enabled sampling has visible
CPU/tail/throughput cost. Process CPU ranged 313–781ns/root baseline/disabled and
781–1875ns/root sampled, with coarse Windows CPU-counter granularity. Allocation
sums live JVM thread counters, includes exporter/JIT variability and is neither
retained heap nor exclusive plugin allocation. No queue/budget drops or consumer
failures occurred in this no-op sample; slow-sink tests separately prove drops.

To reproduce, build the pinned baseline and candidate web reactors through their
wrappers. Generate the candidate dependency classpath with:

```text
./mvnw -pl shenyu-web -am package dependency:build-classpath -Dmdep.outputFile=target/observation-classpath.txt -DskipTests -Dmaven.javadoc.skip=true -Djacoco.skip=true -DskipRemoteResources=true
```

Run PluginObservationBenchmark baseline/disabled/sampled in separate JVMs. Put
the selected checkout's target/classes directories first for register-protocol,
common, SPI, sync-data-api, plugin-api, loadbalancer, plugin-base and web. Append
candidate class directories (for the new optional probe classes), candidate web
target/test-classes and observation-classpath.txt dependencies. Use baseline
directories first only in baseline mode. The probe exits its standalone JVM after
measurement because core shared executors may otherwise outlive main.

```text
java -Xms128m -Xmx128m -cp <ordered-classpath> org.apache.shenyu.web.observation.PluginObservationBenchmark disabled
```

The owning verification uses the wrapper's web/Metrics/gateway-starter reactor,
Checkstyle and Apache RAT; full Docker/distribution and all-protocol SDK E2E are
outside this boundary-specific delivery.
