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

# Config-view comparison (issue #8223)

No measurements are included. This is a separate mode of the existing HTTP
harness; `run.ps1` and its upstream/circuit-breaker scenarios are unchanged.
The proposed change is opt-in frozen BaseDataCache views, enabled by Divide;
this is not a gateway-wide immutable DTO migration.

## Prerequisites and invocation

Windows PowerShell, JDK 17 (`JAVA_HOME`), installed gateway dependencies and the
existing Divide `target/gateway-classpath.txt` are required. Compile the changed
common/base/api/Divide classes separately first. The script rejects missing or
older class files, puts those four local `target/classes` directories ahead of
installed jars (then any available local loadbalancer/web/HTTP support outputs),
and **never runs Maven or downloads dependencies**.

```powershell
./docs/benchmarks/divide-gateway/run-config-views.ps1 -CompileOnly
./docs/benchmarks/divide-gateway/run-config-views.ps1
./docs/benchmarks/divide-gateway/run-config-views.ps1 -Rates 500,1000,2000 -Requests 10000
./docs/benchmarks/divide-gateway/run-config-views.ps1 -Profile
```

`-Repeats` is at least three. Baseline/change order alternates on successive
repeats. Every sample gets a fresh gateway JVM; variants never run concurrently.
Both use the same fixture, dependencies, 512 MiB fixed heap, eight active CPUs,
four server/client loops, 128 client concurrency, 2 ms upstream delay, 1024-byte
body, retries disabled, and 100 warmup requests. Profile mode applies the same
JFR profile to both variants; compare unprofiled results separately. Raw JSON,
CSV, logs and optional JFR go only to Divide's `target/config-views-*` directory.
JFR is local diagnostic material, not a sanitized artifact to publish blindly.

Baseline `548bec1f9` exports exactly BaseData, ConditionData, PluginData,
RuleData, SelectorData, BaseDataCache, MatchDataCache, AbstractShenyuPlugin and
DividePlugin using `git show`, then compiles them as classpath overrides.
It does not export DtoSnapshots or alter any baseline source. The harness is
compiled against those baseline APIs and does not reference `readOnly()` or
DtoSnapshots. Timed requests run the actual variant's Divide execute/matching
and HTTP forwarding. A small subclass only exposes the rule version/observation
time as response headers and overrides no-selector/no-rule to return explicit
`404 config-no-match`, never a URIPlugin NPE or an accidentally accepted 502.

## Matrix and control API

`gateway <upstreams.json> <ready-file> config-views` enables the new controls.
All servers bind to loopback; there are no credentials or token fixtures.

- `/_bench/config-view?size=1|128&conditions=2&state=matched|no-match|cache-hit`
  publishes 1 or 128 custom-flow selectors, each with 1 or 128 rules. Thus the
  large profile has 128 selectors and 16,384 rules, **not 128 total rules**.
  Every selector/rule has `-Conditions` path-only AND predicates (default 2,
  allowed 1..128); selectors continue to rule matching. One upstream per
  selector is held constant. Exactly one selector/rule matches the last route.
- Matched and selector-no-match run without match caches. Cache-hit repeats the
  same matching URI after warmup with bounded 1024-entry caches and a 10-minute
  TTL. Untimed validation also exercises rule-no-match. No-match expected
  responses are 404s; CSV `upstream_successes` counts load requests only and is
  explicitly zero for no-match. Untimed correctness and burst-visibility probes
  still contact the upstream, even in no-match samples.
- Each state runs both steady (`updates=0`) and burst (`-Burst`, default 32,
  capped at 128) cases. At the load midpoint a separate client task calls
  `/_bench/config-burst?count=N`. The single bounded control worker builds new
  same-ID rule DTOs, publishes through `BaseDataCache.cacheRuleData`, and calls
  `MatchDataCache.removeRuleData("divide")` after each update. No parsed handle
  changes are needed: only the rule name/version changes.
- The first HTTP probe issued after the burst response must see the final
  version via real Divide matching, including the warm cached route. Another
  post-load probe checks it again. This tests **publication visibility**, not
  Admin persistence, transport sync, or atomic multi-object refresh. In-flight
  requests may use older snapshots; a late stale-cache refill fails verification.

## Metrics and limitations

The existing load loop supplies allocation bytes, process CPU, GC count/time,
complete-body p95/p99 and scheduling-aware p95/p99. The latter includes time
waiting for the intended send slot/concurrency permit. `successes` means exact
expected status/body, not necessarily an upstream response. `sampled_peak_heap`
is the gateway heap high-water mark sampled every 10 ms, not an exact peak or
RSS. `retained_before/after` report heap after explicit GC, outside the timed
window; the runner requires an observed GC. GC and configuration run on one
bounded control worker, never blocking a gateway event loop (GC itself can
still pause the JVM).

Setup is reported separately in each `.setup.json` and CSV: installation elapsed
ns, allocated bytes, CPU, GC and before/after used heap. It includes DTO/handle
construction and cache publication; heap endpoints are not setup peak/retained
heap. The sampled peak covers the load/update window only.

`updateNs` sums only publication + invalidation; `maxUpdateNs` is the maximum
single update and `burstNs` includes DTO construction. `firstVisibleNs` is
measured on the **gateway clock**, from final publication completion to the
first post-ack probe's Divide observation; it includes control round-trip and
probe scheduling. It is an upper bound from one verified probe, not the earliest
visibility among all concurrent requests. Intermediate burst versions are not
individually probed. Burst allocation/CPU include control/probe work and cannot
be attributed solely to application requests. Gateway metrics are captured
after draining load and the update task; latency elapsed time covers the load.

Fixed-rate throughput is only observed throughput at that offered rate, **not
capacity**. Use `-Rates` to sweep toward saturation and inspect errors plus
scheduled latency/queueing; increase requests/duration and check client CPU as
well. A 128-concurrency generator and 100-request warmup are limits, not proof of
steady state. All child processes and HTTP controls have deadlines and owned
process cleanup. Increase `-TimeoutSeconds` for longer sweeps, not to hide hangs.

Local validation is compilation/static inspection only. On this machine the
current JDK's `Selector.open()` already fails with a loopback IOException, so
HTTP correctness, cache-hit behavior, burst visibility, JFR and performance
remain unverified. The expected gateway-classpath file was also absent at
implementation time; compilation using available installed jars is not a
substitute for a matching, reproducible gateway dependency classpath. No prior
circuit-breaker CSV in this directory is evidence for config-view performance.
