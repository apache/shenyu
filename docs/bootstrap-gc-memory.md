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

# Bootstrap GC and memory diagnostics

[中文](bootstrap-gc-memory-zh.md) · [Container deployment](container-runtime.md)

Use this guide to collect evidence before changing Bootstrap's heap, collector
or plugin configuration. It does not establish a production GC defect or change
runtime defaults. Keep the application revision, configuration and measurement
window with every result. Related: [#8160](https://github.com/apache/shenyu/issues/8160)
and [performance tracking #7650](https://github.com/apache/shenyu/issues/7650).

## Establish the effective environment

Record the following for the **gateway process**, separately from Admin,
upstreams and the load generator:

| Area | Record |
|---|---|
| JVM | JDK vendor/version, active collector, effective flags, heap limits, direct-memory configuration and thread count |
| Deployment | Image digest or archive revision, OS/architecture, CPU quota, throttled CPU time, memory limit, process RSS and cgroup memory usage |
| Traffic | Request rate, concurrency, upstream count, request/response sizes, duration and concurrency of streams, timeouts, retries and cancellations |
| Gateway | Enabled plugins, selector/rule counts, match-cache settings, logging queue capacity/backlog, connection-pool settings and configuration publication times |

The [container entrypoint](../shenyu-dist/shenyu-bootstrap-dist/docker/entrypoint.sh)
uses `-Xms2g -Xmx2g -Xmn1g`; the
[archive start script](../shenyu-dist/shenyu-bootstrap-dist/src/main/resources/bin/start.sh)
uses `-Xms4g -Xmx4g -Xmn1g`. Both also set `-Xss512k`,
`-XX:+DisableExplicitGC` and `-XX:LargePageSizeInBytes=128m`. Neither default
option set enables GC logging, NMT or an explicit direct-memory cap.
`BOOT_JVM` **replaces the complete default option string** in both launchers.
Other launch mechanisms may use different flags: inspect the running process.

For Docker, `docker stats --no-stream <container>` reports container usage;
collect process RSS inside the container separately. On Linux cgroup v2,
capture `memory.current`, `memory.max`, `memory.events`, `cpu.max` and
`cpu.stat` from the process's cgroup (locate it with `/proc/<pid>/cgroup`).
Cgroup v1 uses different files. Container memory can include filesystem/page
cache and other processes; it is not equivalent to JVM heap or RSS. A rising
`nr_throttled`/`throttled_usec` delta in `cpu.stat` helps identify CPU quota
pressure during the same latency window.

## Collect a minimum diagnostic bundle

Use the target JDK's tools as the same effective user/group, on the same host
and in the target PID namespace. Set `pid` to the specific gateway PID; do not
send diagnostic commands to every JVM. The current container entrypoint makes
Java PID 1; archive installations need their actual Java PID.

```sh
pid=1 # container example; replace for an archive installation
jcmd "$pid" VM.version
jcmd "$pid" VM.command_line
jcmd "$pid" VM.flags
jcmd "$pid" GC.heap_info
jcmd "$pid" JFR.start name=shenyu-gc settings=profile duration=120s filename=/tmp/shenyu-gc.jfr
```

Save the command output with timestamps. `VM.flags`/`GC.heap_info` show the
effective configuration and collector; an image's JDK tag alone does not prove
which collector a deployment selected. Command-line output may include secrets;
remove them before sharing the bundle. Do not force a full GC merely to collect
these initial observations.

The JFR recording stops after 120 seconds and writes `/tmp/shenyu-gc.jfr` in
the **target process's filesystem**, not the caller's working directory. Check
it with `jfr summary /tmp/shenyu-gc.jfr` after recording completes. Copy it out
before a container restart. `profile` recording has a cost: record its use in
comparisons and inspect recordings for sensitive data before sharing.

The [current distribution images](container-runtime.md#java-runtime) include a
full JDK. For a derived/minimal image without `jcmd` or `jfr`, use an approved
matching JDK/tooling environment with access to that PID namespace and attach
permissions, or reproduce in a diagnostic image. A missing tool/disabled attach
is not evidence that GC is healthy. Do not use the archive's Bash start script
inside the Alpine image; its POSIX entrypoint starts Java directly.

### GC logs and writable storage

Append this option to the deployment's **existing, reviewed** JVM option set:

```text
-Xlog:gc*,safepoint:file=/tmp/shenyu-gc-%p.log:time,uptime,level,tags:filecount=5,filesize=20M
```

For an explicit existing override, the shell pattern is:

```sh
: "${BOOT_JVM:?set the complete existing JVM option string first}"
export BOOT_JVM="$BOOT_JVM -Xlog:gc*,safepoint:file=/tmp/shenyu-gc-%p.log:time,uptime,level,tags:filecount=5,filesize=20M"
```

If `BOOT_JVM` was unset, first copy the effective launcher's complete option set;
setting it to the logging flag alone discards the default heap settings. Apply
the resulting environment to the gateway deployment and restart it. In YAML,
use a literal folded scalar containing the complete options; `${BOOT_JVM}` is
not a portable Kubernetes environment-variable expansion mechanism.

The target UID must be able to write the directory, including with a read-only
root filesystem. Rotation retains up to five archived files plus the active
file, approximately 120 MiB at these settings, separately from JFR and other
temporary files. Size the writable volume accordingly; the small `/tmp` smoke
example in the container guide is not a diagnostics-storage budget. A tmpfs
also consumes container memory. Bound recording duration/storage and copy
artifacts to persistent storage before pod replacement.

### Native memory

NMT requires the startup option `-XX:NativeMemoryTracking=summary`; it cannot be
enabled after startup. After a controlled restart, collect:

```sh
jcmd "$pid" VM.native_memory summary
jcmd "$pid" VM.native_memory baseline
# Repeat after a comparable steady-load interval:
jcmd "$pid" VM.native_memory summary.diff
```

Compare **committed** and reserved values separately. NMT tracks HotSpot's
accounted native allocations, not all third-party native/process memory, and
does not equal RSS. Pair it with RSS/cgroup usage, thread counts and the
allocator-specific Netty used/active direct-memory measurements available in
your deployment. JVM buffer-pool exports alone may miss allocation paths;
retained pool capacity alone does not prove a buffer leak. NMT adds overhead:
use the same tracking setting in both sides of a comparison.

## Correlate the correct metrics sources

The sample [Bootstrap configuration](../shenyu-bootstrap/src/main/resources/application.yml)
sets `shenyu.metrics.enabled: false`. `props.jvm_enabled: true` only registers
JVM collectors when the metrics service itself starts. With the metrics plugin
and starter available, a loopback-only example is:

```yaml
shenyu:
  metrics:
    enabled: true
    name: prometheus
    host: 127.0.0.1
    port: 8090
    props:
      jvm_enabled: true
```

Confirm the startup message `Prometheus metrics HTTP server ... start success`
and retrieve `http://127.0.0.1:8090/metrics` **inside the gateway's network
namespace**, for example `wget -q -O - http://127.0.0.1:8090/metrics`. Expose a
different address only through the deployment's monitoring access controls.
This is the plugin's independent Prometheus listener, not
`/actuator/prometheus`; Actuator/Micrometer exports have their own availability,
configuration and names. Keep the source and labels with the measurements.

The plugin's HotSpot collectors expose families such as `jvm_memory_used_bytes`,
`jvm_buffer_pool_used_bytes`, `jvm_threads_current` and
`jvm_gc_collection_seconds_count`/`sum`. The last two provide collector counts
and accumulated collection elapsed time, not a pause percentile distribution
or total GC CPU. Use GC logs/JFR for pauses and GC-thread CPU observations for
CPU cost. Sampled JFR allocation weights estimate allocation hot spots; they
are not an exact bytes/request counter. For an exact accounting experiment,
define the counter's thread coverage and measurement boundary explicitly.

Align timestamps with request throughput, p95/p99 end-to-end latency, errors,
active streams, logging queue occupancy/drops and config publications. Queue
occupancy excludes batches already drained for sending: low queue depth does
not prove backend delivery success. Correlation suggests what to investigate,
not which object is retaining memory. The
[gateway diagnostics/telemetry documentation follow-up](https://github.com/apache/shenyu-website/issues/1174)
covers navigation separately; the opt-in gateway diagnostic bundle does not
automatically collect JFR/heap dumps, and Admin is not a complete metrics store.

## Interpret symptoms before changing options

| Observation in comparable windows | Next investigation |
|---|---|
| Frequent young GC, stable comparable post-GC heap | Allocation bandwidth and pause/CPU impact; collection frequency alone is not a leak |
| Increasing comparable post-GC/old-generation baseline | Retained objects, caches, logging backlog and long-lived request state; a young-GC post-heap value is not an exact live-set measurement |
| Stable heap, rising RSS or direct memory | Netty buffer ownership, connection lifecycle, native allocations and pool capacity; distinguish active allocations from reserved capacity |
| GC/allocation spikes around config publications | Snapshot size, JSON conversions and cache rebuilds; compare quiet and update windows |
| Latency spikes without matching pauses | Event-loop blocking, CPU throttling, upstream latency and queueing; GC pause time does not cover all GC CPU contention |

### Evaluate one JVM change at a time

First verify whether G1 is active. For G1, fixing young-generation size with
`-Xmn` constrains its adaptive pause-time control. Compare the same representative
workload with and without that setting while holding the collector, heap and
other variables constant. Do not infer a latency improvement from the flag
change alone. Changing collector is a separate experiment after diagnosis.

Budget heap **plus** direct/native buffers, metaspace, code cache, thread stacks,
GC structures and operating headroom against the container limit. `-Xmx` is not
a process-memory limit; percentage-based heap sizing does not reserve the rest
automatically. Neither a universal heap percentage nor a direct-memory cap
fits every plugin set or streaming workload. Validate caps under peak body
sizes, concurrency, retries and slow consumers; too small a direct-memory cap
can cause allocation failures while heap space remains.

`DisableExplicitGC` prevents application-requested `System.gc()` from initiating
collection. Inspect the actual direct/native allocation and cleanup mechanism
before changing it: disabling explicit collection can affect cleanup that
depends on it, while allowing it can introduce pauses. It does not replace
correct pooled-buffer release or fix retained references.

## Reproducible workload matrix

Reuse the [real Divide HTTP gateway benchmark](benchmarks/divide-gateway/README.md)
and its existing output format. Its current script is PowerShell and its
controlled HTTP chain omits full Bootstrap/Admin sync, authentication and
optional logging/AI plugins. Extend the appropriate fixture for each scenario;
do not treat an HTTP-only harness as coverage for every row below.

| Scenario | Vary and record |
|---|---|
| Ordinary HTTP forwarding | Upstream count, request/response sizes, rate and concurrency |
| Body replay and CURRENT/FAILOVER retry | Retry count, replayed body size, timeout, cancellation and final response/body correctness |
| AI streaming | Chunks/s, event bytes, stream duration, concurrent streams, slow consumers and cancellation |
| Logging with a slow sink | Enabled collector/mode, queue capacity, backlog, drops, send latency and recovery after the sink resumes |
| Configuration bursts | Selector/rule/snapshot sizes, publication frequency, sync completion and match-cache on/off |

For each comparison:

1. Keep JDK, collector, CPU/memory limits, upstream behavior, payloads and
   instrumentation equal; record exact revisions and flags. Run gateway,
   upstream and generator in separate processes so their allocation is not
   conflated. Verify that the generator/upstream is not the limiting resource.
2. Warm up until throughput, JIT and occupancy stabilize. State the warmup and
   measurement duration. Repeat both variants, alternate order, and report
   run-to-run variation rather than one best result.
3. Measure gateway allocation rate and bytes/completed request or bytes/chunk
   with the accounting method stated, post-GC occupancy trends, pause
   distributions, GC CPU where available, throughput, errors and end-to-end
   p95/p99 latency. Include body completion and generator scheduling delay;
   aborted requests must not silently disappear from the denominators.
4. Include a longer steady-load/soak phase spanning repeated GC, connection,
   retry and config-update cycles. Compare after workload drain as well as at
   peak. A short successful run cannot exclude slow retention. Define
   acceptance and rollback criteria before testing a JVM option change.

## Historical evidence and limits

The [issue's preliminary local observation](https://github.com/apache/shenyu/issues/8160)
used JDK 21/macOS arm64, G1, a 256 MiB heap, a 1 KiB response and approximately
2,000 requests/s through a production HTTP plugin chain. It reported 36,000
measured requests without errors, approximately 25–27 KB allocated/request,
seven 3.0–4.2 ms G1 evacuation pauses, no Full GC, and 30.6–32.5 MiB used just
after those collections. Gateway, upstream and generator were separate JVMs;
JFR and NMT were enabled. These are the issue author's historical observations,
not measurements newly produced by this documentation change.

That short run did not start full Spring Boot Bootstrap/Admin or include
authentication, AI, logging, retries or data sync. Post-young-GC heap is not a
precise live set. Neither those numbers nor older published 2.6.0/JDK 8
benchmark results are current production capacity, a collector comparison or
evidence that the gateway is leak-free. The
[confirmed cancellation-retention issue #8157](https://github.com/apache/shenyu/issues/8157)
is a separate implementation investigation.

## Command verification and references

The command syntax, 120-second JFR output, rotating GC-log option and NMT
summary/baseline/diff were exercised with OpenJDK 17.0.20 on macOS x86_64 using
an isolated allocating JVM. This checks the diagnostic commands, not Bootstrap
performance, container attach permissions or a full deployment. Validate those
deployment-specific paths and permissions before an incident.

- [JDK 17 diagnostic commands](https://docs.oracle.com/en/java/javase/17/docs/specs/man/jcmd.html)
- [JDK 17 G1 tuning](https://docs.oracle.com/en/java/javase/17/gctuning/garbage-first-garbage-collector-tuning.html)
- [JDK 17 Native Memory Tracking](https://docs.oracle.com/en/java/javase/17/vm/native-memory-tracking.html)
- [JDK 17 Java options and unified logging](https://docs.oracle.com/en/java/javase/17/docs/specs/man/java.html)
