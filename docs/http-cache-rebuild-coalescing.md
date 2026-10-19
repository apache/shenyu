# HTTP cache rebuild coalescing

This implements the producer-side experiment in [#7839](https://github.com/apache/shenyu/issues/7839).
It applies to independent committed configuration events handled by the HTTP cache listener.
Selector import batching and the event dispatcher are unchanged.

## Configuration and compatibility

```yaml
shenyu:
  sync:
    http:
      enabled: true
      cache-rebuild-window: 10ms
      cache-rebuild-max-pending-keys: 1024
```

The default window is **0ms**, preserving synchronous event processing. Enable a positive
window only when repeated updates to the same namespace/group justify the latency cost.
Accepted windows are zero or 1ms–1s; the pending-key capacity must be positive.
Configuration takes effect when the listener is constructed; restart after changing it.
Return the window to zero and restart to restore the original event path.

The existing seven event-driven groups use the hook: app auth, plugin, rule, selector,
metadata, proxy selector and discovery upstream. Full startup/periodic refreshes,
including the existing AI proxy API key cache path, retain their existing scope.

## Scheduling and correctness

- Each namespace/group owns at most one queued or running rebuild. Its first event sets
  a fixed deadline. Later events increment a generation without resetting that deadline.
- A rebuild reads the authoritative group from the database, then uses the existing Gson,
  MD5 and version logic. An empty database result publishes an authoritative empty group.
  Identical JSON preserves the previous cache object and version.
- A generation change during a rebuild schedules another pass. Comparing the generation
  and removing the pending marker share the same monitor as event admission, preventing
  a lost wakeup at the end of a pass.
- Only snapshot work is combined. Every event still reaches the dispatcher, other
  transport listeners and per-event callbacks. DELETE/UPDATE payloads are never merged
  into a synthetic event. Existing after-commit delivery is retained; rollback cannot
  enqueue a rebuild. Event payloads are not retained in the queue.
- Successful publication queues the existing long-poll notification. When enabled,
  long-poll registration also rechecks MD5/version on the notification scheduler to close
  the compare/register race.
- Namespace locks serialize snapshot loads/publications with periodic and client-triggered
  repair, preventing an older concurrent read from overwriting a newer publication.
  No event triggers a global refresh.
- One separate worker performs rebuilds; the existing notification/timer scheduler keeps
  its existing thread count. At capacity, a new key rebuilds synchronously on its committing
  caller. Existing pending keys continue to coalesce. No unbounded per-event task list is created.
- A failed rebuild retains its dirty generation and retries after at least one second,
  yielding to other keys without a tight database-failure loop. The last successfully
  published cache remains readable. Shutdown cancels
  queued work and interrupts the worker.

For a healthy idle key, the **intentional added delay is at most the configured window W**.
Publication also needs queue wait Q and rebuild time R, followed by notification scheduling.
An event arriving during a rebuild can additionally wait for that in-flight read to finish.
With at most K pending keys and bounded rebuild time Rmax, one queue pass costs at most
approximately K × Rmax; a slow database, GC pause or blocked notification scheduler can
exceed a wall-clock target. This is not a promise of 10ms end-to-end delivery under overload.
The local benchmark below gates final cache publication at **250ms** from the last committed
version of every key. Production rollout needs an explicit end-to-end convergence budget.

## Reproduce the tests

Use Java 17 and the Maven wrapper (on Windows, `./mvnw.cmd`).

```sh
./mvnw -B -ntp -pl shenyu-admin -am test \
  -Dtest=HttpCacheRebuildCoordinatorTest,HttpCacheRebuildTest,AbstractDataChangedListenerTest,HttpLongPollingDataChangedListenerTest,HttpSyncPropertiesTest,DataChangedEventDispatcherTest,SelectorServiceTest \
  -Dsurefire.failIfNoSpecifiedTests=false -Djacoco.skip=true \
  -DskipRemoteResources=true -Dmaven.javadoc.skip=true
./mvnw -B -ntp -pl shenyu-admin apache-rat:check
```

Tests cover concurrent independent H2 commits, rollback, event/callback preservation,
DELETE followed by UPDATE, authoritative empty groups, namespace/group separation,
capacity fallback, unchanged MD5/version, disabled mode, notification ordering and the
registration race. Deterministic coordinator tests cover in-flight writes, retry without
another event, fixed deadlines, key reuse, rejected scheduling and shutdown.
The existing selector import tests run unchanged.

PR verification on master `de93952d9` plus this change: **99 tests passed**, with no failures or errors; reactor Checkstyle
passed. The admin RAT check approved all 1037 scanned resources, including the new Java files.

## Reproduce the producer benchmark

```sh
./mvnw -B -ntp -pl shenyu-admin -am test \
  -Dtest=HttpCacheRebuildBenchmark -Dsurefire.failIfNoSpecifiedTests=false \
  -Dshenyu.http.benchmark=true -DargLine="-Xms512m -Xmx512m" \
  -Djacoco.skip=true -DskipRemoteResources=true -Dmaven.javadoc.skip=true
```

The output is `shenyu-admin/target/http-cache-rebuild-benchmark.csv`. Optional properties:
`shenyu.http.benchmark.rows` (1000), `events` (100), `handleChars` (256),
`warmup` (5), `samples` (3), and `output` (the CSV path); all use the same
`shenyu.http.benchmark.` prefix. Set `handleChars=4096` to repeat with larger handles.

Each variant uses real H2 commits and queries, the real event dispatcher, the HTTP listener,
and complete DTO serialization/hashing. A small JDBC-backed service fixture substitutes for
MyBatis; SQL counts are actual fixture SELECTs, not estimates of production mapper query
fan-out. RULE and SELECTOR have 1000 rows per key, with synthetic handles and no conditions.
The matrix covers one/two namespaces × one/two groups, with burst, 5ms and 25ms spacing.
Every update is a separate transaction, never an import. A serial producer follows a fixed
arrival schedule; if it falls behind, it catches up. Burst mode runs as fast as the caller
can commit, so elapsed time is part of the comparison, not a fixed-rate saturation claim.

Each trial warms the write/rebuild and fetch paths, alternates variant order across three samples,
and runs a concurrent in-process `fetchConfig` reader. CSV metrics include full SELECT,
serialization and hash counts, live-JVM-thread allocated bytes, JVM process CPU time, GC count/time,
edit p99, fetch p99, lag for published versions and final convergence. Allocation totals
include the reader and JVM background threads alive at sampling; process CPU includes
all JVM threads (Windows per-thread CPU deltas are too coarse here). These are not isolated
production allocation profiles. The fetch path is measured in-process, without HTTP,
gateway plugins, a network or business traffic. Intermediate overwritten versions are
intentionally not required to be published.

## Recorded local results

Measured on 2026-10-19 with Microsoft OpenJDK 17.0.18+8-LTS, Windows 11
(10.0.26200), AMD Ryzen 9 8945HX, 32 logical processors, approximately 32 GiB RAM,
and a fixed 512 MiB JVM heap. Base checkout: `297ac6f849857bd0c2ad368709013e98494b0330`
plus this change. Each payload size ran in a separate JVM with three samples per scenario,
five warmup traces of 100 independent commits, and 20 fetch warmups per key.

Both runs used the command above with an explicit output path. The second also set
`-Dshenyu.http.benchmark.handleChars=4096`. All other parameters were identical.

Raw results: [256-character handles](benchmarks/http-cache-rebuild-256.csv) and
[4096-character handles](benchmarks/http-cache-rebuild-4096.csv), 72 measured trials each.
Each table entry is the median of three trials, except the final column, which is the
maximum final convergence across those trials. Pairs show **disabled → 10ms window**.
Every full rebuild below is one actual fixture SELECT, one full serialization and one hash.
Allocation totals include concurrent reads and depend on trace completion time.
CPU and GC observations are retained in the CSV. Windows CPU counters remain coarsely
quantized (including zero deltas); do not interpret those zero values as zero CPU use or
derive a CPU-speedup claim from this run. No gateway business p99 is represented here.

### 256-character handles

| Namespaces × groups | Spacing | Full rebuilds | Allocated MiB | Edit p99 ms | Fetch p99 ms | Max final convergence, enabled (ms) |
| --- | --- | --- | --- | --- | --- | --- |
| 1 × 1 | burst | 100 → 1 | 206.8 → 12.2 | 2.396 → 0.078 | 1.025 → 0.679 | 23.166 |
| 1 × 1 | 5ms | 100 → 33 | 227.5 → 101.0 | 2.489 → 0.294 | 0.931 → 0.853 | 16.555 |
| 1 × 1 | 25ms | 100 → 100 | 332.4 → 339.4 | 1.848 → 0.204 | 0.912 → 0.903 | 17.781 |
| 1 × 2 | burst | 100 → 2 | 219.8 → 16.3 | 1.206 → 0.031 | 1.657 → 0.733 | 26.056 |
| 1 × 2 | 5ms | 100 → 48 | 264.2 → 164.4 | 1.713 → 0.154 | 0.931 → 1.205 | 16.323 |
| 1 × 2 | 25ms | 100 → 100 | 481.0 → 488.3 | 2.193 → 0.181 | 0.950 → 0.978 | 17.464 |
| 2 × 1 | burst | 100 → 2 | 214.1 → 11.0 | 1.331 → 0.024 | 0.814 → 0.773 | 16.311 |
| 2 × 1 | 5ms | 100 → 48 | 258.8 → 161.4 | 2.138 → 0.111 | 1.044 → 0.817 | 12.363 |
| 2 × 1 | 25ms | 100 → 100 | 468.7 → 475.8 | 1.944 → 0.175 | 0.904 → 0.944 | 17.325 |
| 2 × 2 | burst | 100 → 4 | 231.8 → 30.7 | 1.161 → 0.023 | 0.784 → 0.750 | 17.576 |
| 2 × 2 | 5ms | 100 → 73 | 320.6 → 280.3 | 1.723 → 0.150 | 0.841 → 0.805 | 18.476 |
| 2 × 2 | 25ms | 100 → 100 | 756.8 → 775.2 | 2.602 → 0.175 | 0.803 → 0.819 | 17.411 |

### 4096-character handles

| Namespaces × groups | Spacing | Full rebuilds | Allocated MiB | Edit p99 ms | Fetch p99 ms | Max final convergence, enabled (ms) |
| --- | --- | --- | --- | --- | --- | --- |
| 1 × 1 | burst | 100 → 1 | 2386.1 → 76.0 | 9.858 → 0.078 | 8.011 → 5.187 | 27.260 |
| 1 × 1 | 5ms | 100 → 17 | 2406.7 → 797.0 | 9.685 → 0.370 | 8.365 → 6.594 | 25.018 |
| 1 × 1 | 25ms | 100 → 81 | 3932.1 → 3679.6 | 9.102 → 0.214 | 6.539 → 7.982 | 26.345 |
| 1 × 2 | burst | 100 → 2 | 2931.8 → 114.4 | 8.748 → 0.032 | 6.604 → 5.271 | 26.457 |
| 1 × 2 | 5ms | 100 → 35 | 3029.9 → 1202.5 | 9.950 → 0.226 | 7.511 → 9.456 | 20.101 |
| 1 × 2 | 25ms | 100 → 100 | 4414.0 → 4490.6 | 17.781 → 0.243 | 20.706 → 20.806 | 33.166 |
| 2 × 1 | burst | 100 → 2 | 2853.6 → 108.5 | 21.277 → 0.030 | 21.551 → 7.636 | 41.079 |
| 2 × 1 | 5ms | 100 → 34 | 2924.8 → 1504.0 | 19.492 → 0.205 | 19.770 → 17.383 | 38.734 |
| 2 × 1 | 25ms | 100 → 100 | 5704.1 → 6123.5 | 17.301 → 0.235 | 17.769 → 15.769 | 31.384 |
| 2 × 2 | burst | 100 → 4 | 2960.7 → 199.1 | 16.131 → 0.024 | 20.821 → 5.867 | 53.309 |
| 2 × 2 | 5ms | 100 → 64 | 2983.6 → 2010.2 | 11.681 → 0.209 | 7.588 → 17.584 | 34.186 |
| 2 × 2 | 25ms | 100 → 100 | 6158.4 → 6172.5 | 17.628 → 0.293 | 15.166 → 16.400 | 32.356 |

Across all measured burst scenarios, 100 committed events required **1–4 full rebuilds**,
instead of 100. The largest enabled final convergence was **53.309ms**, below the 250ms
local publication budget. With 256-character handles, 25ms-spaced updates performed
100 rebuilds in both modes and slightly increased allocation when enabled: use zero for
that workload. With larger handles, some 25ms traces still overlapped a rebuild and merged;
this does not establish a universal benefit for sparse traffic.

These measurements support an opt-in window for repeated hot-key updates. They do not
support changing the default or promising lower deployed business-request p99.

## Deployment validation boundary

These producer results do **not** establish deployed gateway business-request p99 or
network convergence. The local Docker engine was unavailable during this run, so the
Docker HTTP integration suite and an admin + gateway + backend load run were not executed.
The default therefore remains disabled.

Before enabling in a deployment, repeat baseline/window runs with identical database,
admin, gateway and backend resources, warm all request paths, repeat the same committed
update trace, and measure database statements, CPU/allocation/GC, gateway convergence,
business-request p99 and errors. Require a query/allocation benefit larger than sample
variation while meeting the chosen convergence budget; leave the window at zero for
sparse updates. See the repository's [integration test setup](../shenyu-integrated-test/README.md)
and [benchmark contract](https://github.com/apache/shenyu/issues/7650).
