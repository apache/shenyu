<!--
  Licensed to the Apache Software Foundation (ASF) under one or more
  contributor license agreements.  See the NOTICE file distributed with
  this work for additional information regarding copyright ownership.
  The ASF licenses this file to You under the Apache License, Version 2.0
  (the "License"); you may not use this file except in compliance with
  the License.  You may obtain a copy of the License at

      http://www.apache.org/licenses/LICENSE-2.0

  Unless required by applicable law or agreed to in writing, software
  distributed under the License is distributed on an "AS IS" BASIS,
  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
  See the License for the specific language governing permissions and
  limitations under the License.
-->

# Virtual thread execution proposal

Design deliverable for [#6369](https://github.com/apache/shenyu/issues/6369).
Status: proposal; the configuration and Java types below do not exist yet.
Inventory: `2.7.2-SNAPSHOT` at `297ac6f84`, reviewed on 2026-10-21.
This proposal complements the [server JDK migration plan](server-jdk-upgrade.md).
It does not change [#7602's Java 17 / Boot 3 client migration target](../shenyu-register-center/CLIENT_MIGRATION.md)
or the separate legacy registration wire-compatibility obligation.

## Decision

Keep Reactor Netty and the reactive plugin chain. Introduce an opt-in executor
for identified blocking operations, with the same capacity, deadline,
cancellation and context rules for platform and virtual execution. Do not switch
all gateway work or every `boundedElastic()` caller to virtual threads.

Java 21 finalized virtual threads in [JEP 444](https://openjdk.org/jeps/444).
They help scale work that waits on blocking I/O; they do not make CPU work
faster. Use a thread per admitted task rather than pooling virtual threads.
The [JDK 21 guide](https://docs.oracle.com/en/java/javase/21/core/virtual-threads.html)
also describes pinning, thread-local overhead and diagnostics. The choices below
are ShenYu design decisions; performance improvements remain to be measured.

## Current execution inventory

Paths are relative to the repository. These are application-owned execution
domains, not an inventory of every internal thread in third-party libraries.

| Source / domain | Current behavior | Proposed disposition |
| --- | --- | --- |
| [ShenyuWebHandler](../shenyu-web/src/main/java/org/apache/shenyu/web/handler/ShenyuWebHandler.java) and [ShenyuConfig.Scheduler](../shenyu-common/src/main/java/org/apache/shenyu/common/config/ShenyuConfig.java) | Scheduler defaults disabled; enabled `fixed` selects `newParallel`, other values select shared `boundedElastic`; scheduling uses `subscribeOn` | Preserve current chain scheduling. A whole-chain switch does not control later async callbacks or make CPU work suitable for virtual threads |
| [ShenyuThreadPoolConfiguration](../shenyu-web/src/main/java/org/apache/shenyu/web/configuration/ShenyuThreadPoolConfiguration.java), [ShenyuThreadPoolExecutor](../shenyu-common/src/main/java/org/apache/shenyu/common/concurrent/ShenyuThreadPoolExecutor.java) and `TaskQueue` | Shared platform executor with queue/memory controls and explicit rejection; optional queue sizing uses Byte Buddy instrumentation | Preserve concrete bean/API and memory protections. Add a separate execution service; do not replace this bean with an unrelated `ExecutorService` |
| [ApacheDubboProxyService](../shenyu-plugin/shenyu-plugin-proxy/shenyu-plugin-rpc/shenyu-plugin-dubbo/shenyu-plugin-apache-dubbo/src/main/java/org/apache/shenyu/plugin/apache/dubbo/proxy/ApacheDubboProxyService.java) | Deferred reference resolution/invocation on `boundedElastic`; explicitly snapshots/restores `RpcContext` attachments; response uses a future | First request-path candidate for the synchronous reference/invocation portion. Keep the asynchronous RPC future reactive and test cancellation separately |
| [HTTP sync service](../shenyu-sync-data-center/shenyu-sync-data-http/src/main/java/org/apache/shenyu/sync/data/http/HttpSyncDataService.java), `AccessTokenManager`, WebSocket `ShenyuWebsocketClient` | One long-polling worker per configured Admin; separate token-refresh scheduler and bounded reconnect executor | Possible later blocking long-poll task candidate. Preserve per-server sequencing, reconnect limits and refresh timers; keep a separate budget from request plugins |
| [ShenyuLoaderService](../shenyu-web/src/main/java/org/apache/shenyu/web/loader/ShenyuLoaderService.java) | Scheduled plugin-directory scanning | Keep periodic trigger on a platform scheduler; consider file/jar I/O offload only after measuring it |
| [AbstractLogCollector](../shenyu-plugin/shenyu-plugin-logging/shenyu-plugin-logging-common/src/main/java/org/apache/shenyu/plugin/logging/common/collector/AbstractLogCollector.java) and log consumers | Platform executor drains buffered logs with batch/capacity semantics; client shutdown owns additional lifecycle work | Later candidate for blocking sink sends, not a task per log. Preserve buffer bounds, batch order, drop policy and draining |
| [OrderlyExecutor](../shenyu-disruptor/src/main/java/org/apache/shenyu/disruptor/thread/OrderlyExecutor.java), `SingletonExecutor`, Disruptor factories | Hash-selected serial lanes and dedicated threads | Keep ordering. The field named `virtualExecutors` represents hash-ring lanes, not JDK virtual threads |
| [HierarchicalWheelTimer](../shenyu-common/src/main/java/org/apache/shenyu/common/timer/HierarchicalWheelTimer.java), memory calculator, registry watchers and heartbeat | Dedicated timer/periodic workers | Keep timer/ordering semantics. Offload selected blocking callbacks independently; do not create a virtual thread as a repeating timer |
| [ManagedRemoteMcpCatalog](../shenyu-plugin/shenyu-plugin-agent-gateway/src/main/java/org/apache/shenyu/plugin/agent/gateway/remote/ManagedRemoteMcpCatalog.java) | Dedicated `newBoundedElastic(1, 16, ...)`, version checks and serialized reload lifecycle; bounded blocking during startup/close | Preserve single-writer reload ordering and lifecycle timeouts; changing its executor requires separate evidence |
| Cache plugins and AI sensitive-word plugin | Memory-cache operations/cache-body handling and automaton work use `boundedElastic` or `publishOn` | Review operation type first. Memory access, parsing and automaton construction are not blocking-I/O candidates merely because they use this scheduler |
| WASM API/base plugins | Synchronous guest exports, instance-pool contention and host callbacks | CPU/guest-budget isolation is a separate problem. Virtual threads cannot stop an infinite loop or enforce guest memory/deadline budgets |
| Admin health checks, cluster election, HTTP/WebSocket publication, AI-key resolver and alert dispatch | Separate scheduled/worker pools, with DB/network work and task bounds | Separate Admin workstream; preserve transactions, publication ordering and existing limits before considering any migration |

Netty event loops, nonblocking HTTP/gRPC/WebSocket/AI streams, selector/rule
matching, hashing, serialization, compression, cryptography and WASM guest
computation remain on their existing or explicitly bounded CPU execution paths.
Retain protocol-client callback executors until their thread-affinity contract
has been reviewed.

## Execution abstraction

Add a main-owned `shenyu-executor` module with a Java 17-compatible, JDK-only API.
Keep Reactor adaptation in `shenyu-plugin-base` and wiring in the appropriate
starter, so plugins do not depend on `shenyu-web` and the dependency graph has
no runtime-to-plugin cycle. Proposed API shape:

```java
interface BlockingTaskExecutor {
    <T> TaskHandle<T> submit(String domain, Callable<T> task, Duration timeout);
}

interface TaskHandle<T> {
    CompletionStage<T> completion();
    void cancel();
}
```

These names are illustrative API contracts, not implemented source. A domain
is a bounded registered value, such as `dubbo` or `http-sync`, never a
request ID, selector ID or user-controlled label. The implementation owns task
admission, queueing, timeout, context decoration and lifecycle. Plugins receive
the service by injection; they do not construct executors per request.

Provide a platform backend and a Java 21 capability adapter. While main artifacts
remain Java 17-compatible, the adapter may resolve stable public JDK APIs
reflectively only after checking runtime capability: `Thread.ofVirtual()`,
the public `Thread.Builder` interfaces for naming/factory settings, and
`Executors.newThreadPerTaskExecutor(ThreadFactory)`. Cache resolved handles;
do not reflect on a non-public builder implementation. Keep Java 21 types out
of signatures, constant-pool class references and eagerly initialized classes
loaded on Java 17. Verify this with Java 17 startup/linkage tests.

This allows opt-in Java 21 runtime testing without raising every artifact's
compiler baseline or distributing mixed-baseline providers through automatic
service loading. After the separately announced server baseline becomes 21,
a direct-API adapter can replace the reflective adapter in its own change.
Java 21-compiled server artifacts themselves cannot fall back to a Java 17 JVM.

The Reactor adapter must submit lazily on subscription and connect cancellation
to `TaskHandle.cancel()`. Never call `Future.get()`, `join()`, semaphore
`acquire()` or a blocking queue `put()` on a Netty event loop. Adapt an
asynchronous result without blocking a virtual worker for the entire response
stream. Preserve downstream demand and buffer ownership. Do not silently change
the public `ShenyuPlugin.execute/before/after` contracts.

## Proposed configuration and fallback

The following is a future configuration contract, not a runnable setting in the
current release. Initial defaults are deliberately explicit and must be tested
before implementation is enabled:

```yaml
shenyu:
  execution:
    blocking:
      mode: platform                 # platform | virtual
      fallback: platform             # platform | fail
      plugins: []                    # empty means no request plugin is migrated
      max-concurrency: 64
      max-pending-tasks: 128
      task-timeout: 3s
      shutdown-grace-period: 30s
```

Capacity/deadline values apply to the new service's first request domain and
are proposed defaults, not replacements for current shared-pool settings.
Validate positive concurrency/timeouts, nonnegative pending capacity and known
plugin/domain names. `max-pending-tasks=0` means immediate rejection when all
execution slots are occupied. Add independently bounded configuration for each
later control-plane domain; one global queue must not let slow RPC calls starve
config sync or heartbeat.

| Condition | Required behavior |
| --- | --- |
| Default mode or plugin absent from the allowlist | Existing execution path remains unchanged; the new service's opt-in adapters are not invoked |
| Virtual requested, Java 21 capability available | Use named per-task virtual threads behind admission limits; log the resolved mode once at startup |
| Virtual unavailable and `fallback=platform` | Warn once with a reason and use the bounded platform backend with identical task semantics |
| Virtual unavailable and `fallback=fail` | Fail configuration/startup before accepting traffic |
| Invalid configuration | Fail binding/startup; do not conceal it as a capability fallback |
| Runtime saturation, task error or timeout | Apply the task's normal rejection/error semantics. Never retry the same call on another backend as a fallback |

Mode is resolved at startup. Rollback changes configuration and restarts/drains
the instance; live backend replacement is out of scope. Keep
`shenyu.scheduler.*` and `shenyu.shared-pool.*` unchanged. Do not enable
`spring.threads.virtual.enabled` as a substitute for controlling plugin work.

Reactor 3.6 exposes a [global virtual bounded-elastic option](https://projectreactor.io/docs/core/3.6.7/api/reactor/core/scheduler/Schedulers.html)
on Java 21. That is an alternative experiment, not this rollout mechanism:
the global switch affects multiple shared callers and provides no per-plugin
admission contract. Do not set it globally in the default distribution. Record
the resolved Reactor version and property values in every experiment.

## Admission, timeout and cancellation semantics

Bound active physical tasks to `max-concurrency` and queued task records to
`max-pending-tasks`. Include task context/retained payload in memory accounting;
do not remove existing memory protections or accept arbitrarily large captured
request bodies. Both backends use the same admission state machine:

1. On subscription, capture the allowed request context and a monotonic deadline.
   Admit immediately into an execution slot or a bounded FIFO pending queue.
   If neither has capacity, reject immediately. No `CallerRunsPolicy`.
2. Before starting queued work, check cancellation/deadline again. Account for
   queue wait in the task timeout and discard expired/cancelled queued work.
   A single platform timer schedules deadlines; its thread never runs user work.
3. Run an admitted task once. Complete its result with the original value/error,
   unless its downstream result has already timed out or been cancelled.
4. For an active cancellation/timeout, cancel the underlying future with
   interruption and invoke transport cancellation where supported. A terminal
   downstream result does not prove that the blocking call physically stopped.
5. Release the active permit only in the running task's actual `finally`.
   Pending cancellation removes its queue record once. Handle cancellation
   racing with submission/start without a leaked or double-released permit.
   Do not rely on `Future.isDone()` after cancellation as proof of task exit.

For a task ignoring interruption, retain its capacity charge until it exits,
record it as still running after cancellation, and reject excess work within
the configured limits. No unsafe thread stop and no replacement task beyond the
budget. This containment does not solve WASM execution deadlines or native
calls that never return.

For opted-in request plugins, map capacity rejection to the gateway's 503 error
path and deadline expiration to its 504 path, preserving tracing/error format.
If headers/body have already been committed, cancel/terminate through the
existing response error handling instead of writing a second response.
Subscriber cancellation writes no response. Preserve plugin/business errors
and existing retry policy; the executor introduces no automatic retries or
duplicate side effects.

On shutdown, stop admission, cancel queued work and allow active work up to the
configured grace period, then request interruption/cancellation and report
remaining tasks. Do not wait forever on `ExecutorService.close()` from an event
loop. Integrate with Spring lifecycle; thread daemon/liveness behavior must not
replace lifecycle ownership.

## Context, pinning and observability

Capture Reactor context and the intended tracing/security/MDC fields at the
subscription boundary. Install only those fields in a worker and restore/clear
them in `finally`; do not blindly inherit thread-local state. Preserve the
Dubbo attachment snapshot and cleanup around invocation/future lookup. Test two
overlapping requests with distinct identities and traces, plus failure,
cancellation and a reused platform worker. Spring transaction context is
thread-bound; moving Admin work across threads requires an explicit transaction
boundary and is outside the first gateway change.

For Java 21, inspect blocking while holding monitors and native/foreign calls
with JFR, rather than assuming every `synchronized` statement causes a problem.
The [JDK guide's diagnostics](https://docs.oracle.com/en/java/javase/21/core/virtual-threads.html)
cover `jdk.VirtualThreadPinned` and thread dumps.
[JDK 24's release notes](https://www.oracle.com/java/technologies/javase/24-relnote-issues.html)
describe JEP 491's monitor-pinning improvement; do not assume this improvement
exists on Java 21 or that it removes every native-call limitation.

Name workers `shenyu-blocking-<domain>-<sequence>`. Expose requested/resolved
mode, runtime capability and fallback reason in startup logs and diagnostics.
Use bounded domain/mode/outcome labels, never request IDs or credentials.
Measure submitted, active, pending, completed, rejected, timed-out and cancelled
tasks; actual running-after-cancellation count; queue wait and execution
duration; and shutdown drain duration. Preserve trace parentage across the
handoff, with queue wait and execution observable separately. Use JFR to inspect
carrier/platform thread counts, pinning, allocation and parked tasks; OS thread
counts alone do not measure virtual-thread concurrency.

## Benchmark and verification gates

All rows below are planned experiments. No throughput, latency or memory
improvement is claimed by this document.

Compare (A) unchanged Java 17/platform, (B) unchanged Java 21/platform,
(C) Java 21/new service/platform and (D) Java 21/new service/virtual.
C versus D isolates backend effects from a JDK upgrade or a new admission policy.
Use equal active/pending limits and downstream connection pools for C/D;
report baseline differences for A/B rather than masking them.

| Scenario | Workload / assertions |
| --- | --- |
| Blocking RPC adapter | Disposable provider with fixed 10/100/1000 ms delays; concurrency 1/64/256/1024; verify response values, attachments, trace IDs and provider invocation count |
| Nonblocking HTTP control | Same payloads/rates through normal HTTP routing, including SSE and a slow consumer; verify the untouched path does not gain queueing or buffers |
| Overload and cancellation | Downstream stalls, refused connection and interruption-ignoring test task; cancel while queued/running; verify bounded active/pending memory and no duplicate invocation |
| CPU control | Rule matching/serialization or a bounded CPU fixture; confirm it remains outside the virtual executor |
| Config/order and shutdown | Apply ordered updates during traffic, restart/drain, and exercise platform fallback; verify route state, no lost/reordered callbacks and no leaked owned executors |

Use an external fixed-rate load generator, a controlled backend and fresh
processes with equal heap/CPU/container limits. Record source SHA, JDK/vendor,
OS/architecture, flags, config, payload, provider/connection limits and actual
offered/achieved load. Warm up for 2 minutes, measure for 5, repeat at least
5 times, and report all runs plus median/spread. Count rejects/timeouts/errors
in achieved-load and latency reports so fast rejection cannot look like success.
Collect p50/p95/p99 end-to-end latency, throughput, CPU, RSS/heap, allocation/GC,
active/pending tasks, platform/carrier counts and pinned duration/stacks.

Proposed promotion gates, agreed before measuring:

- Correctness: zero duplicate invocations/context leakage, bounded buffers and
  exact permit cleanup across success/failure/queue cancellation/start races.
  Reactive cancellation and result propagation pass deterministic tests.
- Overload: active and pending counts never exceed limits, including calls that
  ignore interruption; drain completes within grace or reports retained tasks.
- Nonblocking control: no more than 5% p99 or throughput regression versus B
  under matched load; investigate regressions outside run-to-run variability.
- Blocking benefit: versus C, demonstrate at least 15% throughput improvement
  at the same p99 budget, or 15% lower RSS at the same successful throughput
  with no more than 5% p99 regression. Publish confidence/spread and resource
  limits; do not promote from a single favorable run.

## Implementation sequence and rollback

1. Add the service API, bounded platform backend, context/cancellation adapter,
   metrics and race tests without migrating callers. Java 17/21 startup tests
   and capability/fallback tests are required.
2. Add the Java 21 capability adapter and opt-in Dubbo synchronous-call adapter.
   Keep RPC-future handling reactive and preserve attachment cleanup. Test the
   existing plugin suite on both backends plus a real disposable provider.
3. Publish C/D measurements and a canary recipe; enable only an explicit
   `plugins: [dubbo]` configuration after the correctness gates pass.
4. Review long-polling, log sinks and Admin candidates separately. Require
   ordering, independent budgets, lifecycle and integration evidence for each.

Set `mode: platform` to retain the new service's admission/deadline contract
while changing backend; remove the plugin from the allowlist to restore its
original execution path. Restart and drain rather than replaying active calls.
The server JDK rollback rules remain those in the JDK proposal. No registration
payload, split-client dependency or client/foundation Java 17 floor changes are needed.

Runtime maintainers own admission/lifecycle and the Reactor adapter; plugin
maintainers own invocation/cancellation/context semantics; build/release
maintainers own the JDK capability matrix; observability maintainers review
metrics/tracing cardinality. Review this proposal and benchmark thresholds
before enabling a default. Structured concurrency and preview APIs are outside
this design.
