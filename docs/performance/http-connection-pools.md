# HTTP connection and acquisition capacity

This change addresses the pool-policy portion of [#7644](https://github.com/apache/shenyu/issues/7644),
under the measurement contract in [#7650](https://github.com/apache/shenyu/issues/7650).
The `shenyu.httpclient` properties default to `FIXED`. The separate keep-alive
default change remains owned by [#6574](https://github.com/apache/shenyu/issues/6574).

## Configuration and compatibility

```yaml
shenyu:
  httpclient:
    # keepAlive remains false by default. Enable it explicitly when testing reuse.
    keepAlive: true
    pool:
      type: FIXED
      name: shenyu-proxy
      maxConnections: 16
      pendingAcquireMaxCount: 32
      acquireTimeout: 3000 # milliseconds spent waiting for a connection
      metrics: true
```

The numbers above are an example, not a capacity recommendation. Connection and
queue limits apply **per remote-address pool**, not across all destinations or
gateway replicas. Budget their sum, including other transports, against the
upstream's measured capacity and the gateway's memory and socket budget.

| Setting | Behavior |
| --- | --- |
| `type` | `FIXED` remains the default. `ELASTIC` retains its configuration name but now honors the same finite limits. `DISABLED` still bypasses pooling. |
| `maxConnections` | Positive integer; the existing Reactor Netty default remains unchanged. Connections are allocated on demand. |
| `pendingAcquireMaxCount` | Omitted: twice `maxConnections`, saturating at `Integer.MAX_VALUE` without overflow. A positive integer sets a queue limit. `-1` explicitly opts into an unbounded queue. Zero and values below `-1` are rejected at startup. |
| `acquireTimeout` | Default remains 3000 ms for FIXED and now applies to ELASTIC. Negative/null values are rejected. Zero explicitly disables the waiting deadline, retaining Reactor Netty's legacy behavior; use a positive finite value for a bounded waiting deadline. |
| `metrics` | Remains opt-in (`false`). Requires `micrometer-core` on the application classpath, as before; the starter declares it as an optional dependency. No exporter is added. |

**ELASTIC migration:** the old branch forced `Integer.MAX_VALUE` connections,
an unbounded pending queue and a zero acquisition timeout, ignoring configured
limits. It now honors `maxConnections`, `pendingAcquireMaxCount` and
`acquireTimeout`. Overloaded ELASTIC deployments can therefore reject or time out
acquisitions where they previously opened additional connections. Choose explicit
limits from a load test before upgrading. FIXED keeps its existing effective
default queue size and timeout; setting a smaller queue is an explicit opt-in.

A full queue fails with `PoolAcquirePendingLimitException`; an expired waiting
deadline fails with `PoolAcquireTimeoutException`. These remain transport errors
handled by the existing plugin error/retry path. This change adds no automatic
retry or new gateway HTTP-status mapping. The measurement fixture maps errors to
503 solely to count load shedding; that is not a production status guarantee.

HTTP cancellation is propagated to a pending provider acquisition through a
request-scoped Reactor context signal. This is needed with the pinned Reactor
Netty 1.1.20 client, whose HTTP subscriber does not cancel its acquisition
subscription until a connection has been delivered. Queue cancellation, active
request cancellation, streaming cancellation and read timeouts are covered by real
HTTP regressions. Cancelling a waiting request releases its queue slot immediately;
the acquisition timer is not used as a substitute for cancellation.

Rollback is a dependency rollback or a return to the previous application version.
For compatibility, explicitly configuring `maxConnections: 2147483647`,
`pendingAcquireMaxCount: -1` and `acquireTimeout: 0` restores the previous ELASTIC
capacity policy. This disables the practical resource bounds and requires
independent admission control; prefer a measured finite policy.

## Metrics and label budget

With `pool.metrics: true`, reuse the six Reactor Netty 1.1.20 gauges:

| Meter prefix `reactor.netty.connection.provider.` | Meaning |
| --- | --- |
| `active.connections` | Acquired connections |
| `idle.connections` | Reusable idle connections |
| `pending.connections` | Waiting acquisition count |
| `total.connections` | Allocated connections |
| `max.connections` | Configured connection limit |
| `max.pending.connections` | Configured pending limit |

That Reactor version does not provide an acquisition-duration/timeout meter.
`shenyu.httpclient.pool.acquire` adds an opt-in Micrometer timer with only two tags:
the static configured pool `name`, and `outcome` in
`success`, `timeout`, `error`, `cancelled`. Its timeout series' count measures
acquisition timeouts. A response read timeout occurs after acquisition and does not
increment that series. `error` includes full-queue rejection and connection errors.
The timer includes connection establishment when a new connection is needed; it
must not be presented as pure pending-queue duration. Cancellation is recorded
once, and successful acquisitions are not later counted as cancellations when the
HTTP body ends. No request path, route, user, session or model identifier is added.

Reactor's existing gauges have `id`, `name` and `remote.address` tags. Keep pool
names static, bound the configured upstream inventory, and apply an application
Micrometer `MeterFilter.maximumAllowableTags` budget to `remote.address` if dynamic
destinations are possible. Dropping a meter after the label budget is exhausted
does not impose an outbound destination/admission limit. Histograms and exporters
are application choices; enabling this timer does not enable HTTP URI metrics.

## Sizing HTTP and AI streams separately

Measure connection occupancy until the **complete body has drained**, including
slow consumers. For HTTP/1.1, average active connections are approximately
successful requests/second multiplied by mean occupancy seconds. Use measured
upstream saturation, tail latency and a stated safety margin to choose a cap;
increasing the queue does not create upstream capacity. The queue's waiting budget
must fit inside the caller's remaining deadline. Do not treat acquisition timeout,
TCP connect timeout, response idle/read timeout and total request deadline as the
same clock.

A 100 ms HTTP response and a 1 s stream require materially different occupancy at
the same arrival rate. The HTTP starter constructs a named provider. The AI proxy
starter currently uses `HttpClient.create()` and the shared Reactor HTTP resources,
independently of `shenyu.httpclient.pool`. Changing these HTTP properties does not
configure the AI transport. The fixture measures both client-construction paths
under a controlled stream workload before any isolation choice. It does not run
the complete AI engine, a paid vendor API or model inference. This change introduces
no new AI pool, global scheduler tuning or forced HTTP/AI isolation.

HTTP/2 multiplexing has separate concurrent-stream/flow-control limits; connection
count is not an HTTP/2 request concurrency bound. TLS, HTTP/2 and full gateway
deployments need their own capacity runs before using these HTTP/1.1 results for
deployment sizing.

## Reproduce the controlled comparison

The fixture lives in the HTTP starter's **test sources** and is not packaged in
the production JAR or run by the normal test suite. It uses a controlled Reactor
Netty upstream and a minimal streaming forwarder using the real `HttpClientFactory`.
This isolates pool behavior; it is not a full ShenYu distribution QPS benchmark.
The direct-upstream case checks upstream/generator capacity. The load generator is
an external k6 1.0.0 binary with its constant-arrival-rate executor; no benchmark
library is added to the runtime. The Python runner uses only the standard library.

Build test classes with the Maven wrapper, keeping Checkstyle enabled:

```sh
./mvnw -B -ntp -pl shenyu-spring-boot-starter/shenyu-spring-boot-starter-plugin/shenyu-spring-boot-starter-plugin-httpclient -am test \
  -Dtest=HttpClientPoolCapacityTest,HttpClientPluginConfigurationTest,HttpClientResponseTimeoutTest,HttpClientPropertiesTest \
  -Dsurefire.failIfNoSpecifiedTests=false -DskipRemoteResources=true \
  -Djacoco.skip=true -Dmaven.javadoc.skip=true
python script/performance/http-pool/run.py snapshot /tmp/http-pool-after-build
python script/performance/http-pool/run.py run \
  --build /tmp/http-pool-after-build --output /tmp/http-pool-after \
  --pending 4 --k6 /path/to/k6 --repeat 3 --warmup 5 --duration 8
```

Snapshot the baseline's compiled classes **before** modifying the production
factory, and run the same fixture without `--pending`. The fixture needs the new
property accessor only to request the comparison policy; the old factory ignores
it. A snapshot copies the compiled factory, properties and test fixture into an
immutable JAR, records its SHA-256 and retains the dependency classpath. Compare
snapshots made from separate checkouts or retain the baseline snapshot before
recompiling. Do not accidentally run both arms against the same overwritten
`target/classes` directory. Keep all other code, flags and upstream behavior fixed.

The runner records raw k6 points, summaries, successful-only latency samples,
pending-queue lifetimes (including release/cancellation or timeout), 20 ms sampled
connection/queue peaks, process CPU time, heap/direct-buffer peaks and open file
descriptors where supported. A benchmark-only timer uses the same parallel
scheduler as Reactor's default pending timer, measuring queue lifetime directly;
it does not replace a production timer. On Windows, POSIX FD count is unavailable
(`-1`); process handles are recorded separately and must not be called FD counts.
Successful throughput and successful-only p99 must be examined together with error
counts and `dropped_iterations`; fast rejections can otherwise make aggregate p99
misleading. Resource peaks are sampled rather than continuous proofs of a limit.

The fixed-overload case's first repetition also retains a JFR `profile` recording.
The fixture shuts down normally so the recording is flushed. Use `jfr summary` and
JDK Mission Control for inspection. Run each arm in isolation from builds and other
load generators. This desktop run uses an active-processor hint, not an OS CPU
quota. For deployment decisions, repeat longer warmup/measurement windows on a
dedicated host, fixed CPU quota, separate generator/upstream machines and the
actual TLS/plugin/configuration set. Record repeat variability rather than claiming
an improvement smaller than the variation.

The [checked-in result report](http-pool-results.md) links the raw samples and states which acceptance
evidence remains outside the local experiment.

```sh
python script/performance/http-pool/report.py \
  --before /tmp/http-pool-before/results.json \
  --after /tmp/http-pool-after/results.json --output docs/performance
```
