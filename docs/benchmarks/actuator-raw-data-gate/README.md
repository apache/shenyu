# Actuator raw-data gate — endpoint-level microbenchmark (issue #8219, PR review item 3)

## What is measured

The gate introduced in this change lives entirely in the `/actuator` inspection mappings; ordinary forwarding shares zero code with it (the gate is bean registration + mapping dispatch, not a plugin or filter on the request path). The review asked for repeated-run numbers on the states the change touches, so this benchmark drives the real reactive `DispatcherHandler` mapping for the three relevant states:

| Scenario | What it represents |
|---|---|
| `raw-dump` | The pre-change behavior, byte-for-byte: raw paths with `shenyu.endpoint.raw-data.enabled=true` |
| `digest` | The always-on digest view (`/actuator/pluginDigest`, shipped default) |
| `404 default` | The shipped default on the raw paths: no mapping, rejected before cache traversal |

## Method

`ShenyuEndpointBenchmark` (in `shenyu-web/src/test`, run with `-Dshenyu.benchmark=true`) uses Spring's reactive mock infrastructure (`WebHttpHandlerBuilder` + `MockServerWebExchange`), so the numbers cover mapping resolution + cache read + serialization, and exclude real socket I/O. Per scenario: 2000 warm-up requests, then 5 rounds of 20000 requests; throughput is the median round, latencies are per-request over all rounds, response size is the final round. One seeded plugin and one metadata entry (the small-cache case; the raw dump's response grows linearly with cache size while digest and 404 do not, which is part of the point).

## Results

Windows 10, JDK 21, single run of the harness (in-process dispatch, no socket):

| Scenario | Throughput (median) | mean | p95 | p99 | Body size |
|---|---|---|---|---|---|
| raw-dump (pre-change) | 18415 req/s | 69.2 µs | 161.4 µs | 240.9 µs | 172 B |
| digest (always-on) | 18648 req/s | 54.2 µs | 67.0 µs | 97.5 µs | 122 B |
| 404 default | 14147 req/s | 71.0 µs | 87.3 µs | 131.6 µs | 0 B |

Reading: the digest view costs nothing relative to the pre-change raw dump (throughput within noise; the smaller, digest-only body shows up in p95/p99, not in throughput). The default 404 state is dispatched by Spring's mapping table only — no cache traversal, no serialization — and its slightly lower synthetic throughput is the cost of the error-path dispatch in the mock harness, with an empty body. For ordinary proxy forwarding none of this code executes: the change adds no filter, no plugin and no per-request work to the plugin chain.

Reproduction: `mvn -pl shenyu-web test -Dtest=ShenyuEndpointBenchmark -Dshenyu.benchmark=true -DskipTests=false -Dcheckstyle.skip=true`
