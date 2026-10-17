# HTTP pool capacity experiment — 2026-10-15

This is a controlled HTTP/1.1 client-pool experiment for #7644, not a full gateway throughput claim.
The required resource bound is the reason for the change; no maximum sustainable QPS or universal latency improvement is claimed.

See [policy, migration and reproduction](http-connection-pools.md) and the retained
[per-run summaries, successful latency samples and queue lifetimes](http-pool-samples.json).

## Contract

- Reviewed source baseline: `ad89a887ad25fbfee6bc050432395324ab3ffd18`.
- Baseline uses the original factory bytecode. The fixture/property accessor and optional Micrometer dependency were added before snapshotting, without changing that factory's policy.
- Candidate uses the changed factory. Baseline: implicit FIXED queue `2 * 8 = 16`, legacy unlimited ELASTIC. Candidate: 8 connections and an explicitly selected queue of 4 for both types, 250 ms acquisition timeout.
- Keep-alive is an explicit test variable; its production default is unchanged. Both arms have metrics enabled. The AI client control uses the unchanged `HttpClient.create()` construction path, not the HTTP factory's provider or a complete AI engine.
- Each case has a fresh JVM, 5 s warmup and 8 s fixed-arrival measurement, repeated three times. All figures below are medians; individual samples and variability are retained.
- Benchmark runtime: `java version "21.0.9" 2025-10-21 LTS` (also verified in both JFR recordings). Build/test JDK: `21.0.10+7-LTS`; OS: `Windows-10-10.0.26200-SP0`; host logical CPUs: 32.
- JVM flags: `-Xms128m -Xmx256m -XX:MaxDirectMemorySize=64m -XX:ActiveProcessorCount=2`. Two worker threads each for upstream, forwarder and client. The processor hint is not an OS CPU quota.
- Generator, forwarder and upstream share one desktop. Background desktop activity is not controlled. No builds or other benchmark arms ran concurrently with either retained arm.
- Upstream: `/fast` returns a small `ok` body; `/slow` delays 100 ms; `/stream` sends ten SSE chunks over 1 s. The direct slow-upstream control runs at the same 160/s offered overload rate.
- Fixture JAR SHA-256 (baseline): `d2b7b8fbe94861bad22e5d511ec99bb5e553f031e99b34bb1b94a7079e257b99`.
- Fixture JAR SHA-256 (candidate): `55d6b7087f8b2a30b59a4d13681593653c20e92ca910a266cd4f41bcc42f8195`. JAR snapshots include production and fixture classes; the tracked-source diff hash alone does not cover new untracked files.
- k6: `k6.exe v1.0.0 (commit/41b4984b75, go1.24.2, windows/amd64)`.

## Successful traffic and latency

k6 rates include completion/drain time. Error percentage includes rejected overload requests; successful-only p99 prevents fast failures from making the latency comparison look better.

| Case | Offered/s | Successful/s before → after | Successful p99 ms before → after | Error % before → after |
| --- | ---: | ---: | ---: | ---: |
| direct-slow-control | 160 | 157.88 → 158.05 | 115.72 → 115.55 | 0.00 → 0.00 |
| fixed-fast-close | 100 | 100.00 → 100.11 | 1.91 → 2.04 | 0.00 → 0.00 |
| fixed-fast-reuse | 100 | 100.11 → 100.12 | 1.69 → 1.84 | 0.00 → 0.00 |
| fixed-steady | 40 | 39.57 → 39.57 | 116.61 → 116.92 | 0.00 → 0.00 |
| fixed-overload | 160 | 73.44 → 73.74 | 325.76 → 191.50 | 52.30 → 53.24 |
| elastic-overload | 160 | 157.95 → 73.34 | 116.34 → 192.11 | 0.00 → 53.44 |
| http-stream-overload | 16 | 7.35 → 7.23 | 1254.78 → 1255.13 | 47.29 → 48.44 |
| ai-client-stream-control | 16 | 14.31 → 14.31 | 1017.16 → 1016.89 | 0.00 → 0.00 |

## Resource peaks

Peaks are sampled every 20 ms and then summarized across repetitions. Idle reusable connections are included in open physical upstream connections. New connections are counted at physical channel initialization, not per HTTP request.

| Case | Open upstream connections before → after | Pending before → after | New connections/s before → after | Heap MiB before → after | Direct MiB before → after | Windows process handles before → after |
| --- | ---: | ---: | ---: | ---: | ---: | ---: |
| direct-slow-control | 64 → 64 | not instrumented | 8.00 → 8.00 | 57.13 → 58.18 | 8.01 → 8.01 | 650 → 650 |
| fixed-fast-close | 1 → 1 | 0 → 0 | 100.00 → 100.12 | 48.10 → 50.86 | 8.01 → 8.01 | 664 → 664 |
| fixed-fast-reuse | 8 → 8 | 0 → 0 | 0.00 → 0.00 | 36.83 → 38.82 | 8.01 → 8.01 | 678 → 678 |
| fixed-steady | 8 → 8 | 0 → 0 | 0.00 → 0.00 | 24.76 → 25.33 | 8.01 → 8.01 | 682 → 682 |
| fixed-overload | 8 → 8 | 16 → 4 | 0.00 → 0.00 | 44.78 → 47.14 | 8.01 → 8.01 | 682 → 682 |
| elastic-overload | 49 → 8 | 0 → 4 | 0.00 → 0.00 | 52.12 → 46.33 | 8.01 → 8.01 | 764 → 682 |
| http-stream-overload | 8 → 8 | 5 → 4 | 0.00 → 0.00 | 20.14 → 21.15 | 8.01 → 8.01 | 682 → 682 |
| ai-client-stream-control | 19 → 19 | not instrumented | 0.00 → 0.00 | 58.21 → 21.11 | 8.01 → 8.01 | 704 → 704 |

## Variability and interpretation

| Case | Successful p99 range before (ms) | Successful p99 range after (ms) |
| --- | ---: | ---: |
| direct-slow-control | 115.52–115.76 | 115.55–115.73 |
| fixed-fast-close | 1.89–2.22 | 1.90–2.51 |
| fixed-fast-reuse | 1.62–1.79 | 1.65–1.93 |
| fixed-steady | 116.59–116.71 | 116.46–117.55 |
| fixed-overload | 323.71–326.72 | 191.25–192.10 |
| elastic-overload | 116.32–116.50 | 188.55–192.86 |
| http-stream-overload | 1253.56–1254.83 | 1250.99–1261.21 |
| ai-client-stream-control | 1016.92–1017.54 | 1016.72–1017.04 |

- Total generator dropped iterations: 0.
- The unchanged FIXED default is not being presented as an unbounded queue: the before measurement exposes its existing 16-slot limit. The shorter candidate queue is an explicit configuration choice.
- ELASTIC shedding under the new 8-connection budget is intentional admission control, not a throughput improvement. Before ELASTIC can complete more requests by consuming more upstream connections.
- The direct control and AI-construction stream control prevent attributing upstream or generator limits to the new HTTP pool. The AI control is not an end-to-end AI transport or model benchmark.
- Pending lifetimes in the raw data measure queue residence, including timeout/cancellation; the production acquisition timer additionally includes connection establishment.
- Heap peaks are sampled live heap, not retained-heap/leak measurements. Direct-buffer peaks and open connections are separate quantities. This short run cannot establish long-term memory stability.
- Windows has no POSIX FD counter here (`peakFileDescriptors: -1`). Native process-handle counts are retained separately. Linux FD validation remains an explicit follow-up; these handle counts must not be represented as FD measurements.
- JFR `profile` recordings for the first fixed-overload repetition of each arm and complete k6 point streams remain in the runner output directories. They are local diagnostic artifacts; the checked-in JSON retains primary latency/queue samples, summaries and resource counters.
- Longer isolated runs, a stated p99/error SLO rate sweep, separate generator/upstream hosts, Linux FD measurement and full gateway/TLS/AI-engine validation remain necessary before deployment sizing or maximum-throughput claims. These are not claimed complete by this local report.
