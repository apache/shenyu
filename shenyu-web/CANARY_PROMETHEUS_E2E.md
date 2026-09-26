# Canary Prometheus end-to-end test

`CanaryPrometheusIT` exercises real HTTP traffic through `ShenyuWebHandler` and the production Global, Metrics, Divide, URI, Netty HTTP Client and Response plugins. Two loopback HTTP backends identify themselves as stable/canary. A separate **real Prometheus process** scrapes ShenYu's metrics endpoint into its TSDB; assertions use its `/api/v1/targets` and `/api/v1/query` APIs. There are no mocked exchanges, routing decisions, upstream caches or Prometheus responses.

The fixture supplies configuration through `CommonPluginDataSubscriber` and `DivideUpstreamDataHandler`. It does not start ShenYu Admin or test registry/configuration transport, the complete Spring Boot deployment, Grafana, production load or long-running behavior.

## Run

Install/download an official native [Prometheus release](https://github.com/prometheus/prometheus/releases) for your OS/architecture and verify the published checksum. The test does not download executables. Prometheus 3.5.0 on macOS arm64 was used for the local verification.

From the repository root, with Java 17 and Maven:

```sh
mvn -pl shenyu-web -am \
  -Dtest=CanaryPrometheusIT \
  -Dprometheus.binary=/absolute/path/to/prometheus \
  -Dsurefire.failIfNoSpecifiedTests=false test
```

All dependencies use test scope. The `IT` suffix keeps the external-process test out of ordinary Surefire unit-test discovery; the command above explicitly selects it. An absent/non-executable binary fails the selected test instead of silently skipping or substituting an HTTP stub. Network policy must allow loopback listeners and child processes. Docker is not required.

## Assertions

- The Prometheus target is `up`, with no scrape error; build information confirms the server version.
- Three successes and one HTTP 502 in each partition reach the correct backend and create distinct success/error counters.
- Two empty-Canary-pool fallbacks reach Stable, count as Stable requests and increment `canary_pool_empty` exactly twice.
- Two requests under the reject policy return 503 without reaching either backend; they count as intended Canary/reject and do not increment fallback.
- A real first-attempt backend delay exceeds the configured timeout. The second attempt succeeds in Canary; two backend attempts produce only one request and one routing-decision observation.
- One legacy request uses `partition="none"` for latency and creates no Canary metric series.
- PromQL checks request totals, error ratios, fallback totals, histogram counts/sums/buckets and partition latency quantiles. Artificial backend delays provide lower bounds in milliseconds; these are functional assertions, not performance benchmarks.
- The exact label sets are checked after stripping Prometheus's `job`, `instance` and metric-name labels. Changing paths, Authorization, Cookie, sticky-key and request-ID headers does not create extra Canary series or expose their values in these labels. Existing non-Canary metrics are outside that label assertion.

## Evidence and cleanup

Each invocation writes `shenyu-web/target/prometheus-e2e/run-*/`:

- `prometheus.yml`, `prometheus.log`, `data/`: actual scrape configuration, process log and TSDB.
- `requests.jsonl`: actual gateway response statuses/bodies.
- `prometheus-api.jsonl`: actual Prometheus API responses, including queries and server build information.
- `backend-hits.json`: backend attempts counted separately from external requests.
- `result.txt`: written only after all assertions pass.

Servers bind only to `127.0.0.1`, using temporary ports. The test stops its Prometheus process, gateway, backends and metrics server after success or failure. Generated evidence remains under ignored `target/` for inspection and is removed by `mvn clean`.
