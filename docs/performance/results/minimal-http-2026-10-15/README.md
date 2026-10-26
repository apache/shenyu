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

# Local HTTP assembly comparison — 2026-10-15

Historical measurements on baseline `1d599d90`; these figures have not been re-measured on the current PR head.

This is a same-host comparison for #7649, not a production capacity claim. Core baseline: `1d599d90e82e3415720b50885823eadc7b93d431` plus the minimal assembly in this change. Archive hashes and per-run measurements are in [report.json](report.json); exact packaged JAR hashes/coordinates and legal entries are in [minimal-jars.csv](minimal-jars.csv) and [full-jars.csv](full-jars.csv).

Windows 11 build 26200; Ryzen 9 8945HX; 32 logical CPUs; 31.8 GiB RAM; Temurin 21.0.10+7; k6 1.0.0. Each gateway was restricted to CPU affinity 8/9 and `-XX:ActiveProcessorCount=2 -Xms512m -Xmx1024m -XX:MaxDirectMemorySize=256m`. There was no cgroup CPU quota. Generator, Python upstream and gateway shared the host. The Java sources were compiled for Java 17; these local measurements used JDK 21.

Both used identical HTTP/WebSocket configuration, namespace, logging, two Netty server workers and HTTP connection reuse. Only Divide was configured in the synchronized plugin data; the Global/URI/HTTP Client/Response infrastructure plugins also ran. Other full-distribution plugins had no matching configuration. One selector/rule forwarded a 256-byte HTTP/1.1 response. Order alternated across three fresh JVM repetitions. Each repetition had eight routing/failure/recovery checks, 5 seconds of idle sampling, 20 seconds of warmup and 20 seconds at each offered rate. JFR profiling was enabled equally for both.

## Package and startup/resource observations

Values in parentheses are min–max across three repetitions. RSS is Windows working set; CPU is percentage of one logical CPU. Classes/RSS/idle CPU were measured after smoke checks and include exercised failure paths.

| Metric | Minimal | Full |
| --- | ---: | ---: |
| Packaged JAR count | 108 | 384 |
| Packaged JARs (MiB) | 49.09 | 141.63 |
| Compressed archive (MiB) | 43.74 | 128.45 |
| Sync + proxy readiness (s) | 3.906 (3.890–3.922) | 5.531 (5.515–6.063) |
| Idle RSS (MiB) | 250.3 (244.5–255.8) | 363.8 (363.7–364.2) |
| Loaded classes | 9372 (9364–9373) | 12142 (12138–12146) |
| Idle CPU (%) | 0.31 (0.00–1.87) | 0.31 (0.31–0.62) |

The first-observed launch and subsequent warm-filesystem launches are recorded separately in the raw report. These are **not controlled OS-cold-cache measurements**. The small idle CPU values fluctuate; no idle CPU improvement is claimed.

## Fixed-arrival-rate traffic

All direct-upstream controls passed the same SLA, with no response errors or dropped iterations. Limits were p99 < 100 ms, error rate < 0.1%, and zero dropped iterations. Throughput counts only HTTP 200 responses with the exact expected body.

| Offered RPS | Assembly | Successful RPS | p50 ms | p95 ms | p99 ms | CPU % | Peak sampled RSS MiB | Errors / dropped |
| ---: | --- | ---: | ---: | ---: | ---: | ---: | ---: | ---: |
| 200 | minimal | 200.00 (200.00–200.05) | 0.59 (0.58–0.59) | 1.00 (0.96–1.02) | 1.29 (1.26–2.88) | 24.19 (23.91–25.91) | 418.9 (407.8–461.3) | 0 / 0 |
| 200 | full | 200.05 (200.00–200.05) | 0.60 (0.60–0.60) | 1.03 (1.03–1.06) | 1.40 (1.38–1.61) | 29.15 (28.30–30.04) | 616.3 (613.3–623.3) | 0 / 0 |
| 500 | minimal | 500.05 (500.00–500.05) | 0.50 (0.50–0.50) | 0.68 (0.68–0.68) | 1.01 (0.94–1.02) | 32.90 (32.45–33.74) | 561.4 (560.1–569.0) | 0 / 0 |
| 500 | full | 500.00 (500.00–500.05) | 0.50 (0.50–0.50) | 0.69 (0.68–0.69) | 1.02 (0.99–1.13) | 33.82 (31.67–34.67) | 620.9 (619.0–621.6) | 0 / 0 |
| 1000 | minimal | 1000.05 (1000.05–1000.05) | 0.00 (0.00–0.00) | 0.52 (0.52–0.52) | 0.67 (0.67–0.69) | 33.08 (32.28–35.36) | 561.4 (561.0–561.8) | 0 / 0 |
| 1000 | full | 1000.05 (1000.05–1000.05) | 0.00 (0.00–0.00) | 0.53 (0.52–0.53) | 0.71 (0.70–0.74) | 34.31 (34.08–36.34) | 619.3 (616.5–621.0) | 0 / 0 |

k6 recorded 0.00 ms for some medians on this Windows host; treat those values as a measurement limitation, not literal zero latency.

Both assemblies sustained the highest tested rate of 1000 RPS. Saturation was not reached, so this does not measure either assembly's maximum throughput. Some latency ranges overlap; no general latency or QPS improvement is established. The observed benefit is package size, readiness time and working set for this workload/environment.

## Validation and remaining release evidence

- The 35-module minimal reactor and 163-module full/Admin packaging reactor passed, including test compilation, Checkstyle and RAT. Java test execution was skipped for these packaging builds.
- Independent `-f shenyu-bootstrap-http/pom.xml package` passed.
- Five Python harness checks and five CI module-routing cases passed locally.
- Both packaged assemblies passed the WebSocket protocol smoke path, including incomplete-snapshot readiness.
- The real disposable H2 Admin passed six authenticated route registration/update/delete/recreate checks; see [admin-smoke.json](admin-smoke.json).
- The resolved runtime list is in [runtime-dependencies.txt](runtime-dependencies.txt); packaged coordinates, legal-file entries and SHA-256 values are in the two JAR inventory CSV files. The minimal archive intentionally retains the full distribution's legal-file superset.
- Docker Desktop failed to provide a daemon after a launch attempt. Image byte sizes are unmeasured; the matched-base-image build recipe is in [the guide](../../minimal-http.md).
- True OS-cold-cache readiness, a saturation-bracketing sweep, container quotas and an independent JDK 17 CI run remain release-validation follow-ups. Keep #7649 open until its acceptance evidence is complete.

## Retained evidence

[report.json](report.json) retains the per-repetition aggregate measurements; the CSV files retain both complete JAR inventories. The original k6 samples, gateway logs, JVM counter dumps and JFR recordings are not published, so this directory does not provide independently auditable raw performance evidence. Reproduce with the guide's command, using `--duration 20 --warmup 20 --idle 5 --cpu-affinity 8 9 --jfr` to match this historical run. New runs retain their raw files in their output directory.
