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
# Minimal HTTP gateway distribution

The opt-in `shenyu-bootstrap-http` assembly addresses [#7649](https://github.com/apache/shenyu/issues/7649).
The default reactor, `shenyu-bootstrap`, existing full archives and their Docker build remain unchanged.
No integration is removed from the main repository by this assembly.

See the [2026-10-15 local comparison](results/minimal-http-2026-10-15/README.md)
for measured results, dependency inventories, raw evidence hashes and remaining validation.

## Runtime contract

The assembly selects the existing gateway starter and the WebSocket sync starter.
It excludes the gateway starter's optional Context Path and Transform starters.
The five retained plugins are required for the HTTP request path:

| Plugin | Included behavior |
| --- | --- |
| Global | Request context construction |
| Divide | Selector/rule matching, upstream selection, load balancing, timeout/retry settings and its existing built-in upstream circuit breaker |
| URI | Construct the outgoing URI from the selected upstream and incoming path/query |
| HTTP Client | Reactor Netty HTTP forwarding |
| Response | Relay upstream status, headers and response body |

Actuator liveness/readiness and the existing gateway health filter are included.
Dedicated authentication, rate limiting, Resilience4j/Sentinel, response transformation,
caching, RPC, AI/MCP and logging plugins are not included. Configure these explicitly
before using the assembly for workloads requiring those governance features.
The assembly has no instance registry; centrally configured HTTP upstreams come from Admin.

Only WebSocket configuration synchronization is packaged. `initial-sync-readiness: true`
requires a compatible Admin supporting the versioned initial snapshot protocol, such as
the Admin built from the same source revision. Readiness becomes healthy after the initial
snapshot is applied; liveness remains available while Admin is unavailable. This is a
startup latch, not a guarantee of continuous Admin connectivity or upstream health.
For an older Admin, explicitly opt out of that flag and use a functional route probe;
an HTTP listener alone does not prove configuration readiness.

## Build and run

Use JDK 17+ and the repository wrapper:

```sh
./mvnw -B -Pminimal-http -pl shenyu-bootstrap-http -am clean install \
  -DskipTests -Dmaven.javadoc.skip=true -Djacoco.skip=true
```

Checkstyle and RAT run at validation. `-DskipTests` skips Java test execution for this
packaging command; the packaged smoke commands below execute the integration checks.
On Windows use `mvnw.cmd`; quote each `-D...` argument in PowerShell.
The reactor can build excluded optional modules needed to build the gateway starter;
they do not enter the minimal runtime dependency closure.

Outputs are `shenyu-bootstrap-http/target/apache-shenyu-http-2.7.2-SNAPSHOT-bin.tar.gz`
and the extracted `target/apache-shenyu-http-2.7.2-SNAPSHOT-bin/apache-shenyu-http/`.
The archive contains `bin/`, `conf/`, `lib/`, `LICENSE`, `NOTICE` and `licenses/`.
After compatible core artifacts are installed or published, the module also packages
independently with `./mvnw -f shenyu-bootstrap-http/pom.xml package`.

```sh
export SHENYU_NAMESPACE=649330b6-c2d7-4edc-be8e-8a54df9eb385
export SHENYU_SYNC_WEBSOCKET_URLS=ws://localhost:9095/websocket
export SHENYU_SYNC_WEBSOCKET_ALLOW_ORIGIN=ws://localhost:9195
export SHENYU_SYNC_WEBSOCKET_TOKEN=your-admin-sync-token
export JAVA_OPTS='-Xms512m -Xmx1024m -XX:MaxDirectMemorySize=256m'
sh bin/start.sh
# Windows: set the same environment variables, then run bin\start.bat.
curl --fail http://localhost:9195/actuator/health/readiness
```

Keep the namespace, WebSocket endpoint, token and origin policy consistent with Admin.
The 512 MiB initial heap avoids triggering the existing 256 MiB free-memory reserve in
the synchronization executor during startup; smaller heaps need separate validation.
The launcher stays in the foreground and accepts Spring Boot arguments, for example
`bin/start.sh --server.port=9295`. Stop it with the normal process/container signal.

To add a capability, declare its existing `shenyu-spring-boot-starter-plugin-*`
dependency at the same core version in the application POM, rebuild, and configure
the plugin in Admin. For Context Path or Transform, remove the corresponding exclusion.
For another sync channel, replace the WebSocket starter and its configuration together.
Re-run inventory and routing checks after changing the selected features. For external
plugin-store integrations use the announced compatible store version and dependency
closure; publication and migration gates stay with #7294/#7306/#7597/#7599.

## Dependency and legal inventory

```sh
./mvnw -B -Pminimal-http -pl shenyu-bootstrap-http dependency:list \
  -DincludeScope=runtime -DoutputFile=target/runtime-dependencies.txt
python script/performance/minimal-http/inventory.py \
  shenyu-bootstrap-http/target/apache-shenyu-http-2.7.2-SNAPSHOT-bin/apache-shenyu-http \
  --minimal --output shenyu-bootstrap-http/target/inventory.json
```

The inventory reads the packaged JARs, recording file size, SHA-256, embedded Maven
coordinates and embedded LICENSE/NOTICE/COPYING entries. `--minimal` enforces the
five-plugin boundary, the single sync channel and absence of registry modules.
The assembly intentionally carries the full distribution's conservative legal-file
superset, including notices for components not selected here. It keeps embedded legal
files in every JAR. This avoids prematurely dropping notices while dependencies change;
the superset is not a claim that all listed components are shipped. The actual JAR
inventory is authoritative. Review `jars_without_embedded_legal_files` against the
inherited top-level legal files for each release. Dependency additions require renewed
license/notice review; the inventory is evidence, not an automatic legal approval.

## Packaged smoke tests

The Python tools are outside the production runtime. `websockets` supplies the protocol
fixture and `psutil` supplies portable process measurements. k6 is an existing external
load generator; it is not a gateway dependency.

```sh
python -m pip install -r script/performance/minimal-http/requirements.txt
python -m unittest discover -s script/performance/minimal-http -p 'test_*.py'
python script/performance/minimal-http/benchmark.py \
  --minimal shenyu-bootstrap-http/target/apache-shenyu-http-2.7.2-SNAPSHOT-bin/apache-shenyu-http \
  --java "$JAVA_HOME/bin/java" --smoke-only --output /tmp/http-smoke
```

This starts a packaged gateway and a loopback WebSocket protocol fixture, withholds
the initial snapshot to verify readiness is 503 while liveness is 200, then tests
initial sync, rule create/delete, selector delete/recreate, upstream changes, HTTP 503,
connection failure and recovery. It does not emulate Admin database or REST behavior.

For the real control-plane path, build the unchanged Admin distribution, extract it,
and run the disposable H2 Admin test. This test only targets the Admin it starts:

```sh
./mvnw -B -Prelease -pl shenyu-dist/shenyu-admin-dist -am package \
  -DskipTests -Dmaven.javadoc.skip=true -Djacoco.skip=true
tar -xzf shenyu-dist/shenyu-admin-dist/target/apache-shenyu-2.7.2-SNAPSHOT-admin-bin.tar.gz \
  -C shenyu-dist/shenyu-admin-dist/target
python script/performance/minimal-http/admin_smoke.py \
  --admin shenyu-dist/shenyu-admin-dist/target/apache-shenyu-2.7.2-SNAPSHOT-admin-bin \
  --minimal shenyu-bootstrap-http/target/apache-shenyu-http-2.7.2-SNAPSHOT-bin/apache-shenyu-http \
  --java "$JAVA_HOME/bin/java" --output /tmp/http-admin-smoke
```

Backend HTTP registration creates a selector/rule/upstream in the disposable Admin.
The test checks real initial sync, successful and failed proxying, then uses authenticated
Admin REST operations to update the rule's path, delete/recreate the rule and delete
the selector. It verifies convergence through gateway traffic after every mutation.
`.github/workflows/minimal-http.yml` packages and runs both smoke paths on JDK 17.

## Same-environment measurement

Build and extract the unchanged full bootstrap distribution from the same checkout:

```sh
./mvnw -B -Prelease -pl shenyu-dist/shenyu-bootstrap-dist -am package \
  -DskipTests -Dmaven.javadoc.skip=true -Djacoco.skip=true
tar -xzf shenyu-dist/shenyu-bootstrap-dist/target/apache-shenyu-2.7.2-SNAPSHOT-bootstrap-bin.tar.gz \
  -C shenyu-dist/shenyu-bootstrap-dist/target
python script/performance/minimal-http/benchmark.py \
  --minimal shenyu-bootstrap-http/target/apache-shenyu-http-2.7.2-SNAPSHOT-bin/apache-shenyu-http \
  --full shenyu-dist/shenyu-bootstrap-dist/target/apache-shenyu-2.7.2-SNAPSHOT-bootstrap-bin \
  --java "$JAVA_HOME/bin/java" --k6 /path/to/k6 \
  --runs 3 --rates 200 500 1000 --duration 30 --warmup 30 --idle 10 \
  --cpu-affinity 2 3 --jfr --output /tmp/http-assembly-comparison
```

Choose CPUs valid for the host, keep unrelated workloads away, and do not run builds
during the comparison. The harness fixes heap/direct-memory/processor settings, effective
HTTP configuration, namespace, five active plugins, logging, worker count and connection
reuse for both classpaths. Redis/Elasticsearch health checks and optional discovery/
heartbeat are disabled for both. It alternates assembly order across repetitions.
Each JVM starts fresh; each startup measurement ends only after initial sync and a
successful proxied request. Idle RSS/CPU and loaded classes are measured after smoke
checks; the latter includes the exercised failure paths. Windows RSS means working set.
CPU is normalized to one logical CPU, where 100% means one fully occupied CPU.

The benchmark uses a 256-byte HTTP/1.1 loopback response with persistent connections.
k6 runs fixed arrival rates with 64 preallocated VUs, separate warmup and measured
phases, and no closed-loop pacing. Direct-upstream controls check generator/backend
capacity at each offered rate. Both CPU/RSS and p50/p95/p99, successful throughput,
body/status errors and dropped iterations are retained. JFR capture starts before
warmup and includes all measured rates equally. Raw k6 samples, summaries, process
commands/counters, gateway logs, JFR files and `report.json` remain in the output folder.
The output directory must be new, preventing accidental evidence replacement.

Use only rates whose direct control passes when comparing gateway capacity. A point
passes the default limits when p99 is below 100 ms, response errors are below 0.1%, and
no iterations are dropped. Increase `--rates` until a failure brackets sustainable
capacity; if every rate passes, report the highest tested sustainable rate, not a
measured maximum. Compare equal successful throughput separately from capacity limits.
Report repetition ranges and variability; a smaller image does not establish higher QPS.
The co-located Python upstream/generator makes this a local comparison, not a production
capacity claim. Use dedicated hosts/cgroups and a non-bottleneck upstream for release results.

The harness labels first-observed and subsequent warm-filesystem launches honestly.
It does not flush the operating system's file cache. True cold-cache readiness requires
a separate controlled host/VM run with the cache-reset method recorded, before launching
either artifact; an arbitrary first process or fresh container is not proof of cold cache.

For image-size comparisons, use the same digest-pinned runtime and build recipe for
both extracted distributions (BuildKit named contexts):

```sh
docker build -f script/performance/minimal-http/Dockerfile \
  --build-context distribution=shenyu-bootstrap-http/target/apache-shenyu-http-2.7.2-SNAPSHOT-bin/apache-shenyu-http \
  --build-arg JAVA_IMAGE="$PINNED_JAVA_IMAGE" \
  --build-arg MAIN_CLASS=org.apache.shenyu.bootstrap.http.ShenyuHttpApplication \
  -t shenyu-http-comparison:minimal .
docker build -f script/performance/minimal-http/Dockerfile \
  --build-context distribution=shenyu-dist/shenyu-bootstrap-dist/target/apache-shenyu-2.7.2-SNAPSHOT-bootstrap-bin \
  --build-arg JAVA_IMAGE="$PINNED_JAVA_IMAGE" \
  --build-arg MAIN_CLASS=org.apache.shenyu.bootstrap.ShenyuBootstrapApplication \
  -t shenyu-http-comparison:full .
docker image inspect shenyu-http-comparison:minimal shenyu-http-comparison:full \
  --format '{{.Id}} {{.Size}}'
```

Record image IDs, base digest and uncompressed image bytes separately from compressed
archive bytes. `shenyu-bootstrap-http/Dockerfile` also builds the standalone minimal image.
On a machine without a working Docker daemon, image measurements stay explicitly pending.
