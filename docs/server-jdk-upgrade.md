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

# Main repository JDK upgrade proposal

Design deliverable for [#6368](https://github.com/apache/shenyu/issues/6368).
Status: proposed migration plan; no compiler target, supported-runtime policy,
dependency, image, or CI configuration is changed by this document.
Source inventory: `2.7.2-SNAPSHOT`, commit `87c22dde7`, reviewed on 2026-10-21.
Future implementation PRs must refresh the inventory and attach their own results.

## Decision and compatibility boundary

Adopt Java 21 as the next server baseline through separate runtime-validation
and bytecode-baseline stages. First run Java 17 artifacts on Java 21 with the
existing thread model. Keep virtual threads opt-in and assess them through the
[virtual thread design](virtual-thread-design.md); they are not an upgrade gate
for every plugin.

The [#7602 client migration](../shenyu-register-center/CLIENT_MIGRATION.md) targets
Java 17 / Spring Boot 3 with Java 21 verification. The historical
[split-client POM](https://github.com/apache/shenyu-client-java/blob/8b93088973bf9f8f50d614e708bb87f662b707d5/pom.xml)
declares Java 8; it is a legacy wire-compatibility fixture, not the new SDK target.
This separate server Java 21 proposal must not silently raise the Java 17 output
floor of foundations consumed by the migration client.
Preserve the [registration v1 wire contract](../shenyu-register-center/shenyu-register-protocol/README.md)
and test released clients against the candidate Admin. Sharing JSON fields and
endpoint semantics does not require sharing server bytecode.

Java 21 is a concrete migration target, not a claim that it is the newest JDK.
Java 23 and 25 can be separate compatibility lanes; they do not become supported
through a Java 21 test result. A Java 25 proposal must revalidate the selected
framework, bytecode tools, drivers, and extensions before changing policy.

## Current constraints

| Area | Evidence in the inspected source | Consequence |
| --- | --- | --- |
| Main compilation | [root POM](../pom.xml): Java 17, compiler plugin 3.7.0, explicit `source`/`target`; no repository toolchain configuration found | Building with a newer JDK does not currently restrict API usage to Java 17. Introduce `release=17` before claiming that newer-JDK builds retain Java 17 compatibility |
| Build entry point | [wrapper configuration](../.mvn/wrapper/maven-wrapper.properties): Maven 3.6.3; root Maven Enforcer requires Maven 3.6.3+ and Java 17+ | Use `./mvnw` or `mvnw.cmd`; preserve the implemented validation when changing the baseline |
| Framework and tooling | Root POM: Spring Boot 3.3.1, Surefire 3.0.0-M4, JaCoCo 0.8.12, Byte Buddy 1.14.11 | Treat resolved dependencies and agent/test support as migration inputs; a source-level change alone is insufficient |
| Server CI | [ci.yml](../.github/workflows/ci.yml) uses Java 17 for normal builds/tests; `compat_java21` runs on upstream push with code changes | A configured compatibility job is not passing evidence, and it currently does not gate a PR before merge |
| Independent E2E reactor | [E2E POM](../shenyu-e2e/pom.xml) has its own Apache parent and Java 17 property | Updating the main property will not update this reactor |
| Independent examples reactor | [examples POM](../shenyu-examples/pom.xml) has a Spring Boot 3.3.1 parent and compiler configuration | Audit it independently; the `example` profile does not make it inherit the main parent |
| Lower-target declarations | Examples `shenyu-examples-common`, `shenyu-examples-springmvc`, and `shenyu-examples-springmvc-tomcat` declare Java 8 compiler properties; client beat starter declares 17 | Inspect effective POMs and dependency bytecode. These properties are not proof that Boot 3 examples run on Java 8 |
| Distribution images | [Admin Dockerfile](../shenyu-dist/shenyu-admin-dist/docker/Dockerfile): Corretto 17 on Alpine; [Bootstrap Dockerfile](../shenyu-dist/shenyu-bootstrap-dist/docker/Dockerfile): Temurin 17 on CentOS 7 | Both runtime and base-OS changes require explicit image testing. Do not infer runtime JDK from the CI build JDK |
| Other runtime images | Dockerfiles under `shenyu-examples` and `shenyu-integrated-test` also select Java 17 | Test backend containers and gateway containers are independent; upgrade each intentionally |

The server family includes Admin, Bootstrap, WebFlux runtime, gateway plugins,
data sync, registry implementations, distribution launchers, and Kubernetes
controller. No inspected server module has an established Java 8 support
obligation. Existing Java 8-looking example settings are an inconsistency to
resolve, not a reason to retarget the server dependency graph to Java 8.

## Module migration units

| Unit | Proposed handling | Gate before Java 21 bytecode |
| --- | --- | --- |
| `shenyu-admin`, Admin listeners and server registration receivers | First Java 21 runtime canaries, then server baseline | MVC/MyBatis, all claimed DB drivers, authenticated registration, import, config publication, cluster and shutdown tests |
| `shenyu-bootstrap`, `shenyu-web`, gateway starters and server plugins | Upgrade as a compatible server set | WebFlux routing/streaming, protocol plugins, extension loading, cancellation, TLS, and packaged launcher tests |
| Sync, registry, infrastructure, Disruptor and load balancer modules | Follow the server set; retain ordering and lifecycle contracts | Actual registry transports, reconnect/failover, config updates, node recovery and shutdown |
| `shenyu-common`, `shenyu-spi`, plugin APIs and `shenyu-register-protocol` | Keep Java 17 output during staging; inventory external consumers before raising their floor | No retained lower-JDK consumer may depend on Java 21 classes. Publish and review the public plugin/API compatibility change |
| In-tree business `shenyu-client`, API/HTTP registration senders and business starters | Follow #7602's Java 17 / Boot 3 migration before changing their foundation floor; beat and its starter stay in main | No hidden parent or dependency forces the Java 17 migration client to consume Java 21 classes; do not preserve copied Java 8 infrastructure |
| `shenyu-sdk` and SDK starters | Separate consumer-JDK decision; they are outside the registration-client split | Inventory applications and transitive dependencies. Preserve an explicit Java 17 build if promised; otherwise announce the Java 21 requirement |
| Examples and integration/E2E engines | Upgrade server fixtures with the server; run migration-client and any supported historical-client fixtures in client-owned/external workflows | Separate reactors/classpaths compile and start; old-client/new-server verification never adds an external client dependency to main |

A parent-property override cannot preserve a lower-JDK library if that library
depends on Java 21 server classes. Split such dependencies, keep the necessary
shared artifacts at Java 17, or announce the consumer baseline increase.
Do not solve the registration-client boundary with duplicate server source.

## Build and dependency preparation

In an independent preparation PR, replace the root compiler's `source` and
`target` entries with `release` while preserving the value 17. Preserve
`-parameters`, encoding, generated-source configuration and annotation processing.
The [Maven compiler documentation](https://maven.apache.org/plugins/maven-compiler-plugin/examples/set-compiler-release.html)
explains that `--release` also checks the chosen Java SE API surface.
This protects JDK APIs, not the Java baseline of third-party dependencies.

Choose and pin compatible compiler/Surefire/bytecode-analysis versions based on
a clean Java 17/21 build, separately from the eventual target increase. Preserve
the existing Maven Enforcer rules for wrapper-compatible Maven 3.6.3+ and update
the minimum JDK only at the announced baseline stage. If toolchains are introduced, configure compiler and test JVM selection
explicitly and record `java -version`, `./mvnw -v`, and the forked test JDK.
Selecting a compiler toolchain alone does not prove tests run on that JDK.

| Dependency or feature | Risk to verify | Evidence required |
| --- | --- | --- |
| Spring Boot 3.3.1, Spring Framework and Spring Cloud | Exact resolved version support; servlet/reactive adapters and auto-configuration | Effective POM/dependency tree plus startup, routing and config-binding results on the candidate JDK. Do not substitute a newer Boot maintenance page for 3.3.1 evidence |
| Netty 4.1.139.Final, Reactor from the Boot BOM, gRPC 1.65.1 | Native transport, TLS/ALPN, DNS and event-loop behavior | HTTP/HTTPS, gRPC streaming/cancellation, architecture-specific image tests, and reactor version recorded from the resolved tree |
| Dubbo 3.2.14 and other RPC adapters | Reflection/proxy generation, serialization, context propagation and third-party worker assumptions | Provider calls, errors, concurrency and extension loading; preserve explicit attachment cleanup |
| Byte Buddy 1.14.11, Mockito and JaCoCo 0.8.12 | Instrumentation/attach, generated bytecode and coverage execution | Agent-enabled tests and the memory-limited queue option in `ShenyuThreadPoolConfiguration`; test both with and without attach permissions |
| Chicory 1.7.3 | Interpreter/compiler paths and host calls | WASM fixture, concurrent instance-pool, traps, callbacks and gateway tests |
| JDBC drivers, MyBatis, Gson, protobuf and registry SDKs | Reflection, generated code, native binaries and access restrictions | Each supported DB/registry matrix and generated-code compilation. Resolve failures individually rather than adding broad `--add-opens` flags |
| External plugins | API linkage and dependency conflicts after a target change | Load an old plugin on the new server and a rebuilt plugin; document that Java 21 plugin classes cannot load on a Java 17 server |

## Phases and release gates

| Phase | Change | Exit evidence / owner area |
| --- | --- | --- |
| 0: inventory and boundary | Record effective compiler/dependency graphs, API consumers, distribution JDKs and current failures; align with [#6364](https://github.com/apache/shenyu/issues/6364) and [#6370](https://github.com/apache/shenyu/issues/6370) | Build/release and client maintainers review the artifact boundary; failures have named follow-up issues |
| 1: Java 17 artifacts on Java 21 | Add a PR-gating Java 21 job, enforce `release=17`, keep Java 17 primary and virtual threads disabled | Same source passes clean compilation/unit tests on 17 and 21; Java 17-built distribution starts and passes deployment tests on both |
| 2: Java 21 runtime canary | Publish an explicitly identified Java 21 image variant; retain the Java 17 image and configuration | Real DB/registry and HTTP/RPC/streaming tests, resource measurements, restart/rollback drill and platform image checks pass |
| 3: announced server baseline | In a separately reviewed release, raise the selected server artifact set to `release=21`, update minimum-JDK checks, normal CI, images and release notes together | Client/SDK boundary gates satisfied, required tests green, release manager and affected maintainers approve support policy |
| 4: optional execution changes | Introduce narrowly scoped virtual-thread adapters and benchmarks | Gates in [#6369's design](virtual-thread-design.md), with platform execution still selectable on Java 21 |

Every phase can ship independently. Phase 1 is the minimal first implementation:
it adds verification and API-target protection without requiring a new client JDK
or a server bytecode-baseline change. Do not backport a Java 21 bytecode target
into an existing Java 17 compatibility line.

## Images, launchers and CI

Update both distribution Dockerfiles, their build/publish workflows, and each
claimed architecture together. Select an available maintained OS/JDK image and
pin its tested version/digest in implementation; do not mechanically replace
`17` with `21` in a vendor tag that may not exist. Preserve non-root behavior,
certificates, timezone handling, health checks, mounts and signal delivery.

Audit `start.sh`, `start.bat`, and Docker `entrypoint.sh` for both distributions.
They contain JDK-specific branches, heap/stack defaults and `ADMIN_JVM`/`BOOT_JVM`
overrides. Test defaults and operator-supplied options, startup rejection below
the supported JDK, Windows launchers, graceful stop and restart. A legacy JDK 8
GC branch in a launcher does not establish JDK 8 server support.

Required workflow inventory:

- `ci.yml`: normal build, release local-repository artifact, unit-test groups,
  `compat_java21`, Checkstyle and RAT. Namespace caches/artifacts by build JDK and
  target; never reuse `target/` across compiler-baseline comparisons.
- `docker-publish.yml` and `docker-publish-dockerhub.yml`: build JDK and actual
  Admin/Bootstrap runtime-image JDK.
- `integrated-test.yml`, both `integrated-test-k8s-*` workflows,
  `e2e-k8s.yml`, and `k8s-examples-http.yml`: server, example and runner images
  and startup scripts, including rolling updates.
- `codeql-analysis.yml`, `wasm-fixtures.yml` and `native-sql-matrix.yml`:
  analysis/build runtime; preserve their specialized verification.

Retain the Java 17 required lane until Phase 3. Then require Java 21 for the
new server line, while testing the previous Java 17 release for migration and
rollback. Higher-JDK exploratory lanes do not replace baseline tests.

## Reproducible validation and promotion

These are required future validation commands, not results reported by this
design-only change. Run in fresh checkouts on each claimed JDK; use
`mvnw.cmd` on Windows.

```bash
java -version
./mvnw -v
./mvnw -B -ntp -N help:effective-pom -Doutput=target/effective-pom.xml
./mvnw -B -ntp clean install -Dmaven.javadoc.skip=true
./mvnw -B -ntp -Prelease clean package -DskipTests -Dmaven.javadoc.skip=true
./mvnw -B -ntp -f shenyu-examples/pom.xml -Pexample clean install -DskipTests
./mvnw -B -ntp -f shenyu-e2e/pom.xml clean test-compile
```

Do not equate `-DskipTests` packaging or E2E test compilation with deployment
verification. Run the existing Docker/Kubernetes suites for the support claims.
At minimum exercise authenticated client registration and heartbeat/offline,
persisted routes, Admin-to-Bootstrap updates/deletions, a proxied request,
streaming and cancellation, upstream recovery, registry failover and shutdown.
Include every claimed DB dialect, RPC protocol and sync channel before release.

For Phase 1, inspect packaged class files (major version 61 for Java 17) and
test the packaged distribution under both JDKs. At Phase 3, expect major 65
for Java 21 artifacts and update the minimum-runtime diagnostics. Archive source
SHA, exact JDK/vendor, OS/architecture, image digest, effective POM, dependency
tree, unit/integration reports, startup logs and benchmark results. No passing
full-build, deployment or performance evidence is claimed by this document.

## Rollback and ownership

During Phases 1 and 2, roll back the image/JVM and matching launch options to the
tested Java 17 combination; artifacts remain Java 17 compatible. Rehearse this
against the same persisted configuration before rollout.

After Phase 3, Java 21 class files cannot be made Java 17 compatible by setting
a flag. Restore the previous complete Java 17 distribution/image and its plugin
set. Preserve DB/config wire compatibility, keep unrelated schema migrations out
of the JDK change, and test that old Admin/gateways can consume any intervening
configuration. If that cannot be shown, block promotion until a migration and
rollback procedure is agreed.

Build/release maintainers own compiler, CI, images and support announcements;
Admin/runtime maintainers own server behavior; plugin and registry maintainers
own their integration evidence; client maintainers own Java 17 migration tests
and any explicitly supported historical Java 8 fixtures. Changes to shared DTO
semantics or public plugin APIs require both
the receiving server area and the relevant client/plugin reviewers. Update the
compatibility matrix and release checklist with verified versions when each
phase is implemented.
