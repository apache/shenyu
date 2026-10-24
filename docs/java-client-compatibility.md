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

# Java client and server compatibility

Migration status checked: 2026-10-24. Historical client CI run 35421426447 and
server CI run 38836735672 still refer only to the pinned commits below. Candidate
results reported in shenyu-client-java#16 are separate from passing CI and released
compatibility evidence; this date does not certify newer commits or deployments.

This matrix describes business application registration through [shenyu-client-java](https://github.com/apache/shenyu-client-java) to ShenYu Admin, and the JDK requirements of Admin and bootstrap. It addresses [issue #6370](https://github.com/apache/shenyu/issues/6370).

## Migration target and current implementations

The [#7602 migration](https://github.com/apache/shenyu/issues/7602) targets Java 17 / Spring Boot 3, with Java 21 verification following main's policy. Java 8 / Boot 2 describes the historical external default branch, not the baseline for migration contributions. Legacy headerless v1 compatibility remains a receiver obligation independently of the new SDK's JDK requirements. See the [ownership boundary](java-client-repository-boundary.md#inventory-and-migration-baseline) and [migration/release guide](../shenyu-register-center/CLIENT_MIGRATION.md).

| Implementation | Exact source and baseline | Verification and release status at this check |
| --- | --- | --- |
| Historical external `main` | `8b93088973bf`, `2.7.0.1-jdk8-SNAPSHOT`, Java 8 / Boot 2.7.18 | Historical CI below; no released client/server pair certified here |
| Current main reactor | `86e932cdf`, `2.7.2-SNAPSHOT`, Java 17 / Boot 3.3.1 | Main prerequisites merged in [#7605](https://github.com/apache/shenyu/pull/7605); business clients remain staged in-tree |
| External preparation candidate | [`a37668298ede`](https://github.com/apache/shenyu-client-java/tree/a37668298edef5c501e8b56a0595ed50ef895656), `2.7.2-migration-SNAPSHOT`, Java 17 / Boot 3.3.1 | [Draft PR #16](https://github.com/apache/shenyu-client-java/pull/16) reports 285 reactor tests on each of Java 17/21 and source-runtime HTTP/ZooKeeper/gRPC/MCP checks. These are reported local results, not independently rerun here or passing hosted CI; no checks were attached to this head |

Candidate #16 builds pinned main foundations for preparation, using a separate `shenyu.foundation.version=2.7.2-SNAPSHOT`. The [candidate guide](https://github.com/apache/shenyu-client-java/blob/a37668298edef5c501e8b56a0595ed50ef895656/docs/MIGRATION.md) keeps deployment disabled until released foundations, ownership transfer and released Admin/bootstrap matrices are ready. Neither a SNAPSHOT coordinate nor a closed documentation task is a published support guarantee.

## Version dimensions and support levels

Record these dimensions separately when selecting or verifying a deployment:

| Dimension | What to record |
| --- | --- |
| Client | Exact Maven artifact versions and source tag or commit from `shenyu-client-java`, client starter, application framework version, and registration transport. Keep artifacts from the same client release together. |
| Admin | Exact main repository release or commit, enabled registration endpoints, authentication settings, and registry implementation. |
| Bootstrap | Exact main repository release or commit, gateway plugins, and Admin-to-gateway sync channel. Keep Admin and bootstrap on the same release by default; mixed server versions require separate sync and plugin verification. |
| JDK | Build JDK and runtime JDK for the application, Admin, and bootstrap independently, including vendor and patch version when recording test results. |
| Protocol | Registration wire format and capabilities, independently of the client artifact version. Non-HTTP registration also requires registry-specific verification. |

The client runs in the business application's JVM, separately from Admin and bootstrap. A historical Java 8 client can be tested against a newer Admin through the wire contract; the Java 17 / Boot 3 migration candidate requires an application JVM and framework meeting that new baseline. Client and main versions need not match. Main-owned production code, tests, bootstrap and retained examples must never depend on independent client artifacts; the client consumes released main-owned foundations.

Support and test evidence have different meanings:

| Level | Meaning |
| --- | --- |
| **Supported** | A declared JDK baseline, or an exact released client/Admin/bootstrap combination with recorded successful release verification. JDK support alone does not certify a client/server pair. |
| **Best effort** | A plausible combination above the JDK baseline or within the shared registration contract, without complete verification for that combination. Verify it before deployment; optional features can require a newer server. |
| **Unsupported** | A JDK below the component's baseline, an incompatible application framework or dependency, or a required protocol/capability absent from the receiver. Upgrade or select compatible artifacts. |

The **runtime baseline** is the minimum declared Java level. The effective minimum also includes the selected starter, framework, plugins, and transitive dependencies. A compiler `source`/`target` setting alone does not prove that the resolved application can run on that JDK.

**Verified** means a recorded successful test, with its scope stated. **Configured CI** means a workflow schedules tests; it does not establish that a run passed. Unit tests and controller fixtures do not establish full application startup, database persistence, registry behavior, or gateway routing. A release row must link passing results before claiming those scopes.

## Version compatibility matrix

The initial evidence snapshot uses split-client commit [`8b93088973bf`](https://github.com/apache/shenyu-client-java/tree/8b93088973bf9f8f50d614e708bb87f662b707d5) and main repository `2.7.2-SNAPSHOT` at [`297ac6f84985`](https://github.com/apache/shenyu/tree/297ac6f849857bd0c2ad368709013e98494b0330), checked on 2026-10-16. Historical source tags are listed explicitly. Snapshot coordinates are mutable: pin a commit when reproducing results.

| `shenyu-client-java` version / source | Main repo Admin / bootstrap version | Client JDK baseline | Client JDK verified versions / scope | Admin / bootstrap JDK baseline | Registration protocol compatibility | Status / notes |
| --- | --- | --- | --- | --- | --- | --- |
| Tag `v2.7.0.1-jdk8`; POM declares `2.7.0.1-jdk8-SNAPSHOT` | `2.7.0.1` / `2.7.0.1` | 8 | No passing run recorded here; tag configures CI on 8, 11, 17 | 17 | Legacy HTTP registration; exact pair not verified here | **Best effort**. A source tag alone does not establish a published non-SNAPSHOT artifact. |
| `2.7.0.1-jdk8-SNAPSHOT` at `8b93088973bf` | `2.7.0.1` / `2.7.0.1` | 8 | 8, 17, 21, 23, 25: passing client reactor CI on 2026-09-19; see evidence below | 17 | Common legacy HTTP endpoints only; verify required capabilities on this Admin | **Best effort**. New client features are not implied by an older server version. |
| `2.7.0.1-jdk8-SNAPSHOT` at `8b93088973bf` | `2.7.1` / `2.7.1` | 8 | 8, 17, 21, 23, 25: passing client reactor CI on 2026-09-19 | 17 | Common legacy HTTP endpoints; exact pair not verified here | **Best effort**. Client-only tests do not certify this server pair. |
| `2.7.0.1-jdk8-SNAPSHOT` at `8b93088973bf` | `2.7.2-SNAPSHOT` / `2.7.2-SNAPSHOT` | 8 | 8, 17, 21, 23, 25: passing client reactor CI on 2026-09-19 | 17 | Admin HTTP v1 controller fixtures pass on JDK 17, including headerless requests; split-client end-to-end verification remains pending | **Best effort**, development combination. Main snapshot CI is not fully passing; verify heartbeat, discovery, MCP, persistence, sync, and routing before promotion. |

No split-client/server pair above is advertised as fully verified or supported for production solely from these sources. These historical rows do not prescribe the migration baseline. Combinations not listed are best effort only when their JDKs, frameworks and required wire contracts are compatible; otherwise they are unsupported. Add an exact release row with passing evidence to promote a pair to **Supported**. Older in-tree clients use their own release's POM and documentation.

Evidence for the version rows:

- Tagged split-client [POM](https://github.com/apache/shenyu-client-java/blob/22a3cca759b3592dc4a8c9fb45c144ec79d43146/pom.xml) and [CI](https://github.com/apache/shenyu-client-java/blob/22a3cca759b3592dc4a8c9fb45c144ec79d43146/.github/workflows/ci.yml): Java 8, Spring Boot 2.7.18, and the original CI matrix. Do not transfer later test results to this tag.
- Historical split-client [POM](https://github.com/apache/shenyu-client-java/blob/8b93088973bf9f8f50d614e708bb87f662b707d5/pom.xml) and [CI](https://github.com/apache/shenyu-client-java/blob/8b93088973bf9f8f50d614e708bb87f662b707d5/.github/workflows/ci.yml): Java 8 and Spring Boot 2.7.18. [CI run 35421426447](https://github.com/apache/shenyu-client-java/actions/runs/35421426447) passed all five JDK reactor jobs on 2026-09-19 for this exact commit. The [migration verification record](https://github.com/apache/shenyu-client-java/blob/8b93088973bf9f8f50d614e708bb87f662b707d5/progress.md) also reports local successes and leaves live beat/discovery/MCP checks pending. Client reactor CI does not close those deployment checks.
- Main release POMs for [2.7.0.1](https://github.com/apache/shenyu/blob/v2.7.0.1/pom.xml) and [2.7.1](https://github.com/apache/shenyu/blob/v2.7.1/pom.xml): Java 17 and Spring Boot 3.3.1. Their CI configurations are [2.7.0.1 CI](https://github.com/apache/shenyu/blob/v2.7.0.1/.github/workflows/ci.yml) and [2.7.1 CI](https://github.com/apache/shenyu/blob/v2.7.1/.github/workflows/ci.yml); configured jobs are not release verification evidence.
- Main snapshot [POM](https://github.com/apache/shenyu/blob/297ac6f849857bd0c2ad368709013e98494b0330/pom.xml), [CI](https://github.com/apache/shenyu/blob/297ac6f849857bd0c2ad368709013e98494b0330/.github/workflows/ci.yml), and [registration compatibility tests](https://github.com/apache/shenyu/blob/297ac6f849857bd0c2ad368709013e98494b0330/shenyu-admin/src/test/java/org/apache/shenyu/admin/controller/RegistrationProtocolCompatibilityTest.java). For `297ac6f84985`, the [JDK 17 registration fixture job](https://github.com/apache/shenyu/actions/runs/38836735672/job/116767753175) passed on 2026-10-16. The [JDK 17 admin-sync group](https://github.com/apache/shenyu/actions/runs/38836735672/job/116769383235) and [Java 21 compatibility job](https://github.com/apache/shenyu/actions/runs/38836735672/job/116769383256) failed in that run. Investigate the failed checks before promoting that snapshot; these results do not establish the status of newer main commits. These jobs do not launch the independent client against a complete Admin/bootstrap deployment.

## JDK support policy

For #7602, Java 17 / Boot 3 is the target baseline for both repos; verify Java 21 using main's compatibility-lane policy. Java 8/11 are below the migration candidate's baseline. Java 23/25 require separately declared scope and evidence. Future server JDK proposals do not automatically change the client or shared-foundation baseline.

The table below preserves the **historical Java 8 line** and main `297ac6f84985` evidence. It is not the candidate's support table. Build with the repository's Maven wrapper and a JDK meeting the selected implementation's baseline; verify the packaged application on its intended runtime separately.

| JDK | Split client runtime policy | Client test evidence | Main Admin / bootstrap runtime policy | Main test evidence |
| --- | --- | --- | --- | --- |
| 8 | **Supported baseline** for the JDK 8 client line and compatible application dependencies | Passing client reactor CI on 2026-09-19 | **Unsupported**, below Java 17 baseline | No current Java 8 lane |
| 11 | **Best effort**, above client baseline | Tagged source configures CI; no current passing result recorded here | **Unsupported**, below Java 17 baseline | No current Java 11 lane |
| 17 | **Supported** client JDK target, subject to application framework compatibility | Passing client reactor CI on 2026-09-19 | **Supported baseline** | Registration fixtures pass; admin-sync group fails in the recorded snapshot run |
| 21 | **Supported** client JDK target, subject to application framework compatibility | Passing client reactor CI on 2026-09-19 | **Best effort** runtime beyond the baseline until passing checks and deployment smoke tests are recorded | `compat_java21` fails in the recorded snapshot run; no full runtime result recorded here |
| 23 | **Best effort**; test success does not extend every framework's runtime support | Passing client reactor CI on 2026-09-19 | **Best effort**, unverified; check framework and plugin requirements | No current Java 23 lane |
| 25 | **Best effort**; test success does not extend every framework's runtime support | Passing client reactor CI on 2026-09-19 | **Best effort**, unverified; check framework and plugin requirements | No current Java 25 lane |

The historical Boot 2.7 implementation does not certify Boot 3 applications merely by running on JDK 17. Select a compatible starter and framework together. New migration work follows #7602's Java 17 / Boot 3 target; do not preserve the historical copied implementation as a second target. Likewise, lowering the server compiler target cannot make Boot 3 run on JDK 8.

## Registration and version skew

For the current main source, the [HTTP v1 contract](../shenyu-register-center/shenyu-register-protocol/src/main/java/org/apache/shenyu/register/common/protocol/RegistrationProtocol.java) is independent of Maven release numbers. Its [Admin interceptor](../shenyu-admin/src/main/java/org/apache/shenyu/admin/config/RegistrationProtocolInterceptor.java) treats a missing `X-Shenyu-Register-Protocol-Version` header as v1 and accepts an explicit value of `1`. It rejects other explicit values, including an empty value, or multiple version headers with HTTP 400. Successful registration retains the plain `success` body. Existing authentication is still required.

The interceptor advertises `X-Shenyu-Register-Protocol-Version: 1` and `X-Shenyu-Register-Capabilities` on the covered POST endpoints. Capabilities are `metadata`, `uri`, `api-doc`, `discovery-config`, `mcp`, `offline`, and `heartbeat`. `X-Shenyu-Client-Version` identifies an implementation; it is not a protocol version or a compatibility guarantee. Historical `8b93088973bf` sends legacy requests without those headers; candidate #16 adds v1/client headers and tests `success` responses without new response headers. Do not infer automatic capability downgrade from either behavior.

| Direction / combination | Expectation and required action |
| --- | --- |
| Old client to new Admin | Preserve the legacy v1 paths, field meanings, defaults for missing optional fields, authentication, and response body. Current Admin fixtures cover headerless requests and optional fields. Verify a real old client before marking an exact release pair supported. |
| New client to old Admin | Use only endpoints and field semantics understood by that Admin. Missing capability headers mean capabilities are unknown. Verify the required subset explicitly, disable optional features only when the client supports doing so, or upgrade Admin. Do not assume automatic fallback. |
| Same protocol with additive optional fields | Current Admin fixtures check that unknown fields are tolerated and missing optional fields retain defaults. Check the older receiver independently; do not generalize current receiver behavior to historical releases. |
| Required endpoint/capability absent, or explicit incompatible protocol version | **Unsupported** for that feature or protocol. A new client version cannot add an endpoint to an old Admin. Required field changes or changed meanings need an explicit protocol migration and new matrix evidence. |
| Mixed Admin/bootstrap releases or registry transports | **Best effort** until the exact sync channel, registry versions, payloads, plugins, and routing behavior are tested. HTTP controller fixtures do not verify ZooKeeper, Nacos, etcd, or another registry transport. |

The current controller fixtures cover metadata, URI registration (including heartbeat-enabled URI payloads), offline, API docs, discovery config, MCP, and heartbeat. They mock registration publishing and use standalone MVC controllers. Their scope ends before database persistence, live registry discovery, Admin-to-gateway synchronization, and gateway requests.

Upgrade Admin and bootstrap as a verified server pair before enabling client features that require new capabilities. Existing clients can then be upgraded independently within the tested wire contract. Keep old capabilities available during a rolling upgrade, and test rollback with the old client payloads and server data before publishing a compatibility claim.

## Updating the matrix

Use the [release compatibility checklist](../shenyu-dist/README.md#release-compatibility-checklist) for every main or client release, and whenever the JDK baseline, application framework, registration contract, or registry transport changes. Update this document in the same change as the compatibility change. Preserve historical rows and their evidence; add rows for new releases instead of treating a newer snapshot as proof for an older tag.

For each new row, record exact client artifact versions and source ref, Admin and bootstrap versions, client JDK baseline and successfully tested JDKs, server JDK baseline and successfully tested JDKs, transport, capabilities, support level, limitations, and dated links to results. Separate configured jobs, reported local results, passing CI, and full deployment verification. A failed required check prevents promotion; keep unverified combinations marked best effort and document known incompatible combinations as unsupported.

Release verification must exercise application startup with the selected client starter, authenticated registration, persisted metadata and service addresses, heartbeat and offline behavior, discovery, sync to bootstrap, and a request through the required gateway plugin. Include API docs and MCP when claimed by the row. Test an old client with the candidate Admin and the candidate client with the oldest Admin claimed by the row, including upgrade and rollback.

The existing controller fixture suite can be replayed from the main repository with:

```bash
./mvnw -B -ntp -pl shenyu-admin -am test \
  -Dtest=RegistrationProtocolCompatibilityTest \
  -Dsurefire.failIfNoSpecifiedTests=false -Djacoco.skip=true
```

For the historical client, use its pinned wrapper and claimed JDKs. For candidate #16, follow its pinned [preparation guide](https://github.com/apache/shenyu-client-java/blob/a37668298edef5c501e8b56a0595ed50ef895656/docs/MIGRATION.md): prepare pinned main foundations, then run `./mvnw -B -ntp clean verify -Pexamples -Dmaven.javadoc.skip=true -Djacoco.skip=true -Ddocker.skip=true` and its client-owned source-runtime runner on Java 17/21. That preparation does not replace released-image verification. Run main checks and deployment smoke tests on every claimed server runtime. Use `mvnw.cmd` on Windows. Record exact commands, JDKs, refs and results; commands alone do not certify a release pair.
