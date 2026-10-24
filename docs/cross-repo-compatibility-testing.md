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

# Cross-repository registration compatibility testing

Strategy for #6367, aligned with [#7602](https://github.com/apache/shenyu/issues/7602)
and the [migration guide](../shenyu-register-center/CLIENT_MIGRATION.md). The migration
targets Java 17 / Spring Boot 3, with Java 21 verification following main's policy;
releases remain independent. Historical Java 8 clients remain wire-compatibility
fixtures, not the target SDK baseline. See the [implementation/evidence matrix](java-client-compatibility.md#migration-target-and-current-implementations).

At the 2026-10-24 check, main prerequisites are merged in #7605. External
[draft #16](https://github.com/apache/shenyu-client-java/pull/16), pinned at
`a37668298ede`, configures client-owned source-runtime jobs and reports local
HTTP/ZooKeeper/gRPC/MCP results. No checks are attached to that head and no
released Admin/bootstrap matrix is certified. The remaining lanes below are
promotion requirements, not claims that hosted deployment jobs have passed.

## Scenarios and observable outcomes

| Scenario | Exercise | Required evidence |
| --- | --- | --- |
| HTTP metadata | Start a minimal Spring MVC application using the selected client starter; authenticate and register a unique endpoint/namespace | One persisted selector/rule/metadata identity with the expected path, protocol and enablement; a success acknowledgement alone is insufficient |
| Service address | Register the application's bound host/port, then send the same event again and concurrently through two Admin instances | Exactly one logical upstream and selector; no duplicate-key/TooManyResults failures |
| Heartbeat | Send beats, restart one Admin, then stop beats | Persisted last heartbeat advances; configured timeout marks the instance offline; another Admin's missing local heartbeat does not overwrite valid persisted liveness |
| Gateway | Start bootstrap with a real sync transport; wait for the unique route, then send a request through the gateway | Exact upstream response/status/body and route identity; no direct backend request counted as gateway success |
| Offline/crash | Gracefully close one client, then forcibly stop a second upstream without an offline callback | Route removal after deregistration/active checks; healthy siblings remain usable; empty final snapshot removes stale gateway destinations |
| Independent discovery | Register/discover/deregister with one real disposable provider | Provider key/serialization compatibility and expiry/watch removal; test each provider claimed by a release |
| MCP | Register a known tool, list/invoke through bootstrap, restart bootstrap and repeat | Correct schema/result without manually toggling a rule; test rule-before-selector and selector-before-rule replay |
| Version negotiation | Replay headerless v1, explicit v1, unsupported/multiple headers and additive unknown fields | Legacy behavior preserved; invalid versions rejected before enqueue; new optional fields do not break old receivers |
| API documentation | Submit a serialized document and query its persisted detail | Body/schema and source code preserved, independently of deprecated viewers |

Use a distinct test namespace, application name and port per run. Poll observable
readiness with a deadline; do not use fixed sleep as success. On timeout retain
redacted logs, current route/instance state and the last unsuccessful response.

## Matrix and scheduling

Resolve immutable client coordinates plus source ref and Admin/bootstrap image digest
plus source ref. Record the actual application, build and runtime JDK vendor/patch.
Never use an unpinned moving SNAPSHOT to certify a release.

| Lane | Client | Admin/bootstrap | JDKs | Trigger / budget estimate |
| --- | --- | --- | --- | --- |
| Main PR contract | Serialized legacy and additive-field fixtures, no shared DTO dependency | Changed main code | Main baseline 17; Java 21 compatibility lane | Every registration/DTO/heartbeat/sync change; roughly 2-5 minutes warm |
| Client PR contract | Java 17 / Boot 3 candidate plus fixtures from the published main protocol artifact | Pinned stable Admin when wire semantics change | Client/server 17; Java 21 verification under main's policy | Client-owned CI; roughly 5-10 minutes warm |
| Main-change deployed smoke | Pinned independent-client artifact or explicit counterpart source candidate | Candidate Admin/bootstrap | Application 17 for the migration candidate; historical 8 only for an explicitly selected legacy fixture; server 17 | Client-owned or external workflow triggered for main changes, outside main's reactor; roughly 5-10 minutes warm |
| Nightly | Oldest supported and latest released client, plus candidate client | Latest stable and candidate main | Client/main 17/21; separate historical Java 8 fixture when claimed | Client-owned or external workflow; roughly 15-30 minutes with parallel jobs |
| Release | Every pair claimed supported in the matrix | Candidate and oldest supported receiver, including upgrade/rollback | Migration client 17/21; 23/25 or legacy 8 only with explicit support/evidence | Before release promotion; roughly 30-60 minutes with provider lanes |

These are planning budgets, not measured results. Measure cold image/artifact downloads
separately and update budgets after the first ten CI runs. Do not multiply every
framework, JDK and registry into every PR. Release scope must still cover all
combinations advertised as supported. Keep runtime support, configured CI and passing
deployment evidence distinct in [the matrix](java-client-compatibility.md).

## Harness and artifact selection

Keep cross-repository E2E in the client repository or an external workflow, reviewed
by registration and client maintainers. Main's reactor uses raw JSON fixtures and
main-owned APIs, never independent client artifacts. The harness takes:

- client repository ref or published Maven coordinates, client application JDK;
- main repository ref or published Admin/bootstrap image digests, server JDK;
- database image/digest, sync channel, optional registry image/digest;
- unique namespace, startup/route/heartbeat deadlines and output directory.

For a main PR, an external/client-owned job can build candidate Admin/bootstrap
with main's wrapper and a fixture application with a pinned released split-client
starter. Keep their builds, dependency trees and classpaths separate. Reject client
artifacts in main-owned server/test dependencies; the Java client may consume the
released main protocol/register/registry foundations. An explicit counterpart-source
lane must record both commits and remain distinct from released-pair verification.

For a client PR, build/install only candidate client artifacts into an isolated Maven
repository or publish an expiring CI artifact. Launch the fixture with those exact
artifacts against pinned stable server images. Do not silently substitute an in-tree
client if artifact resolution fails. Candidate #16's source-preparation lane can
build pinned main foundations/Admin/bootstrap in a separate checkout with main's
wrapper; this documented exception permits source review before publication, not
SNAPSHOT release certification or a reverse dependency.

The minimum environment is H2 Admin, bootstrap with WebSocket sync and one independent
Spring MVC fixture process. Use Docker Compose for process/network isolation or
Testcontainers with a bounded shared network and dynamically mapped host ports.
The fixture must announce its actual bound container address/port; localhost inside
a container refers to that container. A local-process equivalent is useful for
contributors without Docker, but must record different topology and cannot certify
provider/image parity.

Exercise one real registry in the first discovery lane (choose a provider supported
by both selected artifacts). Add provider-specific Testcontainers/Compose services
later. Registry mocks do not establish session expiry/watch compatibility. Separate
MCP fixture dependencies from the small HTTP fixture so extra framework requirements
do not change the declared client baseline. Any historical Java 8 fixture needs its
own compatible dependency graph.

## Implemented prerequisites and remaining deployment gates

1. Main #7605 already supplies the protocol artifact, eight fixtures, dependency
   guards and receiver tests. The documentation landed in #7947; keep
   RegistrationProtocolCompatibilityTest as a fast main-owned receiver gate.
2. Review candidate #16's client-owned source harness and retain its separate
   local/hosted evidence. It exercises registration, heartbeats, proxying and
   shutdown/removal; publication and released-image verification remain pending.
3. Add a client-owned/external main-change smoke lane and a client-PR lane against
   stable released Admin/bootstrap. Pin published client coordinates and image
   digests. Fail visibly on missing dependencies; do not add client imports to main.
4. Add reverse skew, two Admins, independent discovery, MCP restart and database
   upgrade/rollback to nightly/release lanes. Expand advertised support only when
   corresponding evidence is retained.

## Failure handling, cost and release evidence

Set a job deadline, individual readiness deadlines and bounded retry/backoff.
Distinguish artifact-download/setup failure from assertion failure, but neither is
a passing compatibility result. Retry the environment at most once and preserve the
first failure. Never retry only an assertion until it becomes green.

Cache immutable Maven downloads and image layers; keep candidate artifacts isolated.
Use changed-path selection for PR cost, explicit cancellation of superseded PR runs
and bounded concurrency. Always close clients, gateways, Admins and registry services
in finally/CI cleanup, including failed startups. Redact credentials and payload secrets.

Persist a machine-readable result containing both refs/digests, Maven coordinates,
actual JDKs, provider/database versions, enabled features, command, duration and each
assertion outcome. Attach fixture dependency trees and redacted process logs.
A release checklist must link this evidence and the matching compatibility-matrix
row, identify reviewer ownership and document known exclusions. Block supported
status when a required lane fails or was not run.

The #7602 target is Java 17 / Boot 3. Later server JDK proposals must independently
review the client/foundation floor and preserve supported legacy wire behavior.
Build/release maintainers own CI/image integrity, Admin/runtime owners review routing
and liveness assertions, and client owners review framework/wire fixtures.

