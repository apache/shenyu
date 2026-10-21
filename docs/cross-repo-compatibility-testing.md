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

Proposal for #6367. This specifies test ownership, artifacts and promotion gates;
it does not claim that independent-client deployment jobs already exist.
The main repository and shenyu-client-java keep independent JDK baselines and releases.

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
| Client PR contract | Candidate client plus serialized receiver fixtures | Pinned current stable Admin when the change touches wire semantics | Client baseline 8 and one 17/21 application lane; server 17 | Relevant client PRs; roughly 5-10 minutes warm |
| Main PR deployed smoke | Pinned supported independent-client artifact | Candidate Admin/bootstrap | Application 8 or 17 according to starter; server 17 | Registration/identity/sync changes; roughly 5-10 minutes warm |
| Nightly | Oldest supported and latest released client, plus candidate client | Latest stable and candidate main | Client 8/17/21; main 17/21 | Sparse representative combinations; roughly 15-30 minutes with parallel jobs |
| Release | Every pair claimed supported in the matrix | Candidate and oldest supported receiver, including upgrade/rollback | Include 23/25 only where explicitly claimed; incompatible baselines excluded | Before release promotion; roughly 30-60 minutes with provider lanes |

These are planning budgets, not measured results. Measure cold image/artifact downloads
separately and update budgets after the first ten CI runs. Do not multiply every
framework, JDK and registry into every PR. Release scope must still cover all
combinations advertised as supported. Keep runtime support, configured CI and passing
deployment evidence distinct in [the matrix](java-client-compatibility.md).

## Harness and artifact selection

Implement a versioned external-process harness owned jointly by registration and
client maintainers. Each repository invokes the same scenario protocol with inputs:

- client repository ref or published Maven coordinates, client application JDK;
- main repository ref or published Admin/bootstrap image digests, server JDK;
- database image/digest, sync channel, optional registry image/digest;
- unique namespace, startup/route/heartbeat deadlines and output directory.

For a main PR, build candidate Admin/bootstrap with the repository Maven wrapper.
Build the fixture application using a pinned released split-client starter from the
artifact repository; inspect its dependency tree to reject accidental in-tree
client/register DTO dependencies. A one-time candidate-client lane can accept a
counterpart ref explicitly, recording both commit IDs.

For a client PR, build/install only candidate client artifacts into an isolated Maven
repository or publish an expiring CI artifact. Launch the fixture with those exact
artifacts against pinned stable server images. Do not build server sources from the
client checkout or silently substitute an in-tree client if artifact resolution fails.

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
MCP fixture dependencies from the small HTTP fixture so framework requirements do not
raise the ordinary client's Java 8 baseline.

## First implementation

1. Land this contract, [protocol inventory](registration-protocol-compatibility.md)
   and version matrix. Keep existing RegistrationProtocolCompatibilityTest as a
   fast receiver gate.
2. Add one minimal external Spring MVC fixture and a harness command accepting
   explicit client coordinates and server images. Prove authentication, persistence,
   heartbeat and proxying, then stop the backend and require route eviction.
3. Add a single main-PR deployed smoke lane for registration-related paths and a
   client-PR lane against stable Admin. Gate promotion only after establishing a
   reliable baseline; fail visibly on unresolved dependencies rather than skipping.
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

This proposal adds no requirement for Java clients to upgrade JDK with the server.
Build/release maintainers own CI/image integrity, Admin/runtime owners review routing
and liveness assertions, and client owners review framework/wire fixtures.

