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

# Boundary between ShenYu and shenyu-client-java

This document defines the responsibility boundary for [issue #6364](https://github.com/apache/shenyu/issues/6364), reconciled with the migration tracked by [#7602](https://github.com/apache/shenyu/issues/7602) under [#8036](https://github.com/apache/shenyu/issues/8036). Use it when moving or changing Java registration clients, starters, registration payloads, heartbeat, discovery instance registration, or MCP registration. The [migration guide](../shenyu-register-center/CLIENT_MIGRATION.md), [wire contract](registration-protocol-compatibility.md) and [cross-repository test strategy](cross-repo-compatibility-testing.md) define the release gates and verification.

The main repository owns Admin, gateway behavior and shared protocol/foundation artifacts. [shenyu-client-java](https://github.com/apache/shenyu-client-java) is the target owner of the Java registration code embedded in business applications. The #7602 target is Java 17 / Spring Boot 3, with Java 21 verification following main's CI policy. Java 8 / Spring Boot 2 is the historical split-client implementation, not the target for new migration work. Preserving legacy registration wire compatibility does not require the new SDK to run on Java 8.

Dependencies must remain one-way: `shenyu-client-java -> released apache/shenyu protocol/foundation artifacts`. Main-owned production code, tests, bootstrap and retained examples must not depend on independent client artifacts. In-tree business clients and their examples remain staged in main until the release gates pass; they are not independent-client dependencies.

## Inventory and migration baseline

The original inventory was checked on 2026-10-16 against main `2.7.2-SNAPSHOT` at [`297ac6f84985`](https://github.com/apache/shenyu/tree/297ac6f849857bd0c2ad368709013e98494b0330) and split-client [`8b93088973bf`](https://github.com/apache/shenyu-client-java/tree/8b93088973bf9f8f50d614e708bb87f662b707d5). The historical split-client inventory below is retained for that exact commit. Migration status was rechecked on 2026-10-24 against main [`86e932cdf`](https://github.com/apache/shenyu/tree/86e932cdf) and external preparation [`a37668298ede`](https://github.com/apache/shenyu-client-java/tree/a37668298edef5c501e8b56a0595ed50ef895656):

| State | Baseline and ownership | Evidence and limits |
| --- | --- | --- |
| Historical implementation, still the external `main` branch at this check | Java 8 / Boot 2.7.18; copied common/register DTOs, registry API and beat modules | `8b93088973bf`; this is source inventory and historical test evidence, not the migration policy or a published release guarantee |
| Current in-tree implementation | Java 17 / Boot 3.3.1; main-owned foundations and staged business clients/examples | [#7605](https://github.com/apache/shenyu/pull/7605) merged as `fd2a1efac9d5`; no client-source removal or coordinate handoff is implied |
| External migration candidate | Java 17 / Boot 3.3.1, Java 21 verification; consumes main-owned foundations and removes copied registry/beat ownership | [shenyu-client-java#16](https://github.com/apache/shenyu-client-java/pull/16), draft at `a37668298ede`; local source verification is reported in that PR, with no check results attached to this head at the check date |
| Published migration | Released foundations, external client artifacts and released Admin/bootstrap compatibility evidence | Still pending under #7602; preparation SNAPSHOTs and closed documentation tasks do not establish release support |

Client sources still present in main are migration work, not a second long-term implementation to maintain. The [status and release gates](../shenyu-register-center/CLIENT_MIGRATION.md#current-stage) distinguish merged prerequisites from remaining work.

### Client-related code still in the main repository

Paths below are relative to the main repository. The [root reactor](../pom.xml), [client reactor](../shenyu-client/pom.xml), [registration reactor](../shenyu-register-center/pom.xml), and [client starter reactor](../shenyu-spring-boot-starter/shenyu-spring-boot-starter-client/pom.xml) provide the module inventory.

| Current module or area | Contents | Ownership after separation |
| --- | --- | --- |
| `shenyu-client/shenyu-client-core` | Annotation scanning, registration lifecycle, API extraction, discovery instance registration, and client events | Split client |
| `shenyu-client/shenyu-client-http/shenyu-client-springmvc` | Spring MVC endpoint registration | Split client |
| `shenyu-client/shenyu-client-dubbo/{shenyu-client-dubbo-common,shenyu-client-apache-dubbo}` | Dubbo annotations, metadata extraction, and registration | Split client |
| `shenyu-client/shenyu-client-{grpc,sofa,tars}` | Framework-specific client registration | Split client |
| `shenyu-client/shenyu-client-websocket/shenyu-client-spring-websocket` | Spring WebSocket endpoint registration | Split client |
| `shenyu-client/shenyu-client-mcp/{shenyu-client-mcp-common,shenyu-client-mcp-register}` | MCP annotations, tool extraction, and registration | Split client |
| `shenyu-client/shenyu-client-{api-docs-annotations,autoconfig}` | API documentation annotations and client auto-configuration | Split client |
| `shenyu-spring-boot-starter/shenyu-spring-boot-starter-client` | Child starters: `common`, `springmvc`, `apache-dubbo`, `grpc`, `sofa`, `tars`, `spring-websocket`, `mcp`, and `beat` (each prefixed `shenyu-spring-boot-starter-client-`) | Eight business-service starters move to the split client; `beat` stays in main, including its tests and bootstrap integration |
| `shenyu-register-center/shenyu-register-client/{shenyu-register-client-api,shenyu-register-client-http}` | Registration SDK, retry handling, and HTTP transport | Split client; candidate #16 imports these modules rather than preserving the historical copied core implementation |
| `shenyu-register-center/shenyu-register-client-beat` | Gateway heartbeat sender and lifecycle | Main, together with `shenyu-spring-boot-starter-client-beat`; the `client` name does not make it business SDK code |
| `shenyu-register-center/shenyu-register-protocol` | JDK-only wire DTOs, event/data types, paths, version/capability constants and eight v1 fixtures | Main owns and releases the protocol artifact; the split client consumes it and both repos review wire changes |
| `shenyu-register-center/shenyu-register-common` | Registration configuration, queue subscribers, shared `RegisterUtils` and `OkHttpTools` | Main-owned foundation consumed by the split client; do not recreate the historical copied infrastructure |
| `shenyu-register-center/shenyu-register-mcp` | Shared MCP models and tool generation also consumed by Admin's Swagger import | Main-owned foundation; client annotations and application extraction stay client-owned |
| `shenyu-registry/shenyu-registry-api` and implementations | Registry SPI, entities, events and concrete discovery providers | Main owns and releases both API and providers; the split client consumes required released artifacts without a duplicate registry API |
| `shenyu-common`, `shenyu-disruptor`, `shenyu-spi`, `shenyu-protocol-*` | Shared foundations and server/protocol infrastructure | Main-owned artifacts; the split client may consume required released foundations without copying their sources |
| Client-facing examples | Nineteen roots in the [migration inventory](../.github/client-migration-inventory.json) | Move with their client dependencies and tests; retained main examples must not resolve external client artifacts |

Braces in paths abbreviate multiple modules. The registration client reactor currently contains API and HTTP modules. ZooKeeper, Nacos, etcd, and other instance registry implementations under `shenyu-registry` are a separate concern from HTTP metadata registration.

### Historical modules in shenyu-client-java at 8b93088973bf

The pinned split-client [root POM](https://github.com/apache/shenyu-client-java/blob/8b93088973bf9f8f50d614e708bb87f662b707d5/pom.xml) and [migration record](https://github.com/apache/shenyu-client-java/blob/8b93088973bf9f8f50d614e708bb87f662b707d5/progress.md) establish this historical inventory, not the desired ownership. Paths here are relative to that repository; it has no `shenyu-client/` parent directory.

| Module or area | Current state |
| --- | --- |
| `shenyu-client-core` | Contains local DTOs, constants, configuration, utilities, registration interfaces, HTTP registration, retries, and discovery listeners under `org.apache.shenyu.client.core.*`. These replace dependencies on main-repo `common` and `register.common` sources |
| `shenyu-client-http/shenyu-client-springmvc` | Present |
| `shenyu-client-dubbo/{shenyu-client-dubbo-common,shenyu-client-apache-dubbo}` | Present |
| `shenyu-client-{grpc,sofa,tars,motan}` | Present; `motan` is in the split reactor but absent from the inspected main client reactor |
| `shenyu-client-websocket/shenyu-client-spring-websocket` | Present |
| `shenyu-client-mcp/{shenyu-client-mcp-common,shenyu-client-mcp-register}` | Restored, including MCP registration payload and HTTP path support |
| `shenyu-client-{api-docs-annotations,autoconfig}` | Present |
| `shenyu-spring-boot-starter-client` | All nine child starters listed in the main inventory are present; Spring MVC, gRPC, and Spring WebSocket discovery auto-configuration has been restored |
| `shenyu-register-client-beat` | Restored, with client-local configuration/DTO support |
| `shenyu-registry-api` | Restored API needed by instance registration; concrete registry provider modules are not in this reactor |

That POM declares Java 8 and Spring Boot 2.7.18. Its migration record leaves live beat, discovery and MCP verification pending. Candidate #16 replaces this implementation with Java 17 / Boot 3, removes Motan and copied common/register/timer/DTO, registry API and gateway beat ownership, and imports business clients, API/HTTP registration, eight starters and nineteen example roots. Its [preparation guide](https://github.com/apache/shenyu-client-java/blob/a37668298edef5c501e8b56a0595ed50ef895656/docs/MIGRATION.md) describes local source verification and the remaining publication gates. Neither inventory alone establishes released interoperability.

## Responsibilities

| Area | Main ShenYu repository | shenyu-client-java |
| --- | --- | --- |
| Admin and management APIs | Registration receivers, authentication/authorization, validation, persistence, dashboard APIs, health decisions, and configuration publication | Authentication calls and payload submission needed by registration; no access to Admin's database |
| Bootstrap and gateway runtime | Launcher, WebFlux/plugin execution, routing, upstream selection, gateway caches, gateway lifecycle, and server-specific concurrency changes | Application registration lifecycle; no gateway runtime or plugin execution |
| Plugins and sync | All gateway plugins, Admin listeners, and Admin-to-gateway data sync channels and payloads | No dependency on gateway sync implementations |
| Java registration client and starters | Temporary in-tree copies until explicitly migrated/deprecated | Java annotations, framework adapters, API/docs extraction, client auto-configuration, configuration binding, registration SDK, HTTP/authentication, retry, and shutdown/offline behavior |
| Beat | Own `shenyu-register-client-beat`, its starter/tests, bootstrap reporting, receivers and instance health | Own application URI heartbeat/offline behavior in business-client registration; do not move gateway beat modules |
| Discovery registration | Own/release registry API and implementations, Admin discovery management, watching and gateway upstream updates | Publish application instances/configuration through client listeners and required released main-owned registry artifacts |
| MCP | Registration receiver, Admin Swagger import, persistence/rule generation, and MCP gateway/server plugins | Application annotations, method/tool/schema extraction, and MCP registration requests |
| Build and release | Main foundations, server distributions and tests; current Java 17 baseline | Independent client releases, Java 17 / Boot 3 migration, Java 21 verification, client/starter/example tests |

Gateway, plugin, sync, registry, SDK and Kubernetes starters outside the `shenyu-spring-boot-starter-client` subtree remain in main, as does the beat starter inside it. The request/signing SDK under `shenyu-sdk` and its starters are outside this split. Client-dependent examples move to the client repository; retained main examples and tests may consume released main-owned SDK/protocol/API artifacts, never independent client artifacts. Cross-repository E2E belongs in the client repository or an external workflow, not the main reactor.

## Cross-repository protocol contracts

The wire contract is defined by serialized fields and behavior, not matching Maven versions. The Java migration consumes main's released protocol/foundation artifacts; it must not copy DTO or utility source trees. Other language implementations can represent the same messages independently without sharing Java packages, inheritance or builders.

The main receiver's [RegistrationProtocol](../shenyu-register-center/shenyu-register-protocol/src/main/java/org/apache/shenyu/register/common/protocol/RegistrationProtocol.java), [DTOs](../shenyu-register-center/shenyu-register-protocol/src/main/java/org/apache/shenyu/register/common/dto), [registration controller](../shenyu-admin/src/main/java/org/apache/shenyu/admin/controller/ShenyuClientHttpRegistryController.java), and [instance controller](../shenyu-admin/src/main/java/org/apache/shenyu/admin/controller/InstanceController.java) are the current implementation references. Contract changes must document the request/response format, defaults, and compatibility behavior independently of these classes.

| Contract point | Boundary and compatibility surface | Review areas |
| --- | --- | --- |
| Metadata, URI, API docs, and offline registration | Client to Admin: POST `/shenyu-client/register-metadata`, `/shenyu-client/register-uri`, `/shenyu-client/register-apiDoc`, and `/shenyu-client/offline`. Preserve JSON field names/types, RPC identifiers, namespace handling, event meanings, authentication, and success/error behavior | Admin registration + Java client registration; affected RPC plugin area for metadata semantics |
| Beat | Service URI heartbeat uses `/shenyu-client/register-uri` with `eventType: HEARTBEAT`; instance reporting uses POST `/instance/beat` with `instanceIp`, `instancePort`, `instanceType`, `instanceInfo`, and `namespaceId`. These are distinct messages. Review identity, interval/expiry expectations, retry behavior, and health/offline transitions | Main instance/health and bootstrap beat owners; business-client registration owners for URI heartbeats |
| Discovery configuration and instance registration | Client to Admin: POST `/shenyu-client/register-discoveryConfig` carries discovery type, server list, listener node, handler, plugin, properties, and namespace. Client to registry: instance identity/address, status, weight, registry keys/paths, serialization, lifecycle, and removal semantics consumed by Admin/gateway | Admin discovery + Java client discovery + affected registry provider area |
| MCP registration | Client to Admin: POST `/shenyu-client/register-mcp` carries `metaDataRegisterDTO`, `namespaceId`, and `mcpConfig`. The JSON inside `mcpConfig`, including tool/request configuration and schemas, is part of the contract too | Admin MCP registration + Java MCP client + MCP plugin area |

Additional contract rules:

- Preserve metadata/URI identity and field meanings such as `appName`, `contextPath`, `rpcType`, `host`, `port`, and `namespaceId`. Current Admin defaults a missing namespace for metadata, URI, discovery config, MCP, and offline requests. Verify API-doc and beat behavior separately instead of applying that default to every DTO.
- Wire-visible event values such as `REGISTER`, `UPDATED`, `DELETED`, `OFFLINE`, and `HEARTBEAT` require compatibility review. Local disruptor queues, subscriber classes, and Java dispatch types remain implementation details unless a transport actually serializes them.
- Registration constants such as endpoint paths, headers, success markers and namespace defaults describe contract values owned by main's protocol artifact. Registry paths belong to main's registry API. The Java client consumes those released artifacts; implementation-specific caches and retry executors remain with their owning modules.
- The current main HTTP v1 interceptor accepts a missing version header as v1 and an explicit `X-Shenyu-Register-Protocol-Version: 1`. It rejects unsupported explicit versions or multiple version headers. It advertises `X-Shenyu-Register-Capabilities`; successful covered registration requests retain the plain `success` body. See the [interceptor](../shenyu-admin/src/main/java/org/apache/shenyu/admin/config/RegistrationProtocolInterceptor.java). Existing authentication remains required.
- `X-Shenyu-Client-Version` identifies the implementation and does not select the wire protocol. Historical `8b93088973bf` uses legacy requests; candidate #16 adds v1/client headers and tests legacy `success` responses without new response headers. Neither implies arbitrary capability fallback. HTTP headers do not version external registry payloads.
- Java `InstanceEntity` and `DiscoveryUpstreamData` are local representations. Only the fields actually published through a registry, together with provider-specific paths and encodings, form the instance contract. Admin database IDs/timestamps and gateway cache structures need not be copied into the client.

Admin-to-gateway sync is owned entirely by main and follows registration acceptance/persistence. A client registration protocol change must not require the application to import gateway `PluginData`, `SelectorData`, or other runtime cache models.

## Remaining coupling and migration actions

The following dependencies are intentionally main-owned. #7605 already removed Admin's business-client production/test dependencies and the beat modules' business-client dependencies; these retained foundations must not be mistaken for unfinished reverse-dependency removal.

| Remaining dependency | Evidence | Required treatment |
| --- | --- | --- |
| Admin consumes registration DTOs and constants | [Admin POM](../shenyu-admin/pom.xml) depends on `shenyu-register-common`; controllers, services, transfer code, and subscribers use `org.apache.shenyu.register.common.dto.*`, `EventType`, `DataType`, and `RegistrationProtocol` from the main `shenyu-register-protocol` module | Keep receiver-side contract representations in main. Removing client senders must not remove the DTOs or force Admin to depend on the split client's `client.core` classes |
| Admin consumes registration infrastructure | [RegisterCenterConfiguration](../shenyu-admin/src/main/java/org/apache/shenyu/admin/config/RegisterCenterConfiguration.java) uses `ShenyuRegisterCenterConfig`; [RegisterServerConsumerExecutor](../shenyu-admin/src/main/java/org/apache/shenyu/admin/disruptor/executor/RegisterServerConsumerExecutor.java) uses queue subscribers from `shenyu-register-common` | Keep this foundation in main, including the shared HTTP utilities used by beat and registration; moving API/HTTP client modules does not remove it |
| Admin uses shared MCP generation | [SwaggerImportServiceImpl](../shenyu-admin/src/main/java/org/apache/shenyu/admin/service/impl/SwaggerImportServiceImpl.java) imports models and generators from `shenyu-register-mcp` | #7605 moved this shared code out of the business client; consume the released main module from the client without restoring an Admin-to-client dependency |
| Bootstrap depends on a client-named beat starter | [Bootstrap POM](../shenyu-bootstrap/pom.xml) includes `shenyu-spring-boot-starter-client-beat`; [HeartbeatListenerConfiguration](../shenyu-spring-boot-starter/shenyu-spring-boot-starter-client/shenyu-spring-boot-starter-client-beat/src/main/java/org/apache/shenyu/register/client/beat/HeartbeatListenerConfiguration.java) and [HeartbeatListener](../shenyu-register-center/shenyu-register-client-beat/src/main/java/org/apache/shenyu/register/client/beat/HeartbeatListener.java) use main's `ShenyuConfig` and server properties | Keep both modules, tests and lifecycle in main. Adjust their aggregator placement when removing business starters, not their repository ownership |
| Main common constants alias registration paths | [Constants](../shenyu-common/src/main/java/org/apache/shenyu/common/constant/Constants.java) delegates path values to `RegistrationProtocol` | Retain main aliases and the published protocol artifact; do not replace them with copied client-core constants |
| Staged registration clients depend on main foundations | [HttpClientRegisterRepository](../shenyu-register-center/shenyu-register-client/shenyu-register-client-http/src/main/java/org/apache/shenyu/register/client/http/HttpClientRegisterRepository.java) uses main common/register utilities; [InstanceRegisterListener](../shenyu-client/shenyu-client-core/src/main/java/org/apache/shenyu/client/core/register/InstanceRegisterListener.java) uses main discovery models and registry SPI | Preserve the direction after migration using released foundations; verify each actual registry provider and its lifecycle |

The historical split-client [core POM](https://github.com/apache/shenyu-client-java/blob/8b93088973bf9f8f50d614e708bb87f662b707d5/shenyu-client-core/pom.xml) and [registry API POM](https://github.com/apache/shenyu-client-java/blob/8b93088973bf9f8f50d614e708bb87f662b707d5/shenyu-registry-api/pom.xml) use released `shenyu-disruptor`/`shenyu-spi` `2.6.1`. That historical graph does not define the migration target. Candidate #16 separates client `2.7.2-migration-SNAPSHOT` from `shenyu.foundation.version=2.7.2-SNAPSHOT` and builds pinned main foundations only for source preparation. Formal client publication requires real released foundation versions, a coordinate-ownership handoff and compatibility evidence.

Do not add cross-repository source directories, relative parent POMs, or snapshot dependencies merely to reuse a DTO or utility. Do not place in-tree and split variants of the same client artifacts/classes on one application classpath. Migration PRs must select one implementation, account for coordinate/package compatibility, and document any consumer changes.

## Review ownership and rules for future changes

The review areas in the contract table are responsibility assignments by subsystem, not a claim that a named person has approved a protocol. Protocol PRs must request review from both the main receiver area and the split-client producer area. Include discovery/registry or MCP/plugin reviewers when those semantics change. Main-only implementation changes can stay with the relevant main area when the observable contract is unchanged.

For any future PR that moves or modifies client-related code:

1. Link this document and #7602, identify the owning repo, affected modules and wire changes. Use Java 17 / Boot 3 for the migration and Java 21 verification under main's policy. Keep historical Java 8 evidence and headerless v1 receiver compatibility separate from the target SDK baseline. Further server baseline proposals do not silently change the client/foundation floor.
2. For a protocol change, describe old and new request/response examples, field types/defaults, authentication, capability/version behavior, and registry encodings where relevant. Link counterpart issues/PRs and obtain review from both producer and receiver areas before merging a breaking change.
3. Prefer additive optional fields with compatible defaults. Renaming/removing fields or endpoints, changing required fields/types or enum meanings, changing success/error behavior, or changing registry identity/path/serialization requires an explicit protocol migration, coexistence period, and rollback plan. Preserve legacy headerless HTTP v1 clients during that migration.
4. Release Admin support before enabling a new client capability. Record the minimum compatible Admin version and required capabilities. Client Maven versions and server Maven versions are independent; matching version numbers do not prove compatibility. Document behavior when a required endpoint or capability is absent, including old Admins that advertise no capabilities.
5. Validate an old client against the candidate Admin and the candidate client against the oldest Admin claimed as compatible. Cover registration/authentication, API docs, heartbeat/offline, discovery, and MCP as applicable. Run the split client on its supported JDK/framework combinations and server checks on the server baseline. For discovery, exercise an actual provider and its watch/removal behavior.
6. Run the main [registration compatibility fixtures](../shenyu-admin/src/test/java/org/apache/shenyu/admin/controller/RegistrationProtocolCompatibilityTest.java), and use serialized payload fixtures on both sides so compatibility does not depend on shared Java classes. Controller mocks establish receiver behavior only; separately verify persistence, health handling, Admin-to-gateway sync, and gateway routing for end-to-end claims.
7. Before removing an in-tree module, check production, test, example, starter, distribution and dependency-management references. Preserve main-owned DTO/config/MCP/registry consumers and both beat modules. Release main foundations first, external client artifacts second, and remove staged sources last; no coordinate may be published by both repositories. Retained main modules must build without external client artifacts.
8. Update this inventory, contract documentation, release compatibility evidence, and release notes in the corresponding change. Keep counterpart PRs linked so future migrations and fixes have an explicit owner and compatibility record.
