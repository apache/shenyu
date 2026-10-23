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

This document defines the responsibility boundary for [issue #6364](https://github.com/apache/shenyu/issues/6364). Use it when moving or changing Java registration clients, starters, registration payloads, heartbeat, discovery instance registration, or MCP registration. The [wire contract](registration-protocol-compatibility.md) and [cross-repository test strategy](cross-repo-compatibility-testing.md) define the shared behavior and verification.

The main repository owns Admin and gateway behavior. [shenyu-client-java](https://github.com/apache/shenyu-client-java) owns the Java registration code embedded in business applications. They communicate through documented wire contracts and can release independently. The split client preserves its JDK 8 baseline; the main repository currently requires JDK 17 and can evolve its server JDK and threading model independently.

## Inventory and migration baseline

Inventory checked on 2026-10-16 against main repository `2.7.2-SNAPSHOT` at [`297ac6f84985`](https://github.com/apache/shenyu/tree/297ac6f849857bd0c2ad368709013e98494b0330) and split-client commit [`8b93088973bf`](https://github.com/apache/shenyu-client-java/tree/8b93088973bf9f8f50d614e708bb87f662b707d5). The tables distinguish current source locations from the intended ownership. Client sources still present in the main reactor are migration work, not a second long-term implementation to maintain. This document does not remove modules or change published artifacts.

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
| `shenyu-spring-boot-starter/shenyu-spring-boot-starter-client` | Child starters: `common`, `springmvc`, `apache-dubbo`, `grpc`, `sofa`, `tars`, `spring-websocket`, `mcp`, and `beat` (each prefixed `shenyu-spring-boot-starter-client-`) | Application starters belong to the split client; separate bootstrap heartbeat integration as described below |
| `shenyu-register-center/shenyu-register-client/{shenyu-register-client-api,shenyu-register-client-http}` | Registration SDK, retry handling, and HTTP transport | Split client; equivalent code is currently inside its `shenyu-client-core` |
| `shenyu-register-center/shenyu-register-client-beat` | Heartbeat sender, currently including bootstrap-specific configuration | Split client owns reusable sending support; main owns bootstrap configuration and lifecycle integration |
| `shenyu-register-center/shenyu-register-protocol` | Registration DTOs, event/data types, registration paths, and HTTP version/capability constants | Main owns its receiver-side representations; the wire contract is reviewed across both repos |
| `shenyu-register-center/shenyu-register-common` | Mixed registration configuration, queue subscribers, `RegisterUtils`, and `OkHttpTools` | Split by use: main keeps server configuration/consumption; client keeps its own configuration, HTTP, and retry implementation |
| `shenyu-register-center/shenyu-register-mcp` | MCP models and tool generation also consumed by Admin's Swagger import | Main keeps Admin-side import/generation; split client owns application-side extraction/generation |
| `shenyu-registry/shenyu-registry-api` | Registry SPI, instance entity, configuration, events, and path conventions used by discovery registration | Main keeps gateway/Admin registry APIs; split client has a local API subset. Review actual registry wire formats across both repos |
| `shenyu-common` | Registration path aliases and discovery models/utilities used by in-tree clients, alongside server models | Main keeps server models/utilities; client uses minimal local equivalents rather than depending on the full module |

Braces in paths abbreviate multiple modules. The registration client reactor currently contains API and HTTP modules. ZooKeeper, Nacos, etcd, and other instance registry implementations under `shenyu-registry` are a separate concern from HTTP metadata registration.

### Modules restored in shenyu-client-java

The pinned split-client [root POM](https://github.com/apache/shenyu-client-java/blob/8b93088973bf9f8f50d614e708bb87f662b707d5/pom.xml) and [migration record](https://github.com/apache/shenyu-client-java/blob/8b93088973bf9f8f50d614e708bb87f662b707d5/progress.md) establish the current inventory. Paths here are relative to that repository; it has no `shenyu-client/` parent directory.

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

The split client's POM declares Java 8 and Spring Boot 2.7.18. Restored source and a compilable API do not establish a working registry provider or full interoperability with Admin/bootstrap. The migration record leaves live beat, discovery, and MCP verification pending. No additional registration transport or framework support is implied by this inventory.

## Responsibilities

| Area | Main ShenYu repository | shenyu-client-java |
| --- | --- | --- |
| Admin and management APIs | Registration receivers, authentication/authorization, validation, persistence, dashboard APIs, health decisions, and configuration publication | Authentication calls and payload submission needed by registration; no access to Admin's database |
| Bootstrap and gateway runtime | Launcher, WebFlux/plugin execution, routing, upstream selection, gateway caches, gateway lifecycle, and server-specific concurrency changes | Application registration lifecycle; no gateway runtime or plugin execution |
| Plugins and sync | All gateway plugins, Admin listeners, and Admin-to-gateway data sync channels and payloads | No dependency on gateway sync implementations |
| Java registration client and starters | Temporary in-tree copies until explicitly migrated/deprecated | Java annotations, framework adapters, API/docs extraction, client auto-configuration, configuration binding, registration SDK, HTTP/authentication, retry, and shutdown/offline behavior |
| Beat | Receive and interpret beat messages, maintain instance health, and integrate bootstrap's own reporter | Reusable beat sender and client starter support compatible with the application's JDK/framework |
| Discovery registration | Admin discovery management, registry watching/consumption, gateway upstream updates, and server-side registry implementations | Publish application instances and discovery configuration; own client-side listeners and any compatible provider adapters shipped for applications |
| MCP | Registration receiver, Admin Swagger import, persistence/rule generation, and MCP gateway/server plugins | Application annotations, method/tool/schema extraction, and MCP registration requests |
| Build and release | Server dependency management, server JDK baseline, server distributions, and deployment tests | Client dependency management, JDK 8 compatibility, client artifacts/starters, and application-side tests |

Gateway, plugin, sync, registry, SDK, and Kubernetes starters outside the `shenyu-spring-boot-starter-client` subtree remain in main. The request/signing SDK under `shenyu-sdk` and its starters are outside this registration-client split; moving them needs a separate scope decision. Examples and integration suites belong with the behavior they exercise and may use released client artifacts.

## Cross-repository protocol contracts

Share field meanings and wire behavior, not Java source trees. Each repository may represent the same message using its own DTOs, enums, and utilities. Matching Java package names, inheritance, builders, or Maven versions are not interoperability requirements.

The main receiver's [RegistrationProtocol](../shenyu-register-center/shenyu-register-protocol/src/main/java/org/apache/shenyu/register/common/protocol/RegistrationProtocol.java), [DTOs](../shenyu-register-center/shenyu-register-protocol/src/main/java/org/apache/shenyu/register/common/dto), [registration controller](../shenyu-admin/src/main/java/org/apache/shenyu/admin/controller/ShenyuClientHttpRegistryController.java), and [instance controller](../shenyu-admin/src/main/java/org/apache/shenyu/admin/controller/InstanceController.java) are the current implementation references. Contract changes must document the request/response format, defaults, and compatibility behavior independently of these classes.

| Contract point | Boundary and compatibility surface | Review areas |
| --- | --- | --- |
| Metadata, URI, API docs, and offline registration | Client to Admin: POST `/shenyu-client/register-metadata`, `/shenyu-client/register-uri`, `/shenyu-client/register-apiDoc`, and `/shenyu-client/offline`. Preserve JSON field names/types, RPC identifiers, namespace handling, event meanings, authentication, and success/error behavior | Admin registration + Java client registration; affected RPC plugin area for metadata semantics |
| Beat | Service URI heartbeat uses `/shenyu-client/register-uri` with `eventType: HEARTBEAT`; instance reporting uses POST `/instance/beat` with `instanceIp`, `instancePort`, `instanceType`, `instanceInfo`, and `namespaceId`. These are distinct messages. Review identity, interval/expiry expectations, retry behavior, and health/offline transitions | Admin instance/health + Java client beat; bootstrap area for gateway reporting |
| Discovery configuration and instance registration | Client to Admin: POST `/shenyu-client/register-discoveryConfig` carries discovery type, server list, listener node, handler, plugin, properties, and namespace. Client to registry: instance identity/address, status, weight, registry keys/paths, serialization, lifecycle, and removal semantics consumed by Admin/gateway | Admin discovery + Java client discovery + affected registry provider area |
| MCP registration | Client to Admin: POST `/shenyu-client/register-mcp` carries `metaDataRegisterDTO`, `namespaceId`, and `mcpConfig`. The JSON inside `mcpConfig`, including tool/request configuration and schemas, is part of the contract too | Admin MCP registration + Java MCP client + MCP plugin area |

Additional contract rules:

- Preserve metadata/URI identity and field meanings such as `appName`, `contextPath`, `rpcType`, `host`, `port`, and `namespaceId`. Current Admin defaults a missing namespace for metadata, URI, discovery config, MCP, and offline requests. Verify API-doc and beat behavior separately instead of applying that default to every DTO.
- Wire-visible event values such as `REGISTER`, `UPDATED`, `DELETED`, `OFFLINE`, and `HEARTBEAT` require compatibility review. Local disruptor queues, subscriber classes, and Java dispatch types remain implementation details unless a transport actually serializes them.
- Registration constants such as endpoint paths, header names, success markers, namespace defaults, and registry path conventions describe contract values. They may have separate local definitions. Thread factories, HTTP wrappers, caches, reflection utilities, and retry executors are implementation details owned by their repo.
- The current main HTTP v1 interceptor accepts a missing version header as v1 and an explicit `X-Shenyu-Register-Protocol-Version: 1`. It rejects unsupported explicit versions or multiple version headers. It advertises `X-Shenyu-Register-Capabilities`; successful covered registration requests retain the plain `success` body. See the [interceptor](../shenyu-admin/src/main/java/org/apache/shenyu/admin/config/RegistrationProtocolInterceptor.java). Existing authentication remains required.
- `X-Shenyu-Client-Version` identifies the implementation and does not select the wire protocol. The inspected split client still uses legacy requests; do not assume it negotiates capabilities or falls back automatically. These headers describe HTTP registration and do not version external registry payloads.
- Java `InstanceEntity` and `DiscoveryUpstreamData` are local representations. Only the fields actually published through a registry, together with provider-specific paths and encodings, form the instance contract. Admin database IDs/timestamps and gateway cache structures need not be copied into the client.

Admin-to-gateway sync is owned entirely by main and follows registration acceptance/persistence. A client registration protocol change must not require the application to import gateway `PluginData`, `SelectorData`, or other runtime cache models.

## Remaining coupling and migration actions

The following dependencies exist in the inspected main source. Their presence must be accounted for before deleting or moving any client-related module.

| Remaining dependency | Evidence | Required treatment |
| --- | --- | --- |
| Admin consumes registration DTOs and constants | [Admin POM](../shenyu-admin/pom.xml) depends on `shenyu-register-common`; controllers, services, transfer code, and subscribers use `org.apache.shenyu.register.common.dto.*`, `EventType`, `DataType`, and `RegistrationProtocol` from the main `shenyu-register-protocol` module | Keep receiver-side contract representations in main. Removing client senders must not remove the DTOs or force Admin to depend on the split client's `client.core` classes |
| Admin consumes mixed registration infrastructure | [RegisterCenterConfiguration](../shenyu-admin/src/main/java/org/apache/shenyu/admin/config/RegisterCenterConfiguration.java) uses `ShenyuRegisterCenterConfig`; [RegisterServerConsumerExecutor](../shenyu-admin/src/main/java/org/apache/shenyu/admin/disruptor/executor/RegisterServerConsumerExecutor.java) uses queue subscriber types from `shenyu-register-common` | Retain server configuration and consumers. Separate unused client configuration and HTTP utilities when removing the mixed module; its current POM also brings in OkHttp |
| Admin uses MCP generation code | [SwaggerImportServiceImpl](../shenyu-admin/src/main/java/org/apache/shenyu/admin/service/impl/SwaggerImportServiceImpl.java) imports `ShenyuMcpRequestConfig`, `ShenyuMcpTool`, and `McpToolsRegisterDTOGenerator` from `shenyu-register-mcp` | Keep or relocate Admin's generator/models within main. Coordinate generated MCP JSON with the independent application-side generator; do not copy client scanning/runtime code into Admin |
| Bootstrap depends on a client-named beat starter | [Bootstrap POM](../shenyu-bootstrap/pom.xml) includes `shenyu-spring-boot-starter-client-beat`; [HeartbeatListenerConfiguration](../shenyu-spring-boot-starter/shenyu-spring-boot-starter-client/shenyu-spring-boot-starter-client-beat/src/main/java/org/apache/shenyu/register/client/beat/HeartbeatListenerConfiguration.java) and [HeartbeatListener](../shenyu-register-center/shenyu-register-client-beat/src/main/java/org/apache/shenyu/register/client/beat/HeartbeatListener.java) use main's `ShenyuConfig` and server properties | Keep bootstrap binding, startup/shutdown, and reporting integration in main. Extract that integration before removing the in-tree starter; share only the beat wire contract with the split client's sender |
| Main common constants alias registration paths | [Constants](../shenyu-common/src/main/java/org/apache/shenyu/common/constant/Constants.java) delegates registration/login/beat path values to `RegistrationProtocol` | Retain local aliases while main callers need them. Do not replace main `shenyu-common` with client-core constants or require the split client to import server common |
| In-tree registration clients depend on server common/utilities and registry API | [HttpClientRegisterRepository](../shenyu-register-center/shenyu-register-client/shenyu-register-client-http/src/main/java/org/apache/shenyu/register/client/http/HttpClientRegisterRepository.java) uses `Constants`, `GsonUtils`, `AesUtils`, and `RegisterUtils`; [InstanceRegisterListener](../shenyu-client/shenyu-client-core/src/main/java/org/apache/shenyu/client/core/register/InstanceRegisterListener.java) uses `DiscoveryUpstreamData`, `ShenyuDiscoveryConfig`, and the registry SPI | Complete migration using the split client's local equivalents. Verify registry providers independently; restoring the API alone does not supply an SPI implementation |

The split client's pinned [core POM](https://github.com/apache/shenyu-client-java/blob/8b93088973bf9f8f50d614e708bb87f662b707d5/shenyu-client-core/pom.xml) and [registry API POM](https://github.com/apache/shenyu-client-java/blob/8b93088973bf9f8f50d614e708bb87f662b707d5/shenyu-registry-api/pom.xml) still use released `shenyu-disruptor`/`shenyu-spi` artifacts at `2.6.1`. These are explicit binary dependencies, not permission to import the current main reactor or a newer server dependency stack. Check JDK compatibility and transitive dependencies before changing them. If a new shared library is needed, agree on separate ownership, versioning, and a client-compatible baseline first.

Do not add cross-repository source directories, relative parent POMs, or snapshot dependencies merely to reuse a DTO or utility. Do not place in-tree and split variants of the same client artifacts/classes on one application classpath. Migration PRs must select one implementation, account for coordinate/package compatibility, and document any consumer changes.

## Review ownership and rules for future changes

The review areas in the contract table are responsibility assignments by subsystem, not a claim that a named person has approved a protocol. Protocol PRs must request review from both the main receiver area and the split-client producer area. Include discovery/registry or MCP/plugin reviewers when those semantics change. Main-only implementation changes can stay with the relevant main area when the observable contract is unchanged.

For any future PR that moves or modifies client-related code:

1. Link this document and identify the owning repo, affected modules, and whether the wire contract changes. Keep server JDK upgrades and virtual-thread work in main; preserve Java 8-compatible source, bytecode, APIs, and runtime dependencies in the client line unless a separate client baseline change is approved.
2. For a protocol change, describe old and new request/response examples, field types/defaults, authentication, capability/version behavior, and registry encodings where relevant. Link counterpart issues/PRs and obtain review from both producer and receiver areas before merging a breaking change.
3. Prefer additive optional fields with compatible defaults. Renaming/removing fields or endpoints, changing required fields/types or enum meanings, changing success/error behavior, or changing registry identity/path/serialization requires an explicit protocol migration, coexistence period, and rollback plan. Preserve legacy headerless HTTP v1 clients during that migration.
4. Release Admin support before enabling a new client capability. Record the minimum compatible Admin version and required capabilities. Client Maven versions and server Maven versions are independent; matching version numbers do not prove compatibility. Document behavior when a required endpoint or capability is absent, including old Admins that advertise no capabilities.
5. Validate an old client against the candidate Admin and the candidate client against the oldest Admin claimed as compatible. Cover registration/authentication, API docs, heartbeat/offline, discovery, and MCP as applicable. Run the split client on its supported JDK/framework combinations and server checks on the server baseline. For discovery, exercise an actual provider and its watch/removal behavior.
6. Run the main [registration compatibility fixtures](../shenyu-admin/src/test/java/org/apache/shenyu/admin/controller/RegistrationProtocolCompatibilityTest.java), and use serialized payload fixtures on both sides so compatibility does not depend on shared Java classes. Controller mocks establish receiver behavior only; separately verify persistence, health handling, Admin-to-gateway sync, and gateway routing for end-to-end claims.
7. Before removing an in-tree module, check production, test, example, starter, distribution, and dependency-management references. Preserve Admin DTO/config/MCP consumers and bootstrap heartbeat integration. Document artifact relocation/deprecation and ensure consumers can build against released client artifacts without the other repository's checkout.
8. Update this inventory, contract documentation, release compatibility evidence, and release notes in the corresponding change. Keep counterpart PRs linked so future migrations and fixes have an explicit owner and compatibility record.
