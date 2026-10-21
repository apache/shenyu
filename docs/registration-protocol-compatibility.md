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

# Java client registration compatibility contract

This document addresses #6365. It describes the receiver at main commit
87c22dde7 (2.7.2-SNAPSHOT); it is a wire contract for independent implementations,
not a requirement to share Java DTO classes. Read it alongside
[java-client-repository-boundary.md](java-client-repository-boundary.md) and
[java-client-compatibility.md](java-client-compatibility.md).

## Version negotiation, authentication and acknowledgement

The canonical paths and header names are defined by
[RegistrationProtocol](../shenyu-register-center/shenyu-register-protocol/src/main/java/org/apache/shenyu/register/common/protocol/RegistrationProtocol.java).
POST JSON with Content-Type application/json. Names and path capitalization are
significant; register-apiDoc and register-discoveryConfig retain their historical case.

- A missing X-Shenyu-Register-Protocol-Version header means legacy v1.
  An explicit single value of 1 is accepted. Empty, unsupported or multiple
  version headers return HTTP 400 with "Unsupported registration protocol version".
- Responses advertise X-Shenyu-Register-Protocol-Version: 1 and
  X-Shenyu-Register-Capabilities: metadata,uri,api-doc,discovery-config,mcp,offline,heartbeat.
  These identify receiver capabilities, not a negotiated client artifact version.
- X-Shenyu-Client-Version can identify the sender. It does not override the wire
  version, authenticate the client or establish framework/JDK compatibility.
- Preserve existing Admin authentication. The HTTP client authenticates through
  POST /platform/login, extracts the returned token and uses the existing
  X-Access-Token header on subsequent calls. Credentials, expiry and unauthorized
  responses are deployment concerns; never hardcode the documentation's examples.
- Successful calls below return the plain string success. Registration controllers
  enqueue work through ShenyuClientServerRegisterPublisher before returning.
  This acknowledges submission; it does not prove database persistence, sync delivery
  or route availability. Observe those stages separately.
- HTTP success alone is insufficient: inspect the response body. Admin exception
  handling can return a JSON error envelope with HTTP 200. Retry boundedly on
  transport/transient failures; report authentication, malformed payload and version
  errors explicitly. Do not spin indefinitely on a permanent rejection.

## HTTP endpoints

| POST path | Request | Capability | Receiver behavior |
| --- | --- | --- | --- |
| /shenyu-client/register-metadata | MetaDataRegisterDTO object | metadata | Enqueue backend endpoint metadata; plugin-specific subscribers create/update selectors, rules and metadata |
| /shenyu-client/register-uri | URIRegisterDTO object | uri | Enqueue an upstream registration/update for the application's selector |
| /shenyu-client/register-apiDoc | ApiDocRegisterDTO object | api-doc | Enqueue persisted API documentation |
| /shenyu-client/register-discoveryConfig | DiscoveryConfigRegisterDTO object | discovery-config | Enqueue discovery configuration and selector association |
| /shenyu-client/register-mcp | McpToolsRegisterDTO object | mcp | Enqueue MCP tool registration with embedded endpoint metadata |
| /shenyu-client/offline | URIRegisterDTO object | offline | Enqueue upstream removal; clients must supply the offline/deletion event type used by the consumer |
| /instance/beat | InstanceBeatInfoDTO object | heartbeat | Pass the report to InstanceCheckService; return success |

The receiver is
[ShenyuClientHttpRegistryController](../shenyu-admin/src/main/java/org/apache/shenyu/admin/controller/ShenyuClientHttpRegistryController.java),
with [InstanceController](../shenyu-admin/src/main/java/org/apache/shenyu/admin/controller/InstanceController.java)
for heartbeat. There is no single HTTP endpoint for all external registry instance
providers; do not invent /register-instance.

## Field semantics and defaults

"Required" below means needed to carry out the operation, not necessarily a Jakarta
@NotNull annotation or immediate HTTP rejection. Several controllers accept a DTO
without @Valid and downstream failures can occur after acknowledgement.
Send explicit values for operational fields; a deserialization default is not a
promise that an omitted field creates a usable route.

Metadata, URI, discovery-config, MCP and offline controllers replace a null namespaceId
with the default namespace ID 649330b6-c2d7-4edc-be8e-8a54df9eb385. Empty strings
are distinct from null at the controller; downstream services can normalize further.
API-doc and heartbeat do not share that controller normalization. Always send an
explicit valid namespace for nondefault deployments, including nested MCP metadata.

### Metadata

[MetaDataRegisterDTO](../shenyu-register-center/shenyu-register-protocol/src/main/java/org/apache/shenyu/register/common/dto/MetaDataRegisterDTO.java):

| Fields | JSON type | Semantics |
| --- | --- | --- |
| appName, contextPath | string | Application/selector identity; a missing context path can fall back to appName in registration helpers |
| path, rpcType | string | Endpoint and protocol/plugin identity; required by the selected registration consumer |
| serviceName, methodName, parameterTypes, rpcExt | string | Protocol-specific invocation descriptor; required according to the RPC plugin. Keep rpcExt's embedded JSON schema compatible |
| ruleName | string | Required nonblank by the asynchronous registration consumer |
| pathDesc | string | Optional description |
| host, port | string, integer | Backend address; supply a valid host/port for consumers that also register upstreams |
| namespaceId | string | Tenant identity; null controller default as above |
| enabled, registerMetaData, addPrefixed | boolean | Endpoint enablement, metadata persistence selection and path-prefix behavior. Java no-arg defaults are false; builders may choose other values, so send these explicitly |
| pluginNames | array of strings | Optional plugin selection for registration |
| timeMillis | integer | Sender event time in milliseconds; no-arg field default is 0; builder-generated values do not establish receiver clock ordering |

Do not serialize service objects or Java class names as the contract itself.
Keep parameterTypes/rpcExt formatting stable for the selected invocation plugin.
The current consumer drops metadata events missing appName, path, ruleName,
rpcType or namespaceId; the HTTP acknowledgement can already have been sent.
URI events similarly require nonblank contextPath, rpcType and namespaceId before
dispatch, in addition to the upstream address checks in the service.

### URI registration and offline

[URIRegisterDTO](../shenyu-register-center/shenyu-register-protocol/src/main/java/org/apache/shenyu/register/common/dto/URIRegisterDTO.java):

| Fields | JSON type | Semantics |
| --- | --- | --- |
| appName, contextPath, rpcType | string | Locate the application selector and protocol/plugin |
| host, port | string, integer | Backend identity; valid nonblank host and nonnull port are needed to update an upstream |
| protocol | string | Upstream protocol, preserving the consumer's existing format |
| eventType | string enum | REGISTER, UPDATED, DELETED, IGNORED, OFFLINE, HEARTBEAT retain their exact spellings. Offline path alone does not manufacture the deletion event |
| namespaceId | string | Namespace identity |
| instanceInfo | string | Optional serialized instance information, not a substitute for host/port |

Registration can race ahead of metadata creation. Existing consumers retry when the
selector does not yet exist. Preserve bounded sender retry and idempotent receiver
identity; do not create a second selector to work around a temporarily missing route.
Graceful shutdown sends offline; node death or forced pod termination may send nothing.
Admin active upstream checks and registry expiration/watch events must remove those
unreachable destinations. Gateway request retries are not a liveness protocol.

### Heartbeat and instance lifecycle

[InstanceBeatInfoDTO](../shenyu-register-center/shenyu-register-protocol/src/main/java/org/apache/shenyu/register/common/dto/InstanceBeatInfoDTO.java):

| Field | JSON type | Meaning |
| --- | --- | --- |
| instanceIp | string | Instance address |
| instancePort | string | Port is historically encoded as a string, unlike URIRegisterDTO.port |
| instanceType | string | Preserve receiver-recognized instance type keys |
| instanceInfo | string | Optional implementation details |
| namespaceId | string | Namespace identity; send explicitly |

The beat reports liveness of an Admin-visible instance, not every route's HTTP health.
InstanceCheckService records last-heartbeat state and its checker decides when a
missing report makes an instance offline. Use the configured check interval and
timeout; there is no fixed heartbeat TTL negotiated by HTTP v1. Stop sending beats
on shutdown, deregister where supported, and verify eventual removal separately.
New clients must not label a gateway as a business backend or guess a port before
the server has bound it.

External discovery uses
[InstanceEntity](../shenyu-registry/shenyu-registry-api/src/main/java/org/apache/shenyu/registry/api/entity/InstanceEntity.java)
and ShenyuInstanceRegisterRepository. Instance fields are appName and host (strings),
port (integer), uri (provider-serialized URI), status and weight (integers). The
provider defines persisted identity, serialization, session/lease and watch behavior.
The common register root is /shenyu/register/instance; verify each provider's exact
child key and deletion semantics against its implementation before changing them.
HTTP capability headers do not version ZooKeeper/Nacos/etcd/Apollo/Eureka data.
A registry layout/serialization change needs its own coexistence and rollback plan.

### Discovery configuration

[DiscoveryConfigRegisterDTO](../shenyu-register-center/shenyu-register-protocol/src/main/java/org/apache/shenyu/register/common/dto/DiscoveryConfigRegisterDTO.java)
has string fields selectorName, name, discoveryType, serverList, listenerNode,
handler, pluginName and namespaceId, plus props (JSON object mapping property names
to values). Supply selectorName/pluginName/namespaceId to identify ownership and the
provider-required connection/listener properties. Provider secrets must be redacted
from diagnostics. Handler and props formats are provider contracts, not arbitrary
cross-provider equivalents.

### API documents

[ApiDocRegisterDTO](../shenyu-register-center/shenyu-register-protocol/src/main/java/org/apache/shenyu/register/common/dto/ApiDocRegisterDTO.java)
has contextPath, apiPath, consume, produce, version, rpcType, ext, apiOwner, apiDesc
and document (strings); httpMethod, state and apiSource (integers); eventType (enum);
and tags (array of strings). apiPath/contextPath and rpcType identify the described
endpoint. Preserve numeric method/state/source codes and the existing document/ext
format. This DTO does not contain namespaceId; adding it is a protocol change to
review with API ownership, not an assumed existing capability. An import source
label does not imply the receiver implements that importer.

### MCP registration

[McpToolsRegisterDTO](../shenyu-register-center/shenyu-register-protocol/src/main/java/org/apache/shenyu/register/common/dto/McpToolsRegisterDTO.java)
contains metaDataRegisterDTO (the metadata object above), namespaceId (string) and
mcpConfig (string containing serialized MCP configuration/tool definitions).
The outer namespace is controller-defaulted; send a consistent explicit namespace
in both the outer object and embedded metadata. Consumers require usable embedded
metadata and MCP configuration. The mcp capability indicates registration support,
not arbitrary MCP schema/protocol-version support or that the gateway plugin is enabled.
Verify tool discovery and invocation after configuration reaches bootstrap, including restart.

## Evolution and version skew

No existing field above is declared deprecated by this contract. Track a future
deprecation with the introducing/deprecating release, replacement and removal gate.

1. Add optional fields with documented omission/default behavior. Both sides must
   tolerate unknown optional fields in compatibility fixtures; this is a tested
   requirement, not an assumption about every historical JSON mapper.
2. Never silently rename/remove a field, change its JSON type, reuse an enum code,
   tighten validation of formerly valid input or change a default inside v1.
   Use a new capability/version with an explicit migration when semantics change.
3. Deploy receiver support before requiring the capability from senders. A new
   client talking to old Admin must treat missing capability headers as unknown,
   probe only documented endpoints, and fail clearly if its required feature is absent.
   Do not silently drop an essential MCP/discovery registration after a 404.
4. Old headerless clients retain v1 behavior against new Admin. Authentication,
   namespaces and feature-specific minimum versions still apply.
5. An additive field may be omitted only when the requested operation remains
   meaningful. Do not downgrade authorization or tenant identity as a compatibility fallback.
6. Reconcile idempotent register/retry/offline behavior across multiple Admins and
   transports; duplicate event delivery must not duplicate logical identities.
7. Review changes with both the Admin registration/registry area and the affected
   client/framework owners. Update serialized fixtures in both repositories, the
   compatibility matrix, release notes and rollback guidance.

## Verification

[RegistrationProtocolCompatibilityTest](../shenyu-admin/src/test/java/org/apache/shenyu/admin/controller/RegistrationProtocolCompatibilityTest.java)
and RegistrationProtocolTest exercise receiver/header and serialized DTO contracts.
They are not end-to-end client/server certification. Use the
[cross-repo test strategy](cross-repo-compatibility-testing.md) for authentication,
persistence, heartbeat, external discovery, gateway sync/proxy and MCP coverage.

