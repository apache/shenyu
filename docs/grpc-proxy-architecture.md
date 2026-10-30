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

# Native gRPC Proxy (`grpc-proxy`) — Architecture Design (Milestone M0)

Status: **design note for community review**, produced for issue #8184. This document delivers
the first milestone of that issue (architecture and design) only. It contains **no implementation**
and changes no runtime behavior. Protocol-core, resource-safety, governance, Admin/starter
integration and test work are separately scoped milestones described in section 15.

All code references were verified at `master` commit `a87a3613f`.

## 1. Scope and non-goals

### 1.1 In scope for this note

1. Why the existing HTTP/JSON-to-gRPC bridge cannot be extended into a native proxy, grounded in
   current code rather than assertion (section 2).
2. The naming and compatibility boundary between the new `grpc-proxy` and the existing `grpc`
   runtime identifier (section 4).
3. Module layout, dependency direction and every registration point a new proxy plugin must touch
   (section 5).
4. The dedicated native listener architecture, including which configuration changes may restart
   the listener and which must not (sections 6 and 8).
5. The per-call context and lifecycle adapter contract that replaces ThreadLocal-based gRPC
   `Context` propagation and `CompletableFuture` bridging (section 7).
6. Upstream channel pooling and its relationship to the lease/retirement model proposed in #8207
   (section 9).
7. Streaming governance boundaries, trust boundary, status/trailer mapping and routing granularity
   (sections 10 to 13).
8. Verification strategy and an explicit mapping from the ten acceptance criteria of #8184 to
   design sections and delivery milestones (sections 14 and 15).

### 1.2 Non-goals

- **No implementation.** No class, module, `PluginEnum` entry, database row or starter is added by
  this note.
- **No rename of the existing bridge.** The runtime/plugin identifier `grpc`, its enum order, its
  registration RPC type, its starter coordinates and its selector/rule/metadata bindings are
  unchanged. `grpc-http` is a documentation and dashboard presentation name only (section 4).
- **No canonical/artifact rename and no implicit database migration.** #8184 defers that to a
  separately agreed follow-up which must inventory aliases, starter coexistence, import/export and
  existing installations.
- **No gRPC-Web.** gRPC-Web is a distinct browser-facing wire protocol, not a synonym for
  HTTP/JSON transcoding. It needs its own protocol implementation and tests.
- **No descriptor-aware features.** Field validation, redaction and transformation require
  Protobuf descriptors or generated classes and are later work. Basic forwarding and metadata
  governance must work without per-service generated code.
- **No shared HTTP/gRPC listener.** Exploring a single listener that serves both HTTP routes and
  native gRPC is deferred until trailer and flow-control behavior is proven on the dedicated
  listener.
- **No reflection or wildcard method registration.** The initial registry is exact-method and
  fails closed. Exposing reflection requires an explicit access policy, not a default.
- **No uploaded Java `ServerInterceptor` implementations.** Extension points are ShenYu-owned and
  configuration-driven.

## 2. Grounding in the current runtime

The existing bridge is a protocol converter, not a proxy. Six concrete breakpoints make it
unsuitable as a base for native forwarding. Each is verified in current source.

### 2.1 `GrpcPlugin.doExecute` is bound to `ServerWebExchange`

`shenyu-plugin/shenyu-plugin-proxy/shenyu-plugin-rpc/shenyu-plugin-grpc/src/main/java/org/apache/shenyu/plugin/grpc/GrpcPlugin.java`

The method signature is `doExecute(ServerWebExchange, ShenyuPluginChain, SelectorData, RuleData)`
and every branch reads or writes the exchange: `exchange.getAttribute(Constants.PARAM_TRANSFORM)`,
`exchange.getAttribute(Constants.CONTEXT)`, `exchange.getAttribute(Constants.META_DATA)`,
`exchange.getResponse().setStatusCode(...)`, `exchange.getAttributes().put(Constants.RPC_RESULT, ...)`,
`exchange.getAttributes().put(Constants.CLIENT_RESPONSE_RESULT_TYPE, ...)` and finally
`.then(chain.execute(exchange))`.

A native gRPC call has no HTTP exchange to carry it. #8184 explicitly forbids fabricating one
merely to run the existing chain, so the native path must not reuse this method or the
`RPC_RESULT`/JSON response-writer path it feeds.

### 2.2 Errors are emitted as JSON bodies

The same method answers every local rejection with `ShenyuResultWrap.error(exchange, ShenyuResultEnum...)`
plus `WebFluxResultUtils.result(exchange, error)` — for `META_DATA_ERROR`, `GRPC_HAVE_BODY_PARAM`
and `GRPC_CLIENT_NULL`. A native gRPC response carries `grpc-status`, `grpc-message` and optional
`grpc-status-details-bin` trailers; substituting a JSON error body into it violates acceptance
criterion 5.

### 2.3 The response is aggregated before it is published

`proto/MessageWriter.java` implements `StreamObserver<T>` whose `onNext` appends each message to
`grpcResponse.getResults()` (a list) and whose `onCompleted` terminates the aggregate; overflow is
signalled through `triggerOverflow()`. `GrpcPlugin` then waits for the whole call:
`CompletableFuture<ShenyuGrpcResponse> result = client.call(...)` followed by
`Mono.fromFuture(result.thenApply(...))`.

Consequence: a server-streaming client cannot receive the first message before the server
completes the RPC, and a bidirectional client cannot exchange messages before request half-close.
This is the structural reason acceptance criteria 1 and 2 are unattainable on the bridge, and it is
not fixable by adding a switch branch.

### 2.4 Cancellation does not cross the future bridge

`Mono.fromFuture(result.thenApply(...))` creates a *dependent* future. Cancelling the reactive
subscription cancels the dependent future, which does not cancel `result`; the callback in
`ShenyuGrpcClient.call()` that invokes `ClientCall.cancel` is attached to `result`. #8150 verified
this at plugin boundary: cancelling the original client future invokes `ClientCall.cancel` once,
cancelling the `GrpcPlugin` subscription invokes it zero times.

Design consequence: the native runtime must not bridge through `CompletableFuture.thenApply`. It
must own a per-call handle whose cancellation reaches both legs directly (sections 7.4 and 10.4).

### 2.5 Identity and routing travel through gRPC `Context` ThreadLocals

`doExecute` performs `Context.current().withValue(GrpcConstants.GRPC_SELECTOR_ID, selector.getId()).attach()`
(and likewise for `GRPC_RULE_ID`, `GRPC_REMOTE_ADDRESS`, `RPC_CONTEXT_KEY`), then
`Context.current().detach(Context.ROOT)`. This assumes thread affinity between assembly and
execution. #8184 requires a per-call context with no thread-affinity or ThreadLocal propagation
assumption, because native forwarding crosses two independently scheduled transport legs and
potentially blocking authentication must not run on transport event loops.

### 2.6 The upstream wire contract is a renamed JSON-envelope service

`shenyu-protocol/shenyu-protocol-grpc` contains only `message/JsonMessage.java` and
`constant/GrpcConstants.java`. `JsonMessage` builds a dynamic descriptor
(`buildJsonMarshallerDescriptor`, `GrpcConstants.JSON_DESCRIPTOR_PROTO_NAME` /
`JSON_DESCRIPTOR_PROTO_FIELD_NAME`) whose single field carries the JSON payload as a string, and the
method descriptor appends `JSON` to the service name. The bridge therefore invokes
`demo.ServiceJSON/Watch` while a native client invokes `demo.Service/Watch`.

#8207 independently confirms this contract at a later commit and states that the existing runtime
identifier, the JSON-envelope upstream adapter and the aggregated HTTP response contract must be
preserved. The native proxy must consequently work against an ordinary remote gRPC service
**without** the ShenYu client SDK, without `ServiceJSON` export, without a renamed service and
without generated business classes inside the gateway. The existing marshaller and client must not
be reused unchanged.

### 2.7 What *is* reusable

Not everything must be rebuilt. The following are protocol-agnostic and should be reused or
adapted rather than duplicated:

| Asset | Location | Reuse in the native proxy |
| --- | --- | --- |
| Configuration models | `shenyu-common` `PluginData`, `SelectorData`, `RuleData`, `MetaData` | Reused unchanged as sync input; compiled into a native snapshot (section 8) |
| Data sync channels | `shenyu-sync-data-center/*` | Reused unchanged; the native listener is a consumer, not a new channel |
| Plugin lifecycle SPI | `shenyu-plugin-base` `PluginDataHandler` (six callbacks: `handlerPlugin`, `removePlugin`, `handlerSelector`, `removeSelector`, `handlerRule`, `removeRule`) | Implemented by the new plugin to drive snapshot compilation and listener lifecycle |
| Protocol-server pattern | `shenyu-protocol-mqtt` `BootstrapServer` (`init`/`start`/`shutdown`) driven by `MqttPluginDataHandler`; `shenyu-plugin-tcp` `TcpBootstrapFactory` | Followed for listener ownership, extended with drain semantics (section 6.3) |
| Namespace and discovery | Admin namespace configuration, discovery/upstream health | Reused for upstream candidates and namespace ownership |
| Load-balancer SPI | `shenyu-loadbalancer` via `shenyu-spi` (`@SPI`/`@Join`) | Reused for per-RPC-stream upstream selection; the bridge's private `loadbalance/picker` tree is **not** reused (section 13) |

## 3. Terminology

| Term | Meaning in this note |
| --- | --- |
| **bridge** | The existing HTTP/JSON-to-gRPC plugin, runtime identifier `grpc`. Converts an HTTP/JSON request into a JSON-envelope gRPC call and aggregates the response. |
| **native proxy** | The new `grpc-proxy` plugin. Accepts native gRPC over HTTP/2 and forwards the original full method name and original message payload to an ordinary remote gRPC service. |
| **leg** | One of the two independent transport negotiations of a proxied call: the downstream leg (client ↔ gateway) and the upstream leg (gateway ↔ backend). |
| **call** | A single RPC stream, identified by a gateway-generated call id, spanning both legs from headers to terminal status. |
| **per-call context** | The immutable-plus-mutable state object owned by one call: matched snapshot, selected upstream, identity, policy decisions, deadline budget, counters and terminal flag. Never a ThreadLocal. |
| **snapshot** | An immutable, atomically published routing/policy view compiled from validated configuration. A call binds to exactly one snapshot for its whole life. |
| **local reject** | A gateway-originated termination before or during forwarding (authentication failure, authorization denial, admission rejection, message limit, deadline exhausted at the gateway). Must map to a gRPC status, never to a JSON body. |
| **restart-class configuration** | Listener transport settings: port, TLS/ALPN, cleartext policy, thread/executor sizing, message-size transport limits. Changing these restarts the listener with drain. |
| **snapshot-class configuration** | Selectors, rules, method registry, upstream candidates, governance policies, limits. Changing these publishes a new snapshot and never restarts the listener. |
| **probe** | A half-open recovery attempt. Not applicable to the native listener itself, but upstream health and channel retirement interact with the circuit breaker in `shenyu-plugin-base` (section 9.3). |

## 4. Naming and compatibility boundary

### 4.1 Decision

| Surface | Existing bridge | New native proxy |
| --- | --- | --- |
| Documentation / dashboard display name | `grpc-http` | `grpc-proxy` |
| Runtime / plugin identifier | `grpc` (**unchanged**) | `grpcProxy` (new) |
| `PluginEnum` entry | `GRPC(310, 0, "grpc")` (**unchanged**) | New entry, order chosen in section 5.3 |
| Starter coordinate | `shenyu-spring-boot-starter-plugin-grpc` (**unchanged**) | `shenyu-spring-boot-starter-plugin-grpc-proxy` (new) |
| Selector / rule / metadata bindings | Existing rows and semantics (**unchanged**) | Own rows keyed by the new plugin name |
| Upstream wire contract | JSON-envelope, service name suffixed with `JSON` (**unchanged**) | Original native service/method, original Protobuf payload |

The display name `grpc-http` is applied to documentation and dashboard presentation only. It must
not silently repurpose `grpc` to mean native proxying, and it must not require an existing
installation to migrate data.

### 4.2 Coexistence rules

1. Both plugins may be installed simultaneously. Each has its own listener (the bridge has no
   dedicated listener; it runs inside the WebFlux request path), its own selectors/rules and its
   own metadata bindings.
2. Native gRPC traffic must never be processed by JSON conversion, HTTP body rewriting or JSON
   error writers. The native path does not enter `ShenyuWebHandler`'s exchange-based chain, so this
   is structurally guaranteed rather than convention-based (section 6.2).
3. Installing the native proxy must not change the bridge's wire contract, identifier, order or
   starter behavior. Acceptance criterion 9 requires this explicitly.
4. Activation is independent: enabling `grpc-proxy` does not enable or disable `grpc`.
5. A canonical/artifact rename of the bridge is a separately scoped follow-up. It is **not** a
   prerequisite for this feature and must not appear here as an implicit database migration.

### 4.3 What must be inventoried before any future rename

Recorded so the decision is not lost: all aliases of the `grpc` identifier across Admin UI, `PluginEnum`,
database plugin rows, starter artifact ids, client SDK RPC type registration, import/export
packages, `docs/` and the website, plus the coexistence rule for a gateway that has both the old
and new starter on its classpath.

## 5. Module layout and registration points

### 5.1 New and touched modules

| Module | Status | Responsibility |
| --- | --- | --- |
| `shenyu-protocol/shenyu-protocol-grpc-native` | **new** | Native listener: grpc-java `Server`, transport/executor ownership, drain, TLS/ALPN and cleartext configuration. Deliberately separate from `shenyu-protocol-grpc`, which holds the JSON-envelope marshaller of the bridge. |
| `shenyu-plugin/shenyu-plugin-proxy/shenyu-plugin-rpc/shenyu-plugin-grpc-proxy` | **new** | Per-call forwarding runtime (`ServerCall` ↔ `ClientCall`), per-call context, interceptor lifecycle, snapshot compilation, upstream channel pool, `PluginDataHandler`, audit. |
| `shenyu-spring-boot-starter/shenyu-spring-boot-starter-plugin/shenyu-spring-boot-starter-plugin-grpc-proxy` | **new** | Auto-configuration and properties, mirroring `...-plugin-mqtt` / `...-plugin-grpc`. |
| `shenyu-common` | touched | `PluginEnum` entry only. No change to `PluginData`/`SelectorData`/`RuleData`/`MetaData` shapes, because those are shared with Admin and the gateway and any change ripples to both sides of every sync channel. |
| `shenyu-admin` | touched | Plugin registration rows for the new plugin name; no new API entity (#8184 forbids making one a prerequisite). |
| `db/init/mysql/schema.sql`, `db/init/oracle/schema.sql`, `db/init/pg/create-table.sql` | touched | New `plugin` row for `grpcProxy` alongside the existing `grpc` row (`id` `15`, `role` `Proxy`, `sort` `310`; for comparison `mqtt` is `id` `28`, `sort` `125`). Note that the pg insert has a different column order from mysql/oracle. Existing rows untouched. |
| `shenyu-admin/src/main/resources/sql-script/h2/schema.sql` | touched | The H2 fresh-install schema lives here, **not** under `db/init`. Same insert-only rule. |
| `db/upgrade` | touched | Insert scripts for existing installations across the dialect suffixes present there (`-h2`, `-mysql`, `-ob`, `-og`, `-oracle`, `-pg`), idempotent, must not modify or rename the `grpc` row. OceanBase MySQL mode and openGauss have upgrade scripts but no separate `db/init` tree, so their fresh-install path follows mysql/pg respectively. |
| `shenyu-integrated-test`, `shenyu-e2e`, `shenyu-examples` | touched (later milestones) | Real-transport suites and a native gRPC example service. |

### 5.2 Dependency direction

`shenyu-plugin-grpc-proxy` depends on `shenyu-protocol-grpc-native`, `shenyu-plugin-base`
(for `PluginDataHandler` and cached configuration), `shenyu-common` and `shenyu-loadbalancer`.
`shenyu-protocol-grpc-native` must **not** depend on `shenyu-plugin-*`, so that the listener stays
testable without the plugin chain — the same separation `shenyu-protocol-mqtt` maintains.

The bridge's `shenyu-plugin-grpc` must not gain a dependency on the new modules, and the new
modules must not depend on `shenyu-plugin-grpc` or `shenyu-protocol-grpc`. Sharing code between
them would recreate the JSON-envelope coupling that section 2.6 rules out.

### 5.3 `PluginEnum` order

Existing relevant orders: `MQTT(125, 0, "mqtt")`, `SPRING_CLOUD(200, 0, "springCloud")`,
`DUBBO(310, 0, "dubbo")`, `GRPC(310, 0, "grpc")`, `TCP(320, 0, "tcp")`, and
`PluginEnum.rpcPlugin()` returns `DIVIDE`, `GRPC`, `TARS`, `SPRING_CLOUD`, `DUBBO`.

Decision: the native proxy takes an order adjacent to the other RPC proxies but must not collide
with `310` in a way that makes chain ordering ambiguous **if it ever runs inside the exchange
chain**. Since the native path does not enter that chain (section 6.2), the order primarily matters
for Admin listing and for `rpcPlugin()` membership. Proposal: a distinct order value, and
`rpcPlugin()` includes the new name only if a consumer of that list is proven to need it — otherwise
it stays out, to avoid implying exchange-chain participation. This is recorded as an open question
in section 17.

### 5.4 Registration checklist for the implementation milestones

`PluginEnum` entry → init schema for **all six dialects** (mysql, oracle, pg under `db/init`, plus
h2 under `shenyu-admin/src/main/resources/sql-script/h2`) → `db/upgrade` idempotent inserts for
`h2`/`mysql`/`ob`/`og`/`oracle`/`pg` → Admin plugin row and dashboard display → starter module and
auto-configuration import → `PluginDataHandler` registered under the new plugin name →
`shenyu-dist` packaging decision (the gateway activates a plugin by having its starter on the
classpath) → e2e plugin enum in `shenyu-e2e/shenyu-e2e-common` `Plugin` if the suites need to
toggle it.

**CI gate**: per `db/upgrade/native-sql-matrix.md`, the `native-sql-matrix` workflow (aggregate
check `sql-matrix`) is required for SQL/schema or Admin changes. It runs the offline harness
regression tests, the real H2 mapper/service/controller list, detail/export and index tests, and
five native database jobs (MySQL 8.0, OceanBase MySQL mode 4.3.5, PostgreSQL 15, openGauss lite
5.0.1, Oracle Free 23.26.3), each executing **both** the upgrade flow from the pinned `v2.7.1`
schema and the fresh-install flow. Skipped applicable jobs fail the gate. Milestone M4 must budget
for this: a plugin row added to only some dialects, or an upgrade script that is not idempotent,
fails the matrix rather than merely looking inconsistent.

## 6. Listener architecture

### 6.1 Decision: dedicated grpc-java listener

Use a dedicated grpc-java `Server` with its own executor resources, bound to its own port, and a
native per-call forwarding runtime built on grpc-java `ServerCall`/`ClientCall` primitives.

Rationale: grpc-java already implements HTTP/2 framing, length-prefixed message framing, initial
metadata, trailers, `grpc-status`/`grpc-message`/`grpc-status-details-bin` propagation, half-close,
cancellation, compression negotiation and flow control. Reimplementing any of that on raw Netty
would put acceptance criteria 2, 3, 4 and 5 at risk. #8184 mandates this choice ("Reuse grpc-java's
supported HTTP/2 transport and call primitives instead of implementing message framing, trailers
and stream state from scratch").

### 6.2 Relationship to the WebFlux request path

The native listener terminates gRPC outside `ShenyuWebHandler`. There is no `ServerWebExchange`, no
`ShenyuPluginChain`, no `RPC_RESULT` attribute and no JSON response writer. This is a deliberate
separation, not an omission:

- it structurally prevents JSON conversion or body rewriting from touching native traffic
  (section 4.2 rule 2);
- it removes the `CompletableFuture` bridge that breaks cancellation (section 2.4);
- it allows per-call scheduling without WebFlux backpressure adapters sitting between the two legs.

Policy services that are already protocol-agnostic (selector/rule matching against cached
configuration, load-balancer SPI, namespace resolution, discovery/health) are reused through
explicit adaptation, not by running the exchange chain. Anything that reads or writes
`ServerWebExchange` is out of scope for the native path and must not be adapted by faking an
exchange.

Consequence for HTTP-only plugins: they are **not** automatically compatible with native gRPC.
Each governance capability is implemented natively (section 7.3); claiming that an existing HTTP
plugin "also works" for native calls is out of scope.

### 6.3 Lifecycle: extend the `BootstrapServer` pattern with drain

`MqttPluginDataHandler` is the reference integration: it owns a `MqttBootstrapServer`, keeps a
`runningConfiguration`, and on each `handlerPlugin` either shuts down (plugin disabled), no-ops
(`configuration.hasSameRuntimeSettings(runningConfiguration) && server.isRunning()`), or
shutdown → `init()` → `start()` → record configuration, all inside a `synchronized` method.
`MqttBootstrapServer.shutdown()` closes the bound channel and calls `shutdownGracefully()` on both
event loop groups; `TcpBootstrapFactory.createBootstrapServer` follows the same
create-then-start-then-cleanup-on-failure shape.

That pattern is correct for listener ownership but insufficient for native gRPC in one respect:
`shutdown()` is immediate, so a configuration change would sever long-lived streams. #8184 requires
graceful drain and policy-driven retirement. The native handler therefore splits configuration into
two classes and treats them differently.

| Configuration class | Examples | On change |
| --- | --- | --- |
| **restart-class** | port, TLS/ALPN material, cleartext policy, executor/thread sizing, transport-level message size limits | Start a new listener, stop accepting on the old one, drain established calls to a deadline, then release old resources. Never a bare `shutdown()`. |
| **snapshot-class** | selectors, rules, method registry, upstream candidates, governance policies, rate/concurrency limits | Compile a new immutable snapshot and publish it atomically. The listener is untouched and no active call is disturbed (section 8). |

A no-op guard equivalent to `hasSameRuntimeSettings` is required for restart-class settings so that
an unrelated selector edit cannot restart the listener. This directly serves acceptance criterion 8
("configuration reloads do not reset outstanding concurrency accounting or replay active streams")
and the requirement that route updates must not silently replay calls.

Drain policy: on restart-class change or on plugin disable, the listener stops accepting new calls
immediately, existing calls continue against their bound snapshot and upstream, and a configurable
drain deadline applies. Two explicit behaviors must be configurable because they conflict:
ordinary drain waits for the deadline, while **immediate security revocation** (for example a
compromised upstream credential) must be able to terminate affected calls at once. Removal and
disable events must reach listener, catalog and channel cleanup even when their configuration
payload is null — `PluginDataHandler.removePlugin`/`removeSelector`/`removeRule` are the delivery
points, and the implementation must not early-return on a null config.

Failure cleanup follows the MQTT/TCP precedent: if `start()` fails after resources are allocated,
release them in a `finally` block so a later retry can bind again.

### 6.4 Transport, TLS and coexistence

The listener must declare supported TLS/ALPN modes and explicitly configured cleartext HTTP/2
(h2c) modes, with port configuration independent of the WebFlux port so that both can run in one
process. Coexistence with existing HTTP routes and plugins is therefore a port-and-lifecycle
question, not a request-routing question. Compression support and the distinction between
wire-byte limits and decompressed-message limits are specified in section 10.3.

## 7. Per-call context and lifecycle adapters

### 7.1 Call model

One proxied RPC produces one call object owning:

- a gateway-generated call id (bounded cardinality; never derived from caller metadata);
- the canonical full method name `/package.Service/Method` and the method type from the
  synchronized method registry;
- the bound snapshot, selected upstream and identity/policy context;
- the downstream `ServerCall` and the upstream `ClientCall`;
- the deadline budget, counters (messages and bytes per direction, active duration) and the
  terminal flag.

Forwarding preserves the original full method name and the original message payload. A bounded
per-message byte representation is acceptable; buffering an entire RPC stream is not. The design
must not promise zero-copy or identical compressed wire bytes, because the two legs negotiate
compression independently.

### 7.2 Interception lifecycle

```text
call start -> authenticate -> authorize -> route/admit
  -> request headers -> request messages -> request half-close
  -> response headers -> response messages -> final status/trailers
  -> terminal audit and resource release

cancellation / deadline / disconnect / error
  -> terminate both legs as appropriate -> release resources exactly once
```

For bidirectional RPCs request and response events interleave. The diagram is a lifecycle order for
terminal accounting, **not** a requirement to buffer one direction until the other finishes.

### 7.3 Capability contract

Extension points are ShenYu-owned, configuration-driven and executed against the per-call context.
Ordering, asynchronous execution, cancellation and error behavior are part of the contract and must
be specified for each point. Potentially blocking authentication or inspection must not block
transport event loops — it runs on a bounded dedicated executor with an explicit rejection policy.

| Capability | Required behavior | Native mapping |
| --- | --- | --- |
| Authentication | Documented initial mechanism using credentials in metadata or a verified client identity; extensible | Before routing; failure terminates with `UNAUTHENTICATED` and never reaches the upstream |
| Service/method authorization | Authorize the authenticated principal for `/package.Service/Method` and the relevant tenant scope | Before forwarding; failure terminates with `PERMISSION_DENIED` |
| Metadata policies | Validate/allowlist metadata, strip untrusted reserved identity fields, inject trusted caller/tenant context | Applied to downstream headers before upstream headers are built |
| Request id and tracing | Validate or generate correlation ids; propagate trace context across both legs | Call id is gateway-generated; inbound ids are validated, not trusted blindly |
| Admission and limits | Call-rate and active-RPC/stream concurrency limits, bounded queuing, explicit overload outcomes | `RESOURCE_EXHAUSTED` with optional retry information; accounting survives reloads (section 8.3) |
| Deadline and cancellation | Cap calls by the remaining deadline budget; propagate cancellation, reset and disconnect upstream; do not restart the timeout budget per hop | Upstream deadline is derived from the remaining downstream budget |
| Status and errors | Preserve upstream outcomes; map gateway-side rejections and timeouts to suitable gRPC statuses | Section 12 |
| Audit and observability | Record service/method, routing decision, duration, final status, stream/message/byte statistics and terminal reason; no raw credentials or business bodies by default | Terminal hook, exactly once |

The dedicated runtime may implement built-in policies through native `ServerInterceptor` and
call-listener adapters. The extension contract remains owned and tested by ShenYu; arbitrary
uploaded interceptors and unchanged execution of HTTP-only plugins are out of scope.

### 7.4 Terminal accounting: exactly once

Every termination path — response complete, upstream error, downstream cancel, upstream reset,
deadline expiry, write failure, drain deadline, listener shutdown — must run terminal cleanup
exactly once, releasing the call, retained buffers and concurrency permits. Implementation rule:
a single atomic terminal flag per call guards cleanup, and no path may release permits by relying
on a `CompletableFuture` dependent-stage cancellation (the mechanism proven broken in section 2.4).
Acceptance criterion 4 is a test obligation, not an aspiration, so the terminal paths are
enumerated in the test strategy (section 14.3).

## 8. Immutable configuration snapshot

### 8.1 Why the current cache pattern is insufficient here

The bridge's `GrpcPluginDataHandler` mutates several independent structures per event:
`Singleton.INST` for `GrpcRegisterConfig`, `ApplicationConfigCache.cacheRuleHandle` /
`invalidate` / `removeRuleHandle`, and `GrpcClientCache.initGrpcClient(selectorId)`. Each callback
lands separately, so an in-flight call can observe a rule from one generation and a client from
another. #8207 further records that `initGrpcClient` evaluates `GrpcClientBuilder.buildClient(selectorId)`
before `putIfAbsent`, so repeated notifications can build an uncached candidate even when a cached
client exists.

For native proxying, where a call may live for minutes and cross two transport legs, #8184 requires
a stronger guarantee: compile each validated update into an immutable snapshot, publish it
atomically, and bind each admitted call to one snapshot for its whole life.

### 8.2 Snapshot contract

1. **Compile then publish.** All six `PluginDataHandler` callbacks feed a builder; a validated
   update produces a new immutable snapshot object; publication is a single atomic reference swap.
   Partial mutation of a live snapshot is prohibited.
2. **Bind at admission.** An admitted call captures the snapshot reference, the selected upstream,
   the identity and the policy context. It must not mix an old routing decision with new
   credentials or new limits during the call.
3. **Deterministic matching.** Matching is against the canonical gRPC method path with consistently
   normalized metadata. Method type and permission come from the synchronized exact-method
   registry; unknown or unconfigured methods fail closed with an appropriate native status. Neither
   may be inferred from untrusted caller metadata.
4. **No path-only caching of identity-dependent decisions.** Any matching cache must include every
   relevant scope and condition, or be disabled for identity/metadata-dependent decisions. A
   path-only cache cannot cache arbitrary caller-dependent authorization.
5. **Namespace selection is trusted.** Namespace comes from listener/deployment/routing
   configuration, never from arbitrary caller metadata.

### 8.3 Reload-safe admission accounting

Outstanding concurrency permits must remain accounted for across reloads, including an update whose
configuration is semantically unchanged. Swapping the snapshot holder must not make existing
executions disappear from the process-wide budget. Implementation rule: permit accounting lives in
a lifecycle object that outlives snapshots (owned by the listener/runtime, not by a snapshot
instance), and each snapshot publishes limits, not counters. Acceptance criterion 8 and the
requirement that removing an upstream drains existing calls by policy while stopping new calls from
selecting it both depend on this separation.

### 8.4 Removal and disable

`removePlugin`, `removeSelector` and `removeRule` must reach listener, catalog and channel cleanup
even when the configuration payload is null. Removal publishes a snapshot without the removed
scope; it does not terminate bound calls abruptly except where an explicit immediate
security-revocation policy says so (section 6.3).

## 9. Upstream channel pool

### 9.1 Requirements

Channels are pooled with bounded size and explicit lifecycle. Reuse identity must account for
endpoint, authority and transport/credential identity: a credential or route reload must not
accidentally reuse another scope's security context. Old channels are retired by draining
established calls according to policy.

### 9.2 Relationship to #8207

#8207 gives selector-owned `ShenyuGrpcClient`/`ManagedChannel` resources explicit creation,
admission, retirement and terminal cleanup ownership, as the first concrete delivery of the #8061
resource-lease and #8062 bounded-retirement work. The native proxy needs the same four ownership
verbs for its own channels.

Decision: the native channel pool should build on the lease/retirement model that #8207 establishes
rather than inventing a parallel one, because two independent channel-lifecycle implementations in
the same plugin family would diverge on drain deadlines, permit accounting and failure cleanup.
Sequencing consequence: the resource-safety milestone (M2) should start after #8207's API shape is
settled, or explicitly accept a temporary private implementation with a recorded migration task.
This is raised on #8184 and #8207 rather than decided unilaterally here.

### 9.3 Circuit breaker interaction

Upstream failure accounting for native calls must reuse `shenyu-plugin-base`'s
`UpstreamCircuitBreaker` rather than adding a second breaker.

Status at the verified commit `a87a3613f`: `UpstreamCircuitBreaker` records the `ON_ERROR` terminal
signal without distinguishing a gateway-local rejection from a genuine upstream failure, and
contains no local-reject classification at all. PR #8189 (pending, not merged at that commit)
introduces exactly that classification for the HTTP path, and its review established two
constraints that the native proxy inherits:

1. **Classify by an explicit marker type, never by HTTP status.** #8189's first attempt treated
every 4xx `ResponseStatusException` as a client error, which silently stopped counting 408 — the
status that `DefaultRetryStrategy` produces when upstream-timeout retries are exhausted, and which
`AbstractHttpClientPlugin.mapUpstreamTimeout` deliberately leaves unrewritten. A permanently
timing-out upstream would then never open the breaker. The merged shape recognizes only an explicit
`ShenyuLocalRejectException` marker.
2. **Half-open probe accounting must treat a gateway-local rejection as an invalid probe**, not as a
failed probe, so that a locally rejected probe does not consume the single probe slot and re-open
the breaker for a full wait window.

A native call has no HTTP status at the classification point, so constraint 1 maps cleanly: local
rejects carry the marker, upstream gRPC statuses do not. The native runtime must therefore emit the
same marker type (or an equivalent one shared through `shenyu-plugin-base`) for its local rejects,
and must not invent a status-code-based classification of its own. If #8189 has not merged by the
time M2 starts, M2 must either wait for it or land the marker itself — recorded as an open
question in section 17.

Routing and load balancing are per-RPC-stream decisions (section 13), and `isRequestAllowed` must be
consulted per call, not per connection.

## 10. Streaming governance boundaries

### 10.1 Streams, not just QPS

Long-lived active streams are accounted for separately from call rate. A gateway can be well within
its QPS budget and still be exhausted by concurrent long-lived streams, so both limits exist and
both have explicit overload outcomes.

### 10.2 Timeouts

Three distinct budgets, not one inherited unary timeout:

| Budget | Meaning |
| --- | --- |
| Maximum call duration | Hard cap on a single RPC regardless of message activity |
| Message idle timeout | Cap on the gap between messages in either direction |
| Deadline budget | The remaining downstream deadline, propagated upstream without restarting per hop |

### 10.3 Size limits and compression

Wire-byte limits and decompressed-message limits are distinguished. A decompressed-size bound must
not be claimed without implementing bounded decompression. Split and coalesced message frames are
handled consistently with the configured limits, and supported compression is declared rather than
assumed, because the two legs negotiate independently (section 7.1).

### 10.4 Flow control and bounded demand

Slow consumption on one leg must translate into bounded demand on the other, using grpc-java flow
control across downstream and upstream rather than only adding an application queue. Buffers and
prefetch are bounded, and retained buffers are released on every termination path (section 7.4).
Acceptance criterion 3 requires slow-reader and slow-writer tests over a real HTTP/2 transport, not
only mocked observers.

### 10.5 No transparent stream retry or failover

A stream must not be transparently retried or failed over after messages may have been processed or
emitted. Any limited unary retry support requires explicit replay-safety rules. This mirrors the
constraint recorded for AI retries in the HTTP path and is a hard boundary for the native proxy:
once a message has been emitted downstream or accepted upstream, the call is not replayable.

### 10.6 Metric cardinality

Labels must not take unbounded-cardinality values: no request ids, no arbitrary metadata keys, no
raw caller or tenant identifiers. Service and method come from the registry and are bounded by
configuration.

## 11. Trust boundary and identity

Backend services must not trust arbitrary caller-supplied `user-id` / `tenant-id` metadata. The
gateway strips or replaces reserved identity fields and establishes a verifiable gateway-to-backend
trust mechanism. Application-specific object authorization remains the backend's responsibility;
gateway method authorization does not replace it (acceptance criterion 6).

Namespace selection is never driven by caller metadata (section 8.2 rule 5), and the call id used
for correlation and audit is gateway-generated (section 7.1) so that a caller cannot influence
label cardinality or collide with another tenant's audit records.

## 12. Status, error and trailer mapping

1. Preserve initial metadata, response headers, final trailers, binary metadata, `grpc-status`,
   `grpc-message` and `grpc-status-details-bin` where supplied.
2. Handle trailers-only responses and errors occurring before or after the first response message.
3. **HTTP 200 alone must not be considered RPC success.** The terminal gRPC status decides.
4. Gateway-side local rejects map to native statuses (`UNAUTHENTICATED`, `PERMISSION_DENIED`,
   `RESOURCE_EXHAUSTED`, `DEADLINE_EXCEEDED`, `UNAVAILABLE` during drain, `FAILED_PRECONDITION` for
   unknown or unconfigured methods) and never to a JSON error body.
5. Upstream non-OK statuses survive forwarding unchanged in status, message and details-bin
   (acceptance criterion 5).

## 13. Routing and load-balancing granularity

Routing and load balancing are **per-RPC-stream** decisions, not per TCP connection and not per
message. Different calls on one client HTTP/2 connection may route to different upstreams, while a
single established RPC retains its selected upstream for its whole life (acceptance criterion 7).

Implementation consequence: selection happens once at admission and is stored in the per-call
context; it is never re-evaluated mid-call. The bridge's private `loadbalance/picker` tree
(`RoundRobinPicker`, `RandomPicker`, `ShenyuPicker`, `EmptyPicker`, `SubChannelCopy`,
`SubChannels`) operates on grpc-java internal subchannel state for the JSON bridge and is not
reused. The native path selects an upstream endpoint and then uses the `shenyu-loadbalancer` SPI so
that gateway-wide load-balancing policy stays consistent across protocols.

## 14. Verification strategy

### 14.1 Transport realism

Tests must run over a real HTTP/2 transport against a real remote gRPC server, not only mocked
`StreamObserver`s or a recording `ClientCall`. #8184 notes that its own cancellation probe used a
recording `ClientCall` and a mocked `ManagedChannel`, and explicitly states that this is evidence
for architecture requirements, not a claim that native transport integration passed. The same
distinction applies here: unit tests may use recording doubles for state-machine logic, but
acceptance criteria 1 to 5 and 7 require real-transport suites.

### 14.2 Suite placement

| Suite | Covers |
| --- | --- |
| Module unit tests in `shenyu-protocol-grpc-native` | Listener start/stop/drain, bind-failure cleanup, configuration class discrimination, no-op guard |
| Module unit tests in `shenyu-plugin-grpc-proxy` | Snapshot compilation and atomic publication, per-call context binding, terminal exactly-once accounting, permit conservation across reloads, status mapping, method-registry fail-closed behavior |
| `shenyu-integrated-test` (Docker) | Four RPC types end to end against a real gRPC server, incremental delivery, trailers/status/details preservation, cancellation and deadline propagation, multi-call routing independence on one connection |
| `shenyu-e2e` | Admin-configured selectors/rules driving native routing; plugin enable/disable lifecycle; coexistence with the bridge |
| `shenyu-examples` | A native gRPC example service that does **not** use the ShenYu client SDK and does **not** export a `ServiceJSON` service (acceptance criterion 9) |

### 14.3 Mandatory negative and boundary cases

- Client cancellation, deadline expiry, failed writes, upstream reset and plugin shutdown each
  release calls, buffers and permits exactly once (criterion 4).
- Slow-reader and slow-writer in both directions show bounded buffering and effective flow control
  (criterion 3).
- Trailers-only failure, error before the first response message, error after the first response
  message, binary metadata round-trip (criterion 5).
- Spoofed identity metadata is neither trusted nor forwarded as verified identity; unauthorized
  calls are rejected before backend invocation (criterion 6).
- Two concurrent RPCs on one downstream connection route independently; each established stream
  stays bound to its chosen upstream (criterion 7).
- Configuration reload preserves outstanding concurrency accounting and does not replay active
  streams; upstream removal drains; namespace isolation holds (criterion 8).
- Server-streaming client receives the first message before server completion; bidi client
  exchanges messages before request half-close (criterion 2).
- Unknown or unconfigured method fails closed (section 8.2 rule 3).
- A backend exposing only its original native service works with no SDK and no `ServiceJSON`
  export, while existing HTTP/JSON-to-gRPC behavior and legacy `grpc` identifiers stay compatible
  (criterion 9).

## 15. Acceptance mapping (#8184) and delivery plan

### 15.1 Criteria to design sections

| # | Acceptance criterion (#8184) | Design section | Delivery milestone |
| --- | --- | --- | --- |
| 1 | Unchanged generated client invokes all four RPC types against a real remote server | 6.1, 7.1, 2.6 | M1 |
| 2 | Server-streaming first message before completion; bidi exchange before half-close | 2.3, 7.2, 10.4 | M1 |
| 3 | Slow-reader/slow-writer bounded buffering and flow control over real HTTP/2 | 10.4, 14.1 | M2 |
| 4 | Cancellation, deadline, failed writes, upstream reset, shutdown release exactly once | 2.4, 7.4, 6.3 | M2 |
| 5 | Non-OK statuses, trailers-only failures, binary metadata, details-bin survive; no JSON body | 12, 2.2 | M1 |
| 6 | Unauthorized calls rejected before backend; spoofed identity not trusted | 7.3, 11 | M3 |
| 7 | Concurrent RPCs on one connection route independently; each stream stays bound | 13, 8.2 | M3 |
| 8 | Reloads preserve concurrency accounting and do not replay streams; drain and namespace regression | 8.3, 8.4, 6.3 | M2/M4 |
| 9 | Native-only backend works without SDK or `ServiceJSON`; bridge compatibility preserved | 2.6, 4.1, 4.2 | M4 |
| 10 | Documentation of native proxying vs transcoding vs gRPC-Web, transports, compression, limits | 4, 10.3, 6.4 | M6 |

### 15.2 Milestones

Each milestone is a separately reviewable PR. #8184 stays open as the umbrella until all are merged.

| Milestone | Content | Depends on | Rough size |
| --- | --- | --- | --- |
| **M0** | This design note | — | docs only |
| **M1** | Native protocol core: listener, four RPC types, incremental forwarding, metadata/trailers/status, half-close, cancellation | M0 | large |
| **M2** | Resource safety: two-way flow control, bounded buffering, deadlines/idle timeout, message limits, reload-safe admission accounting, exactly-once terminal cleanup | M1; coordinate with #8207 (section 9.2) | large |
| **M3** | Initial governance: authentication, method authorization, trusted metadata, routing/load balancing, request id/tracing, final-status auditing | M1, M2 | large |
| **M4** | Admin/starter/configuration integration: `PluginEnum`, init schema for all six dialects, `db/upgrade` inserts, `native-sql-matrix` gate, selectors/rules, namespace sync, discovery, independently configurable listener | M3 | medium |
| **M5** | Bridge naming compatibility: document and display `grpc-http` while retaining legacy `grpc` identifiers and starter/configuration behavior | independent, can run in parallel with M0/M1 | small |
| **M6** | Integration tests, examples, protocol support matrix, operational documentation | M1 to M4 | medium |

M4 is deliberately late: registering the plugin in Admin and the database before the runtime can
forward a call would expose a non-functional plugin to installations. M5 is deliberately early and
independent because it is documentation-only and unblocks the website work tracked separately in
apache/shenyu-website#1172.

### 15.3 Later work explicitly not advertised as part of opaque forwarding

Descriptor-aware field validation/redaction/transformation, message-rate and byte-rate policies,
gRPC-Web, reflection policy, Kubernetes `GRPCRoute` integration (evaluated in #8171), and a shared
HTTP/gRPC listener. If reflection is ever exposed, it requires an explicit access policy rather than
being automatically public.

## 16. Risks and rollback

| Risk | Mitigation |
| --- | --- |
| Two parallel channel-lifecycle implementations (this feature and #8207) | Section 9.2: sequence M2 after #8207's API shape, or accept a private implementation with a recorded migration task |
| Cancellation defect reproduced in new code via a future bridge | Section 2.4 and 7.4: forbid `CompletableFuture` dependent-stage cancellation as a release mechanism; single atomic terminal flag |
| Listener restart on unrelated configuration edit severs long-lived streams | Section 6.3: restart-class vs snapshot-class discrimination plus a `hasSameRuntimeSettings`-equivalent no-op guard |
| Permit leak across reloads exhausts admission permanently | Section 8.3: counters live in a lifecycle object that outlives snapshots; explicit reload regression test (criterion 8) |
| Native traffic accidentally processed by JSON conversion or body rewriting | Section 6.2: native path never enters the exchange chain; structural, not conventional |
| `db/upgrade` script damages existing `grpc` rows, or a dialect is missed | Section 5.1 and 5.4: insert-only, idempotent, all six dialects plus h2's separate location; the `native-sql-matrix` gate runs upgrade **and** fresh-install on five native engines and fails on skipped applicable jobs |
| Local rejects misclassified as upstream failures (or the reverse), so the breaker never opens or opens on gateway-side rejections | Section 9.3: marker-type classification only, shared with `shenyu-plugin-base`; no status-code-based classification in the native path |
| Unbounded metric cardinality from caller-supplied values | Section 10.6: labels only from the bounded registry; gateway-generated call id |
| Scope creep into descriptor-aware features | Section 1.2 and 15.3: recorded as later work with their own policy requirements |

Rollback: the feature is additive and activated only by having its starter on the gateway classpath.
Removing the starter removes the listener and the plugin; the bridge is untouched throughout, so
rollback needs no data migration. The `db/upgrade` inserts across the six dialects are the only
persistent artifact and are reversible by deleting the new plugin row.

## 17. Open questions for review

1. **`PluginEnum` order and `rpcPlugin()` membership** (section 5.3): does any consumer of
   `PluginEnum.rpcPlugin()` need the native proxy listed, given that it does not participate in the
   exchange chain? Listing it may imply exchange-chain semantics that do not hold.
2. **Listener port configuration model**: a single fixed port per gateway, or one listener per
   namespace/deployment? The answer constrains how namespace selection stays trusted (section 8.2
   rule 5).
3. **Channel pool ownership boundary with #8207** (section 9.2): shared lease/retirement component,
   or private implementation with a recorded migration task?
4. **Immediate security-revocation semantics** (section 6.3): which configuration classes may
   terminate established calls at once, and how is that expressed in Admin without adding a new API
   entity?
5. **Initial authentication mechanism** (section 7.3): metadata-borne credentials, verified client
   identity from TLS, or both in the first release? This determines the gateway-to-backend trust
   mechanism in section 11.
6. **Bounded decompression** (section 10.3): implement in M2, or declare decompressed-size limits
   unsupported until it exists? Declaring a bound without implementing it is explicitly disallowed
   by #8184.
7. **Whether M5 (documentation-only `grpc-http` display naming) should land before M1**, so that
   users are not surprised by a second gRPC plugin appearing later without the naming context.
8. **Local-reject marker ownership and timing** (section 9.3): if PR #8189 has not merged when M2
   starts, does the native runtime wait for it, or land the marker type in `shenyu-plugin-base`
   itself and let #8189 consume it? The marker must live in `shenyu-plugin-base` either way, since
   both the HTTP path and the native path classify through `UpstreamCircuitBreaker`.
9. **OceanBase and openGauss fresh-install path** (section 5.1): `db/init` has no `ob` or `og`
   tree, only `db/upgrade` scripts exist for them. Confirm that fresh installs for those engines are
   expected to reuse the mysql and pg init schemas respectively, so the new plugin row does not need
   a separate `ob`/`og` init file.
