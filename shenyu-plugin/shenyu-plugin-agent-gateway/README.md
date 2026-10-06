# Agent Gateway Plugin

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

## Overview

The Agent Gateway plugin adds a request-scoped context to selected LLM traffic
and then continues the existing plugin chain. Explicitly matched MCP rules use
a bounded tools-only entry instead. It does not replace the AI Proxy or legacy
MCP server, or introduce MCP SDK dependencies. Only the MCP entry reads its body.

Agent Gateway 插件为匹配的 LLM 请求建立请求级上下文并继续现有链；显式匹配的 MCP 规则使用有界 tools-only 入口。它不替换 AI Proxy 或旧 MCP Server，不引入 MCP SDK 依赖；仅 MCP 入口读取自己的正文。

## Enablement

Add the starter dependency:

```xml
<dependency>
    <groupId>org.apache.shenyu</groupId>
    <artifactId>shenyu-spring-boot-starter-plugin-agent-gateway</artifactId>
</dependency>
```

Enable local bean assembly explicitly:

```yaml
shenyu:
  plugins:
    agent:
      gateway:
        enabled: true
```

The Admin plugin is initialized disabled. Enable `agentGateway` in Admin only
after the starter is loaded. Disabling the Admin plugin stops new context
creation while leaving the existing AI Proxy and MCP plugins unchanged.

## Rule handle

The LLM rule handle uses these fields:

```json
{
  "trafficType": "LLM",
  "responseRequestId": true
}
```

The existing MCP plugin remains independently configurable. `trafficType` is
required and must be exactly `LLM` or `mcp`. `responseRequestId` is an
optional boolean and defaults to `false`. Admin rejects unknown fields,
malformed JSON, missing required fields, invalid field types, and unsupported
traffic types before saving or importing an `agentGateway` rule handle. At
runtime, unknown fields in existing configurations are logged and ignored for
forward compatibility. Malformed JSON, missing required fields, invalid field
types, and unsupported traffic types return HTTP 500 because they are server-side
configuration errors. A selector with `continued=false` has no
rule handle, so this plugin passes it through without creating a context.

When enabled, the response contains the gateway-generated
`X-Shenyu-Agent-Request-Id` header. A client-provided value with the same name
is never used as the internal request ID.

## Context contract

### MCP tools entry and Accept policy

An MCP rule uses `"trafficType": "mcp"` and an `mcp` configuration, for example:

```json
{
  "trafficType": "mcp",
  "mcp": {
    "allowedTools": ["order_status"],
    "responseMode": "json"
  }
}
```

Only `server/discover`, `tools/list` and `tools/call` are supported. Tools must be
explicitly registered; rule permissions intersect trusted server-side tool
grants. Missing trusted identity is rejected. This is not remote tool aggregation.

The entry targets the [2026-07-28 Streamable HTTP client contract](https://modelcontextprotocol.io/specification/2026-07-28/basic/transports/streamable-http#sending-messages):
clients must explicitly list both `application/json` and `text/event-stream` in
`Accept`, with positive quality values. `responseMode` selects the server's
successful response format; it does not change the client's two-format contract.
This entry intentionally returns HTTP 406 for JSON-only, SSE-only, missing,
wildcard-only, or zero-quality required types, in either response mode. The
protocol's client requirement does not itself mandate this server rejection
status; 406 and explicit-type enforcement are this entry's strict policy, not a
claim that every server must reject those clients. Malformed Accept is HTTP 400.
Rejection precedes body subscription, identity resolution and tool invocation;
preflight errors use ordinary JSON in either mode. JSON-only compatibility is
not enabled by choosing `responseMode=json`.

| Accept | responseMode=json | responseMode=sse |
| --- | --- | --- |
| `application/json, text/event-stream` | JSON result | SSE result |
| `application/json` or `text/event-stream` alone | 406 | 406 |
| Missing, wildcard-only, or required type with `q=0` | 406 | 406 |
| Malformed media type | 400 | 400 |

### Protocol support and compatibility

This is an implementation contract for the tools-only entry, not a claim of
complete MCP conformance or final protocol approval. The PR remains draft while
the protocol choice and wider Agent Gateway MCP contract are stabilized. The
[fixed versioning contract](https://modelcontextprotocol.io/specification/2026-07-28/basic/versioning)
uses per-request version metadata rather than an initialization handshake.

| Boundary | Current entry behavior |
| --- | --- |
| Version | Only `2026-07-28`; no implicit fallback or translation to older revisions |
| `server/discover` | Advertises that version and tools capability only |
| `tools/list` | Complete permission-filtered local list; any supplied cursor is rejected |
| `tools/call` | One authorized local Provider invocation per subscription, without retry |
| Successful response | JSON object, or one final `event: message` SSE frame; no progress stream |
| Lifecycle | No `initialize`, protocol session, GET listener or session DELETE endpoint |
| Other methods | Rejected; no prompts/resources, MRTR, Tasks or subscriptions |

These behaviors apply only after an enabled, continued MCP rule matches. The
plugin does not expose a universal `/mcp` path. Unmatched or disabled traffic,
LLM rules and independently routed legacy MCP traffic keep their existing chain.
An initialization-based client must use a separately configured legacy endpoint
or an explicit client adapter; this entry does not make SDK 0.17.0 interoperable
with the newer wire contract. Remote SDK experiments are not part of this module.

The parser accepts one UTF-8 JSON-RPC 2.0 object with a string or integral ID,
object `params`, and object `_meta`. It requires a string
`io.modelcontextprotocol/protocolVersion` and object
`io.modelcontextprotocol/clientCapabilities`. Batch, notification, response and
request/response-hybrid bodies are rejected. Client metadata is not trusted
identity or a replacement for server-side grants.

| Header | Validated against |
| --- | --- |
| `MCP-Protocol-Version` | `_meta.io.modelcontextprotocol/protocolVersion` |
| `Mcp-Method` | JSON-RPC `method` |
| `Mcp-Name`, for `tools/call` | `params.name`; canonical Base64 sentinel decoding is supported |

Each mirrored header must have one safe value. Header names are case-insensitive,
but values must match exactly. `Content-Type` must be UTF-8 `application/json`.
Absent Origin is not authentication; a supplied Origin must exactly match the
rule's allowed origins, and duplicate origins are rejected. A client-supplied
session ID does not select identity or request state.

### Results, errors and authorization

Visible and callable tools share the intersection of registered tools, rule
`allowedTools` and trusted identity grants. An empty intersection grants no
calls. With a valid rule/registry configuration, unknown and invisible names
have the same public denial. A rule referring to an unregistered tool instead
fails as server misconfiguration, not a successful empty list. Record-level
authorization belongs to the Provider; tool-name permission alone is not enough.

The current Provider returns a business JSON object. The entry encodes it as
`structuredContent` plus one text item and `isError=false`. Declared argument
validation failures and explicit `AgentToolExecutionException` failures instead
produce a sanitized `isError=true` result. Unexpected exceptions and empty
Provider completion are internal protocol errors. Providers must not return an
entire remote `CallToolResult` expecting lossless passthrough; native remote
results need a separate aggregation adapter, outside this PR.

Successful discovery/list results use `resultType=complete`, `ttlMs=0` and
`cacheScope=private`; calls also have `resultType=complete`. Each result includes
server information in `_meta`; this does not create a shared identity cache.

The following matrix assumes an authenticated request passing preceding checks;
it does not specify precedence when several inputs are invalid.

| Condition | HTTP | Body |
| --- | --- | --- |
| Invalid Origin / non-POST / unsupported media / rejected Accept | 403 / 405 / 415 / 406 | Ordinary JSON transport error; malformed media headers use 400 |
| Missing trusted identity | 401 | Ordinary JSON transport error |
| Invalid JSON or UTF-8 | 400 | JSON-RPC error `-32700`, null ID |
| Invalid envelope, including batch or notification | 400 | JSON-RPC error `-32600`, null ID |
| Invalid metadata or parameters; supplied list cursor | 400 | JSON-RPC error `-32602` |
| Missing, duplicate or mismatched mirrored header | 400 | JSON-RPC error `-32020` |
| Unsupported version / method | 400 / 404 | JSON-RPC error `-32022` with supported versions / `-32601` |
| Unknown or forbidden tool | 403 | JSON-RPC error `-32602` |
| Missing required client capability | 400 | JSON-RPC error `-32021` with the missing capability tree |
| Declared Provider argument or business failure | 200 | JSON-RPC result with `isError=true` |
| Invalid server configuration / unexpected tool failure / encoded response overflow | 500 | Configuration or sanitized internal error; no tool retry |
| Request body overflow / execution deadline | 413 / 504 | Ordinary JSON transport error |

Transport and JSON-RPC error responses are JSON in either response mode,
including failures occurring after body parsing. Business failures are results
with `isError=true` and still follow `responseMode`. Parsed protocol errors
preserve the available RPC ID; preflight errors
do not parse the body to recover one. Once the response is committed, the handler
propagates a failure rather than writing a second response. A disconnected client
is not guaranteed to receive an error body.

### Deadlines, isolation and configuration limits

One execution budget covers identity resolution, body acquisition, parsing,
Provider execution, encoding and response writing. The invocation receives its
absolute deadline. The handler cancels the current reactive subscription on
timeout or downstream cancellation, without closing shared registry/pool state.
Termination cleanup is request-local; equal client RPC IDs or supplied session
IDs do not merge calls. Adapters must honor cancellation for their own transport;
this entry cannot kill blocking code, roll back a side effect or stop billing.

After execution has terminated, an uncommitted error write has a separate
best-effort budget of at most `min(1000, timeoutMs)` milliseconds. It does not
extend the Provider's execution allowance. Earlier server filters/connection
policies have their own budgets.

| MCP rule field | Default | Bounds / behavior |
| --- | --- | --- |
| `allowedTools` / `allowedOrigins` | Empty sets | Explicit unique names / exact HTTP(S) origins; no wildcard grant |
| `responseMode` | `json` | `json` or `sse`; does not relax Accept validation |
| `timeoutMs` | 30000 | Integer 100–120000 |
| `maxRequestBytes` | 262144 | Integer 1024–1048576, UTF-8 bytes |
| `maxResponseBytes` | 1048576 | Integer 1024–4194304, includes SSE framing |

Admin create/update/import rejects unknown fields and invalid known values.
Runtime parsing permits unknown fields but rejects invalid known values. Rules
are captured per subscription; later updates affect new requests, not the
in-flight snapshot. `configurationVersion` is a node-local generation, not an
Admin revision. Tool registration is frozen at startup; dynamic targets and
credential/session lifecycle are not provided. Server codec limits may be
smaller than rule limits and remain effective.

### Contract regression coverage

`AgentMcpRequestParserTest` covers envelope, version and mirrored-header checks;
`AgentMcpDispatcherTest` covers discovery, permission filtering, result/error
semantics and asynchronous isolation. `AgentMcpHttpHandlerTest` covers both
response modes, strict Accept, HTTP error mapping, byte limits and cancellation
through body, execution and writing. Config/registry/plugin/Starter tests cover
assembly and snapshot boundaries. Passing these targeted tests is not complete
MCP, OAuth, full-project or arbitrary-client acceptance, nor a reviewer approval.

### Request context

`AgentTrafficContext` is immutable and is created per subscription. It contains
only the generated request ID, traffic type, selector ID, and rule ID. During
downstream execution it is available from the dedicated Reactor context key
`AgentGatewayConstants.REACTOR_CONTEXT_KEY` and the exchange attribute
`AgentGatewayConstants.REQUEST_CONTEXT_ATTRIBUTE`.

The context is not stored in a global map or thread-local. It does not contain
the request body, credentials, response content, or mutable plugin-chain state.
Each subscription gets its own context. The same exchange should not be
subscribed concurrently, because exchange attributes are shared by that request.

## Scope

This module provides LLM request correlation and a tools-only MCP entry with
request-local identity, permissions, deadlines and cancellation. Remote tool
aggregation, prompts/resources, callback bridging and usage/cost accounting are
not included. Existing LLM forwarding and legacy MCP sessions remain separate.
