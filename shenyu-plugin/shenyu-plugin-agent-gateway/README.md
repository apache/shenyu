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
