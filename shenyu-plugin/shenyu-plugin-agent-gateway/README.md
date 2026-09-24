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
and then continues the existing plugin chain. It does not replace the AI Proxy
implementation, read the request body, or introduce MCP SDK dependencies.

Agent Gateway 插件为匹配的 LLM 请求建立请求级上下文，然后继续执行现有插件链。它复用现有 AI Proxy，不读取请求体，也不引入 MCP SDK 依赖。

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

The first version accepts only this rule handle shape:

```json
{
  "trafficType": "LLM",
  "responseRequestId": true
}
```

`trafficType` is required and must be exactly `LLM`. `responseRequestId` is an
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

`AgentTrafficContext` is immutable and is created per subscription. It contains
only the generated request ID, traffic type, selector ID, and rule ID. During
downstream execution it is available from the dedicated Reactor context key
`AgentGatewayConstants.REACTOR_CONTEXT_KEY` and the exchange attribute
`AgentGatewayConstants.REQUEST_CONTEXT_ATTRIBUTE`.

The context is not stored in a global map or thread-local. It does not contain
the request body, credentials, response content, or mutable plugin-chain state.
The plugin does not support subscribing to the same execution publisher twice.

## Scope

This module provides request correlation and Reactor context propagation for
LLM traffic. MCP discovery, tool aggregation, callback bridging, usage/cost
accounting, and unified identity or permission governance remain separate
features and are not enabled by this plugin.
