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

# Local Agent Gateway MCP example

This example registers one read-only `order_status` tool and a verified-principal
adapter for the real Agent Gateway plugin. It uses synthetic data only.
It is disabled by default, requires an explicit loopback bind, and has no default
JWT signing key. Do not expose it publicly or reuse its policy as production authentication.

## Assembly

Build the Agent Gateway starter and existing JWT module with the root Maven wrapper,
then build `shenyu-examples-plugin` with the examples POM:

```sh
./mvnw -pl shenyu-spring-boot-starter/shenyu-spring-boot-starter-plugin/shenyu-spring-boot-starter-plugin-agent-gateway,shenyu-plugin/shenyu-plugin-security/shenyu-plugin-jwt -am install -DskipTests
./mvnw -f shenyu-examples/pom.xml -pl shenyu-examples-plugin verify
```

Put the example JAR and its dependencies on the local bootstrap classpath.
Explicitly import `AgentGatewayExampleConfiguration` in a local launcher configuration;
the normal bootstrap does not component-scan the examples package.
Alternatively, on a bootstrap distribution classpath containing this JAR, add the
configuration through `spring.main.sources`. No production auto-configuration or
new global HTTP route is installed by the example.

Set the following properties in an isolated local deployment:

```properties
server.address=127.0.0.1
shenyu.plugins.agent.gateway.enabled=true
shenyu.examples.agent.gateway.enabled=true
shenyu.examples.agent.gateway.jwt-secret=${EXAMPLE_MCP_JWT_SECRET}
```

Generate a new cryptographically random ASCII secret (at least 32 characters),
supply it through the process environment, and never commit it.
The example reuses `DefaultJwtPayloadParseStrategy` from the existing JWT plugin
and permits signed HS256 tokens only. Tokens require issuer `shenyu-agent-example`,
audience `shenyu-mcp-example`, and a valid `exp`; signature, expiration and
`nbf` are checked by the existing verifier.

Server-side policy grants `order_status` to subjects `agent-a` and `agent-b`;
`agent-none` has no grants. A JWT's arbitrary `tools` claim, request headers,
client metadata, session ID and JSON-RPC ID do not grant privileges.
A verified token installs `AgentMcpPrincipal`; the plugin never reparses credentials.
The filter authenticates only `/agent/mcp` and its children and leaves other routes alone.

This is a local bearer-token demonstration, **not** an OAuth resource-server or
authorization-server implementation. It does not implement protected-resource
metadata, discovery or authorization-code flows. A production MCP HTTP deployment
must provide its own verified identity adapter and evaluate the MCP authorization
specification; this example does not establish OAuth conformance.

## Admin configuration

Registered providers may override `getRequiredClientCapabilities()` to return a
capability requirement object, for example `{"sampling": {}}`. Empty objects
require a declared object; nested objects and `true` feature markers are supported.
Other requirement shapes fail registration. Definitions and each invocation's
`clientCapabilities` are defensively copied. The registry checks requirements
after tool authorization but before argument validation or business execution.
Missing requirements return HTTP 400 / JSON-RPC `-32021`, with only the missing
object tree in `error.data.requiredCapabilities`. No previous request or session
supplies capabilities. A declaration never grants access to a tool or an order.

`order_status` requires no client callbacks. Capability gating does not implement
sampling, elicitation, MRTR or Tasks; it is not evidence that these features work.
Conformance diagnostic tools belong only in external test launchers, not this example
or normal bootstrap registration.

Use the normal Admin API/UI and selected data-sync channel. Enable `agentGateway`,
create a selector matching only `/agent/mcp/json` (or an explicitly chosen child),
and create a matching rule with this handle:

```json
{
  "trafficType": "mcp",
  "responseRequestId": true,
  "mcp": {
    "allowedTools": ["order_status"],
    "allowedOrigins": ["http://127.0.0.1:9195"],
    "responseMode": "json",
    "timeoutMs": 3000,
    "maxRequestBytes": 262144,
    "maxResponseBytes": 1048576
  }
}
```

For single-result SSE, use a distinct selector/rule and `responseMode: "sse"`.
Origins are exact allowlisted values, not wildcards. Origin-less native clients
are permitted after authentication. Keep these paths disjoint from old MCP and
AI Proxy selectors. Only a matched, enabled MCP rule consumes the request.
An empty tool list denies all tools; unregistered configured names fail closed.

## Requests and expected results

Send a POST with `Content-Type: application/json`,
`Accept: application/json, text/event-stream`,
`Authorization: Bearer <token>`, `MCP-Protocol-Version: 2026-07-28`,
`Mcp-Method: tools/call`, and `Mcp-Name: order_status`:

```json
{
  "jsonrpc": "2.0",
  "id": "same-client-id",
  "method": "tools/call",
  "params": {
    "name": "order_status",
    "arguments": {"orderId": "demo-A-001"},
    "_meta": {
      "io.modelcontextprotocol/protocolVersion": "2026-07-28",
      "io.modelcontextprotocol/clientCapabilities": {}
    }
  }
}
```

With subject `agent-a`, the complete tool response contains `structuredContent`
with `orderId: "demo-A-001"`, `status: "SHIPPED"` and an internally generated
`requestId`. The optional `X-Shenyu-Agent-Request-Id` header matches that ID.
Subject `agent-b` can read only `demo-B-001`. A nonexistent order and another
subject's order produce the same sanitized tool error, without revealing existence.

Use `server/discover` and `tools/list` with matching `Mcp-Method` and the same
required metadata (omit `Mcp-Name` for those methods). Discovery advertises tools
only, not resources/prompts, subscriptions, tasks, progress or sampling.
JSON returns one complete response; SSE returns one final message and closes.
There is no new MCP session, GET stream, initialization handshake or retry.

The internal invocation carries subject, requestId, ruleId, node-local configuration
generation and deadline, without raw credentials or the HTTP exchange.
Cancellation remains linked to the provider's reactive subscription; deadlines
do not guarantee interruption of blocking code or rollback of business operations.
Providers must validate arguments and return one bounded result lazily, without
detached subscriptions, shared request state or automatic retry.
