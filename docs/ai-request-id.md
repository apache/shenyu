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

# AI RequestId plugin

The optional `aiRequestId` plugin correlates one logical AI request across AI
governance, upstream attempts, response delivery, access logs and observation
events. It is AI-only and does not add a general-purpose HTTP/RPC request-id
feature.

`requestId` is **not** an identity, an authorization grant, an idempotency key,
or a session/conversation identifier. It does not authorize replay or guarantee
exactly-once model or tool execution. It also does not replace a provider request
id, model response id, W3C trace id or MCP JSON-RPC id.

## Enablement

The starter is part of the default bootstrap and is assembled unless explicitly
disabled:

```yaml
shenyu:
  plugins:
    ai:
      request:
        id:
          enabled: true   # default: assembled when absent
```

Assembly alone does not generate ids. The Admin `aiRequestId` plugin is seeded
disabled. Enable it and add a selector/rule that matches the intended LLM paths:

```text
authentication (keyAuth 150, JWT 30, OAuth2 40, ...)
  -> aiRequestId (168)
  -> agentGateway (169, LLM rules only)
  -> aiPrompt (170)
  -> aiTokenLimiter (171)
  -> piiDetection (196)
  -> aiProxy (199)
```

Keep the selector scoped to LLM paths. Top-level MCP endpoints are not covered
by the first delivery and must not be included in this selector.

A rule handle has one optional field:

```json
{
  "responseHeaderEnabled": true
}
```

`responseHeaderEnabled` defaults to `true`. Setting it to `false` keeps generation
and internal correlation but stops the canonical response header.

Requests that do not match the `aiRequestId` selector keep their existing behavior
and receive no generated id. A request rejected by an authentication plugin before
`aiRequestId` runs cannot receive its response header.

## Ordering

`aiRequestId` uses default sort **168**, before Agent Gateway (169). Admin's
persisted `plugin.sort` / `namespace_plugin_rel.sort` override the Java default,
so verify the effective order after upgrading:

```sql
SELECT id, name, sort, enabled
FROM plugin
WHERE name IN ('aiRequestId', 'agentGateway', 'aiPrompt', 'aiTokenLimiter', 'aiProxy')
ORDER BY sort, name;
```

Equal sort values do not establish a safe relative order. The required invariant
for an enabled LLM path is:

```text
authentication < aiRequestId < agentGateway (LLM rules only) < aiPrompt < aiTokenLimiter < aiProxy
```

See [Agent Gateway governance order](agent-gateway-governance-order.md) for the
upgrade and inspection procedure.

## Response header contract

The canonical response header is:

```text
X-Shenyu-Ai-Request-Id: <requestId>
```

The legacy Agent Gateway header keeps its existing behavior:

- `X-Shenyu-Agent-Request-Id` is still controlled only by the Agent Gateway rule
  field `responseRequestId`.
- `aiRequestId` never writes the legacy header.
- When both headers are returned for the same LLM request, they carry the same
  value; neither plugin emits a different id under another name.

The id is registered before response commitment, including before the first SSE
byte. If the plugin is not enabled or does not match, no canonical header is
added. A matched rule that fails validation is still rejected with the canonical
header and correlation id installed, so the rejection appears in access logs and
can be correlated. Caller-supplied `X-Shenyu-Ai-Request-Id` values are ignored;
the first version does not accept inbound ids.

The first delivery covers LLM HTTP requests through standalone AI Proxy and
Agent Gateway's LLM path. Top-level MCP traffic is not covered by the canonical
header; Agent Gateway MCP rules keep their own entry id. Because `aiRequestId`
runs before Agent Gateway and cannot tell LLM from MCP, do not configure its
selector over MCP endpoints: an overlapping configuration can return a canonical
header whose value differs from the legacy MCP entry header and can split
access-log/MCP-event correlation.

### Browser CORS exposure

The plugin does not weaken CORS policy. Browsers only expose a custom response
header when the deployment lists it:

```yaml
# Example only: add this to the deployment's existing CORS configuration.
Access-Control-Expose-Headers: X-Shenyu-Ai-Request-Id
```

## One logical id, separate attempts

One AI request keeps the same `requestId` across AI policy plugins, Reactor
scheduler switches, retries and fallback providers:

```text
requestId = 5f0c...c41
  primary attempt    (requestId, 0)
  retry attempt      (requestId, 1)
  fallback attempt   (requestId, 2)
```

Retry/fallback events expose a zero-based `attemptIndex`; observation attempt
events expose a one-based `attemptNumber`. Both describe actual upstream attempts,
not new logical requests.

Agent Gateway LLM rules and AI Proxy reuse the id established by `aiRequestId`;
they do not replace it. An existing context is reused only when its id is 1-128
characters from `[A-Za-z0-9._:-]`; an invalid or unbounded value is replaced by a
new gateway id and a bounded warning is logged without the rejected value.
The logical requestId is preserved, but observation events report the selector
and rule of the observation owner: Agent Gateway for its LLM path and AI Proxy
for standalone requests. Enabling `aiRequestId` therefore does not change
existing usage attribution.
Agent Gateway MCP rules continue to create their own MCP entry id in the first
delivery; the canonical header contract above does not apply to top-level MCP
traffic. HTTP tool child invocations dispatched by the MCP entry copy the parent
context into the child exchange so child logs can correlate, and the `aiRequestId`
plugin skips generation for those internal calls.

## Logging

Access logs add a nullable `requestId` field to `ShenyuRequestLog`. It is populated
at response time from the installed `AgentRequestContext`, so it also appears when
logging plugins run before `aiRequestId` in the sorted chain.

The field appears in every logging sink payload because sinks share
`ShenyuRequestLog` serialization. Consumers of Kafka/Elasticsearch/etc. should
tolerate one additional nullable field. `requestId` participates in the configured
logging keyword masking like other text fields, is not used as a metric label,
and no prompt, credential, tool argument or response content is added.

## Trace ids

`traceId` remains optional and independent. Correlation works with tracing
disabled. When tracing is enabled, the same request can carry both a `traceId`
and an AI `requestId`; they are associated but never substituted for each other.

## Upgrade notes

The plugin seed is included in the database init scripts and in the
2.7.2 -> 2.7.3 upgrade scripts for every supported dialect. After upgrading:

1. Confirm the `aiRequestId` plugin row exists and is disabled by default.
2. Confirm both `plugin.sort` and `namespace_plugin_rel.sort` are `168`.
3. Enable the plugin and create selectors/rules for the intended AI paths.
4. Synchronize the configuration to gateways; a database update alone does not
   republish plugin caches or reorder a running chain.

## Non-goals

- No request/response body reads, aggregation or rewriting.
- No id appended to prompts, JSON/SSE bodies or tool arguments.
- No inbound id acceptance or trust policy in the first version.
- No upstream header forwarding adapter in the first version. Provider request ids,
  model response ids, trace ids and JSON-RPC ids are left untouched. A future
  adapter must be opt-in, explicitly configured per provider/transport, and must
  not blindly copy inbound headers or rewrite bodies.
- No conversation-wide or agent-run-wide correlation.
- No new audit, retry or tracing subsystem.
- No full MCP first-delivery coverage; only linked child-invocation propagation.

## Related documents

- [Agent Gateway request context](agent-gateway-request-context.md)
- [Agent Gateway governance order](agent-gateway-governance-order.md)
- [Agent Gateway terminal events](agent-gateway-terminal-events.md)
- [AI Proxy fallback targets](ai-proxy-fallback-targets.md)
