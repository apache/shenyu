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

# Agent Gateway request context and terminal facts

## Governance entry

The matched Agent Gateway rule creates a fresh request identifier at subscription time.
The default order is authentication (key-auth 150), Agent Gateway 169, AI Prompt 170,
AI Token Limiter 171, and AI Proxy 199. MCP terminates at Agent Gateway and does not
enter the LLM limiter/proxy. LLM continues through the existing AI plugins. No second
selector/rule matcher, request-body subscription, or request-body cache is introduced.

Admin's persisted plugin and namespace-plugin order overrides the Java default.
Apply the documented default-order migration and check custom orders before enabling
governance; an incorrect custom order is not automatically corrected. See
[order and upgrade contract](agent-gateway-governance-order.md).

## Trusted immutable context

`AgentRequestContext` is shared from `shenyu-common` in Reactor Context under
`AgentRequestContext.REACTOR_KEY`. It carries server-generated request identity,
matched selector/rule, a node-local rule generation, trusted tenant/subject when
resolved, and the target/operation/version when actually known. New facts produce
new immutable snapshots; cancellation and concurrent requests never share a global
request map or a thread-local context. Existing `AgentTrafficContext` remains for
compatibility and uses the same request identifier.

The rule generation is **not** an Admin revision or an authorization-policy version.
No policy revision is fabricated where an existing identity adapter supplies none.
MCP records only its validated inbound protocol version. The AI protocol identifier
is reported separately on attempts and is not a fabricated protocol version.
The stateless inbound endpoint has no session: its session is always unknown. A
remote transport session, client RPC id, trace id, or arbitrary header is not substituted.

MCP continues to require the existing authoritative security resolver. Its final
authorization remains the intersection of rule tools and effective grants, with
denials already removed by the adapter. Context is not an authorization token.

LLM identity adaptation is explicitly opt-in:

```yaml
shenyu:
  plugins:
    agent:
      gateway:
        enabled: true
        security:
          llm:
            enabled: true
```

This requires exactly one configured `AgentMcpSecurityResolver`, reusing the
existing verified JWT, OAuth principal, API-key binding, or application adapter.
An empty resolution returns 401 before reading the LLM body; a resolver failure
does not silently turn into anonymous access. With the setting disabled, existing
LLM behavior is preserved and tenant/subject remain unknown. Tool grants are not
interpreted as permission to use a model. Existing LLM access and limiter policy
remain responsible for their own admission decisions.

Authentication plugins that reject **before** Agent Gateway remain outside its
terminal-event scope. This is not an audit of every HTTP request to the gateway.

## One logical terminal, separate attempts

Applications may provide one `AgentMcpTerminalObserver` and one `AiCallObserver`.
The matched LLM entry owns `AiRequestObservation` across downstream limiter, proxy,
retry/fallback, response writes and cancellation. AI Proxy and its engine reuse that
owner rather than emitting additional logical terminals. When used without Agent
Gateway, AI Proxy/engine create their own anonymous correlation and own termination.

MCP snapshots cover matched preflight, identity, parsing, authorization, tool outcome,
timeout, cancellation and write failure. A requested tool does not prove it was
authorized or executed. AI has one event per actual upstream attempt, and one
logical event after the owned chain terminates. A limiter denial has zero attempts.
Cancellation is not success; assigned HTTP status does not prove response delivery.
No event contains request/response text, tool arguments, credentials or exception messages.

Observer exceptions are isolated from business behavior. Observers must be nonblocking
and use bounded application-owned queues; durable logging, queue policy and audit
storage belong to the later audit feature, not this callback contract.

## Usage is an observation, not a charge

The existing AI engine reads each upstream body once. A bounded passive tap observes
raw JSON/SSE before provider adaptation and protocol conversion; it does not subscribe
again, change bytes, or alter retry/fallback policy. Only explicit upstream
`prompt_tokens`, `completion_tokens`, and `total_tokens` are observed. Missing counts
are null, not zero; totals are not calculated. Multiple cumulative SSE usage objects
replace the prior snapshot rather than being added. Invalid, unsupported and
oversized observations are distinguishable from reported usage.

The logical terminal carries the **last started attempt's** usage and attempt count,
not a fabricated aggregate across retries. Attempt events preserve each attempt's
own observation. These snapshots do not establish billing correctness or settle a
budget. Existing Redis Token Limiter charging behavior is deliberately unchanged.
Provider-specific cached/reasoning tokens, prices, multi-attempt cost aggregation,
and budget settlement remain separate P1 work.

## Trace propagation

The optional application-owned MCP tracing bridge reads the gateway's explicit
`TracingConstants.REACTOR_CONTEXT_KEY` parent first, retaining `Context.class`
compatibility. The gateway SERVER span is the parent of the MCP logical-call span.
Only the locally encoded W3C `traceparent`/`tracestate` are passed through the
request-scoped JDK MCP client for `tools/call`. Shared initialization, initialized
notification and catalog discovery are not attributed to the first Agent request.

AI WebClient transport similarly uses the explicit gateway Reactor tracing parent.
It does not create another SDK, exporter or ambient thread-local scope. Baggage,
client credentials and tenant/subject headers are not copied to the upstream.
Outbound service credentials retain their existing target-owned resolution.
Trace identities are telemetry and never authorization evidence.

## Acceptance boundary

Regression coverage includes real loopback HTTP authentication-to-limiter-to-AI
JSON/SSE traffic, single request-body subscription, early denial, actual sorted
plugin cache order, tenant isolation, gateway-to-MCP parentage, real remote MCP
outbound headers, shared-handshake isolation, concurrent/cancelled requests, usage
fragments, retries/fallback and terminal de-duplication. Windows and RISC-V/QEMU
must run the final identical source snapshot before local acceptance. Broader
Admin/WebSocket/Bootstrap, selected external Agents, Java-agent permutations and
distributed failure matrices are tracked separately in P0-04, not implied by unit
or loopback results.
