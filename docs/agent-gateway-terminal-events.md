# Agent Gateway MCP terminal events

This document describes the MCP terminal-event part of P0-03. The integrated
AI/MCP context, governance and tracing contract is documented in
[request context and terminal facts](agent-gateway-request-context.md).

An optional application-owned AgentMcpTerminalObserver receives one immutable
event per subscription after execution and response writing terminate, including
recovery, preflight/authentication rejection and downstream cancellation.
Authentication failures do not consume the body for observability.

Events carry gateway request/rule/selector references, node-local configuration
generation, tenant and subject only after trusted resolution, validated operation/requested
tool and protocol version when available, outcome, monotonic handler duration
and assigned HTTP status. Requested tool does not mean authorized or executed;
assigned status does not prove response delivery, especially on cancellation.
No body, arguments, credentials, RPC ID, invented session or fabricated usage is
retained. Identifiers must not become unrestricted metric labels. Observers must
be nonblocking and bound their queues. Callback failures cannot affect responses,
metrics or tracing. No observer means no event sink is installed.

Existing tools/call metrics retain their authenticated-and-parsed scope and timing.
The event covers all matched MCP requests and includes authentication duration;
do not count it into the same counter as the existing metrics callback.

The integrated implementation carries trusted identity from the configured resolver,
enters governance at order 169 before AI Prompt (170), Token Limiter (171) and
AI Proxy (199), and connects the gateway SERVER span to the MCP logical-call span.
MCP tools/call and AI outbound tracing use request-local W3C context without
forwarding identity or baggage. AI usage is observed from the existing response
subscription, with separate attempt events and one logical terminal.

Node-local configuration generation is not an authorization-policy revision. No
revision is invented when the identity source provides none. Trace IDs are not
identity; missing usage is unknown, not zero. Deployment-wide verification beyond
the feature's loopback integration regressions belongs to P0-04.
