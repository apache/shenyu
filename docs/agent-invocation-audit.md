# Agent invocation audit

Agent Gateway and AI Proxy can emit metadata-only invocation audit events when `shenyu.plugins.agent.audit.enabled=true`.

## Record contract

Every event has schema version `1`, a logical request id, selector/rule ids, configuration version, policy version, target, decision, bounded outcome/reason, optional HTTP status, and timestamp. Logical records are terminal results; attempt records are separately numbered and correlated by the same request id.

Trusted tenant and subject are included when a server-side identity adapter resolves them. No prompt, arguments, response body, credential, raw API key, header map, or trace id is stored. W3C trace ids are correlation-only when used by tracing, never identity.

Target is optional metadata. The bridge omits caller-supplied targets that are blank, longer than 256 characters, padded with whitespace, or contain control characters. It preserves the logical rejection record and its trusted identity, outcome and status; it does not truncate an invalid target into another tool name. Valid targets remain unchanged, and direct audit-event builders retain strict metadata validation.

Policy version identifies the admission contract (`tool-grant-intersection-v1`, `agent-ai-admission-v1`, `key-auth-admission-v1` or `agent-rule-admission-v1`), not a revision of an external identity provider's policy. Configuration version is the captured node-local Agent Gateway rule generation. Standalone AI Proxy and rejected invalid configuration have no such generation and explicitly report `unknown`; no fingerprint of credential-bearing configuration is emitted.

## Outcomes

Logical and attempt records use `SUCCESS`, `TOOL_ERROR`, `REJECTED`, `RATE_LIMITED`, `TIMEOUT`, `CANCELLED`, and `SERVER_ERROR`. MCP attempt presence is marked after pre-execution validation; its terminal snapshot and duration come from the logical owner, including validation, result filtering and response writing. It is an actual-execution marker, not an upstream wire-latency metric. Rejected authorization or missing client capabilities never create an attempt. Reasons are bounded strings such as `TOOL_REPORTED_ERROR`, `DEADLINE_EXCEEDED`, `DOWNSTREAM_CANCELLED`, `HTTP_500`, `CREDENTIAL_REJECTED` and `INVALID_CONFIGURATION`.

The bridge consumes existing `AiRequestObservation` and `AgentMcpTerminalObservation` events. It does not create another request registry or complete an inherited logical call again. AI retry and fallback attempts use the existing one-based attempt sequence. Application terminal observers continue receiving events even when audit delivery fails. Key Auth rejection happens before Agent Gateway and produces a separate admission terminal record with a fresh server request id and no invented tenant/subject/model; it does not create an AI call or upstream attempt. Existing response status/body remain unchanged, including Key Auth's existing response-status convention. HTTP status is an assigned status, not proof of response delivery.

## Delivery

The shared `shenyu-spring-boot-starter-agent-audit` is included by both Agent Gateway and AI Proxy starters; AI-only deployment does not require Agent Gateway. The default observer submits metadata to ShenYu's existing `AbstractLogCollector`, which delivers asynchronously to SLF4J logger `org.apache.shenyu.agent.invocation.audit`. Callback exceptions cannot alter a gateway response. Applications may replace the observer bean with an external nonblocking sink, or provide an `AgentInvocationAuditConsumer` for worker-side batch delivery.

The adapter reuses the existing collector's single-client bounded admission, worker, batches of up to 100, polling interval of approximately 100 ms and `LoggingQueueMetrics`. Queue saturation rejects new records; accepted/dropped/backlog/capacity have the existing collector-class labels. Sink exceptions increment `AgentInvocationAuditCollector.getFailedRecords()` and emit a fixed warning without exception payloads. The collector interrupts its worker and attempts a final queue flush on close; it does not guarantee delivery of an interrupted in-flight batch, persist records, retry failed batches or count post-close submissions as queue overflow. Final flush runs on the shutdown caller and depends on the sink's timeout behavior. Custom sinks must bound their blocking time. This is best-effort, not lossless delivery.

Configuration:

| Property | Default | Meaning |
|---|---:|---|
|`shenyu.plugins.agent.audit.enabled`|`false`|Enable the managed observer.|
|`shenyu.plugins.agent.audit.queue-capacity`|`8192`|Maximum waiting records.|

Retention and remote delivery are owned by the configured logging or storage backend, not by the gateway plugins.

## Known identity boundary

AI traffic inside Agent Gateway uses the inherited immutable trusted tenant/subject context, including a rejection before AI Proxy. Standalone AI Proxy requests do not infer tenant/subject from an API key or trace id and remain unscoped. MCP uses its resolved security identity. AI Proxy includes the synchronized mapping's internal database ID when available; older Admin payloads without an ID remain supported and leave that field absent. Key Auth's current authentication contract has no separate credential-record ID, so it is left absent rather than substituting a raw key. Neither the presented proxy key nor the upstream credential is copied into audit events.

## Integration baseline

The request/terminal contracts from PR #7860 are now on master. This candidate still integrates runtime governance from PR #7921; the audit delta must be reviewed against the resulting master after that prerequisite merges. The earlier independent prototype's identity interface, completion logic and custom queue are not used.
