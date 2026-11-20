# AI Normalized Request View (issue #8255)

A bounded, immutable, generation-aware structural view of one AI request body
version, shared across the AI governance chain instead of every stage re-parsing
the full body. This is the first delivery of #8255: the contract, the
publication/consumption semantics, the wiring of the prompt plugin (publisher),
the token limiter (inspection consumer) and the AI proxy (authoritative
publisher at decode time), and the source-to-consumer matrix below.

## Contract

- `org.apache.shenyu.common.ai.AiNormalizedRequestView` — the immutable view:
  protocol/operation/model hints, bounded message overview (role, content kind,
  bounded text prefix), tool presence, view kind, mutator, generation, and an
  explicit unsupported flag with a reason.
- `org.apache.shenyu.plugin.ai.common.view.AiRequestViewOperations` — publication
  and consumption over a server exchange attribute.

Bounds and guarantees:

- At most `MAX_MESSAGES` message entries, at most `MAX_MESSAGE_CHARS` characters
  of textual content per message, structured content is reported by kind and is
  never flattened into text, opaque fields stay opaque (passthrough semantics of
  the native protocols are untouched — the view is an interpretation, the proxy
  decode remains authoritative and is never replaced by the view).
- At most two views are retained per exchange (the original wire view and the
  current view) plus a bounded inspection log of at most `MAX_INSPECTIONS`
  entries. The request body itself is never retained, only a 64-bit content
  fingerprint per publication.
- State lives exclusively in a server exchange attribute: it is released with
  the exchange, there is no global map and no explicit cleanup path. Request
  content never leaves the view contract — it is never placed in
  `AgentRequestContext`, trace or audit events.

## Generations and inspection currency

The first publication of an exchange is the `WIRE` view. A publication with a
different body content creates the next generation as an `EFFECTIVE` view naming
the mutator (for example `aiPrompt`). Consumers read the view through
`snapshotAndRecordInspection`, which records the generation they saw. A consumer
whose recorded generation is no longer current is reported as superseded in
`inspectionReport` — a misordered or stale consumer cannot claim a successful
inspection of the effective request. When no publisher ran before a consumer,
nothing is recorded and the snapshot is empty: unsupported inspection stays
visible, never simulated.

## Source-to-consumer matrix

| Stage | Order | Body access | View usage |
|---|---|---|---|
| aiRequestTransformer | 65/66 | full body (template rewrite) | not wired yet (full-content consumer); follow-up |
| aiPrompt | 170 | full body via `rewriteRequestBody` | publishes the `WIRE` view and, on a real decoration, the next `EFFECTIVE` generation |
| aiTokenLimiter | 171 | Redis counters, response-side usage | inspection-only consumer: records the generation it acted on; budget estimation plugs into the same seam later |
| piiDetection | 196 | full body via `rewriteRequestBody` | not wired: scanning requires the full body, a bounded view would weaken detection (documented full-content consumer) |
| sensitiveWord | 197 | full body via `rewriteRequestBody` | not wired: same full-content reasoning as pii |
| aiProxy | 199 | authoritative decode in `AiProxyEngine.prepare` | publishes the body it is about to decode; republishing an already published version is a no-op, so the generation only moves when the body really changed |

## Coverage matrix (supported protocols)

| Protocol | View coverage | Unsupported/Opaque handling |
|---|---|---|
| openai-chat (`messages`) | first-class: roles, content kinds, tools, tool calls, model, stream | unknown message fields stay opaque; non-textual content is `STRUCTURED`, never flattened |
| openai-responses (`input`) | structural only: operation, model, tools presence | deep field coverage is a follow-up; content stays opaque |
| anything else | explicitly unsupported view with a reason (empty, not JSON, not an object, no known protocol fields) | unsupported inspection remains visible; the proxy still enforces its own protocol match |

## What this does not do (follow-ups)

- No second body subscription is added: the wire body is still read once by the
  component that owns it (prompt rewrite or the proxy), and every view reader
  consumes the published interpretation instead of re-parsing.
- No streaming view, no request/response transformer wiring, no versioned
  prompt-template integration (#8051), no evaluation sampling (#8256) and no
  schema validation/repair (#8257) — those plug into the same generation seam.
- No throughput or overhead claim beyond design: publication is one Jackson
  parse per changed body version and a fingerprint compare per unchanged call;
  proxy-only and disabled/unmatched governance behavior is unchanged.
