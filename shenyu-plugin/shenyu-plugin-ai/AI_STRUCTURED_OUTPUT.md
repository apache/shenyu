# AI Structured Output Validation & Bounded Repair (issue #8257)

An upstream HTTP/Chat envelope can be valid while the generated message is invalid
JSON or violates the application's schema. This is the first delivery of #8257:
gateway-side validation of the **generated content field** against an
operator-managed JSON Schema, a bounded classification vocabulary, and an
explicitly configured, budgeted repair regeneration. Streaming strict-validation is
explicitly out of scope (a separate reviewed release contract); this delivery
covers non-streaming openai-chat completions only.

## Enabling (both switches opt-in, default fully off)

The policy rides the selector handle (`AiProxyHandle.structuredOutput`) through the
existing configuration pipeline:

| Field | Meaning | Default |
|---|---|---|
| `schema` | operator-managed JSON Schema (Draft 2020-12, bounded dialect); its presence enables validation | absent (feature off) |
| `repairEnabled` | whether repairable failures regenerate | `false` (reject only) |
| `repairPrompt` | the operator's repair instruction; required when repair is enabled | — |
| `repairMaxAttempts` | bounded regeneration budget | `1`, hard-capped at `3` |
| `repairMaxTokens` | upper bound applied to `max_tokens` of a repair generation (the provider dialect may rename the field, e.g. `max_completion_tokens`) | `1024` |

Misconfiguration fails fast (unsupported schema, repair without prompt, out-of-range
budget) — nothing silently degrades. Without a schema the response bytes are
forwarded byte for byte.

## Classification → outcome

Every classification is derived from protocol fields (`finish_reason`,
`tool_calls`, content presence) or the bounded schema validator — never a semantic
judgement. Validation itself is local work and is never recorded as another
upstream attempt.

| Classification | Signal | Outcome | Repairable |
|---|---|---|---|
| VALID | content parses and satisfies the schema | forwarded, bytes unchanged (buffered once for inspection) | — |
| TOOL_CALLS_PRESENT | `finish_reason: tool_calls` or message `tool_calls` | forwarded untouched — the client's workflow decision | never |
| INVALID_JSON | content field present but not parseable | REJECT (HTTP 502, reason `CONTENT`) | yes |
| SCHEMA_MISMATCH | bounded validator rejects the content | REJECT (HTTP 502) | yes |
| SCHEMA_BUDGET_EXCEEDED | content exceeds the bounded validator's size/complexity budget — it may be perfectly valid, just too large to validate | REJECT (HTTP 502) | no — a regeneration of a merely oversized document wastes billable work |
| MALFORMED_ENVELOPE | outer response is not a parseable chat completion | REJECT (HTTP 502) | no |
| EMPTY | content absent or blank | REJECT (HTTP 502) | no |
| TRUNCATED | `finish_reason: length` | REJECT (HTTP 502) | no |
| REFUSAL | `finish_reason: content_filter` | REJECT (HTTP 502) | never — a refusal is not a formatting error |

Disabled or unmatched governance behavior, streaming responses, and
Responses-protocol traffic are untouched.

## Bounded repair

A repairable failure regenerates by re-preparing the original request with one
appended user message: the operator's `repairPrompt` (the only instruction source),
the authoritative schema, the bounded classification evidence, and the previous
output as clearly labeled **data** — model text cannot inject repair instructions.
`stream` is forced off, model/provider references are preserved, and
`repairMaxTokens` clamps the generation's token bound. The repair re-asks the
upstream that produced the failing response (primary after a primary failure,
the fallback after a fallback failure). Tool-bearing traffic — the `tools` array
as well as the legacy `functions`/`function_call` shapes — never enters repair.

Refusal detection is deliberately protocol-level only (`finish_reason:
content_filter`): a refusal delivered as plain text with `finish_reason: stop`
is a semantic judgement owned by the semantic-safety boundary (#7623), not
something the gateway guesses — such traffic is classified like any other
content, never specially repaired.

Every actual regeneration is a correlated billable attempt: it has its own
`Phase.REPAIR` attempt identity (`AiAttemptEvent.Kind.REPAIR`), its own usage tap,
and it never draws on the network retry counter. Transport or status failures of a
repair terminate the exchange instead of being retried. All repair work happens
inside the shared request deadline (the same gate the fallback path uses); when the
deadline is exhausted no repair is launched. Validation and repair both observe the
initial and post-repair outcomes as separate attempt events
(`CONTENT_REPAIR`, `CONTENT_REPAIR_FAILED`), so a repair never erases the initial
reliability signal.

## Capability semantics

Provider-native strict output stays the first choice: `response_format`
declarations are already capability-gated (`JSON_MODE`/`JSON_SCHEMA`) by
`AiProviderCapabilities.validatePayload`. The gateway-side validator is a
compensation layer for providers whose JSON mode does not enforce a schema — it is
never a downgrade of an unsupported requirement, and unsupported schemas fail fast.

## Schema validation dialect

Validation reuses the shared `BoundedJsonSchemaValidator`
(`shenyu-plugin-base`): Draft 2020-12 only, explicit keyword allow-list, hard
budgets (schema 512 nodes / 64 KB, document 2048 nodes / 64 KB, arrays 128,
composition depth 2), no remote or file references, fail-closed. The same
validator serves the MCP agent gateway's output-schema filtering.

## Follow-ups

Streaming strict-validation/holdback, Responses-protocol coverage, budget
reservation integration (attempt count and optional `max_tokens` clamp already
bound the spend), and finer content-type handling for multimodal messages.
