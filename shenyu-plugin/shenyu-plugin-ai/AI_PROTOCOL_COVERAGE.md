# AI Governance Plugin × Protocol Coverage (issue #8458)

Which governance read path understands which client/upstream protocol shape.
Name provenance (deliberate, not a single namespace): **openai-chat** and
**openai-responses** are the `AiProxyProtocol.name()` values the proxy can
produce today; **anthropic-messages** and **gemini-generate-content** are the
PII profile ids and request-view hints — they are groundwork for #8457 / #8388
and no `AiProxyProtocol` implementation emits them yet, so the usage-tap merge
path keyed on `anthropic-messages` is reachable only once that protocol lands:

- **openai-chat** — OpenAI Chat Completions
- **openai-responses** — OpenAI Responses
- **anthropic-messages** — Anthropic Messages
- **gemini-generate-content** — Gemini generateContent / streamGenerateContent

Coverage levels: **full** (parsed and governed), **partial** (see note),
**none** (explicitly reported as a gap — never silently treated as covered).

| Plugin / component | openai-chat | openai-responses | anthropic-messages | gemini-generate-content |
|---|---|---|---|---|
| Usage metering (`AiUpstreamUsageTap`) | full | partial¹ | groundwork² (split-event merge is unreachable until an anthropic `AiProxyProtocol` exists, #8457/#8388) | partial³ (`streamGenerateContent` without `alt=sse` returns a JSON array and is unmetered) |
| Token limiter (output accounting) | full | full (`response.usage`) | groundwork⁴ (stream fields unreachable until #8457/#8388; non-streaming read is live where shapes match) | partial⁵ (non-SSE array shape unmetered) |
| PII request masking | full | full | full (system, text blocks, `thinking`, `tool_use.input` incl. arrays) | full (`systemInstruction`, `parts[].text`, `functionCall.args`) |
| PII response masking | full | full | full (`content[]` blocks incl. `thinking` and `tool_use.input`) | full (`candidates[].content.parts` incl. `functionCall.args`) |
| Normalized request view | full | partial (structural metadata) | full (system → leading system message; text blocks → TEXT) | full (`systemInstruction`/`contents` parts → TEXT; model is URL-path only, stays null) |
| Sensitive word scanning | full | full | full | full (shape-independent: every string member is walked) |
| Structured output validation | full | — | none⁶ | none⁶ |

Notes:

1. Responses bodies carry no OpenAI-Chat usage fields; their usage surfaces only
   through the token limiter's terminal-event path — the tap records `UNKNOWN`.
2. Anthropic: `usage.input_tokens` → prompt, `usage.output_tokens` → completion;
   **no total is ever invented** (the protocol has no total). Stream events:
   `message_start.message.usage` (input) and cumulative `message_delta.usage`
   (output) replace the snapshot — values are never summed. `message_stop`
   provides the stream-completion marker (Anthropic has no `[DONE]` sentinel).
   `cache_read_input_tokens`/`cache_creation_input_tokens` are not read today,
   so cached prompt segments meter at ~0 extra prompt tokens — documented gap,
   revisit with the Anthropic protocol work.
3. Gemini: `usageMetadata.promptTokenCount/candidatesTokenCount/totalTokenCount`,
   replacing the snapshot on every chunk that carries it (the final chunk is
   authoritative). Gemini SSE has no terminal marker; stream-completion evidence
   stays unset rather than guessing. `streamGenerateContent` without `alt=sse`
   returns a JSON array, which the object-root readers reject — that shape is
   unmetered (documented `partial`).
4. Anthropic non-streaming already matched the OpenAI `usage`/`output_tokens`
   field names; stream events (`message_start`/`message_delta`) are now read
   explicitly. Last event wins, matching the cumulative counters.
5. Gemini `usageMetadata.candidatesTokenCount`; every stream chunk re-carries
   `usageMetadata`, last event wins.
6. Structured output validation is a deliberate openai-chat feature
   (`AI_STRUCTURED_OUTPUT.md`); other protocols fail the policy-open path
   explicitly rather than being validated against an incompatible schema.

Explicit-gap reporting: a payload whose shape no profile matches is never
governed silently — PII reports "no supported protocol profile matches the
payload" and follows the rule's fail-open/fail-closed policy, the request view
publishes an `unsupported` view with a reason, and usage metering records
`UNKNOWN`/`UNSUPPORTED` on the attempt observation.

Protocol detection is intentionally not single-sourced today, and the
divergence is deliberate and visible: the request view uses `AiClientProtocolShape`
(ai-common), PII uses `PiiProtocolProfiles.select`'s validity-based matching,
and the proxy uses `AiProxyProtocol.matches`. They already disagree on one
shape: `{"messages":[...],"system":"s"}` is anthropic-messages for the view
and PII, but openai-chat for the proxy. For the view, an Anthropic request
without a top-level `system` is structurally indistinguishable from openai-chat
and is classified as such — the view then reports it as STRUCTURED, so its text
blocks are **not** surfaced as text; PII masking and sensitive-word scanning
govern the payload shape-independently regardless of that classification.

Follow-up: the byte-level SSE framing duplicated between `AiUpstreamUsageTap`
and the token limiter's `ResponseUsageExtractor` could be extracted into one
ai-common decoder once a third consumer appears.
