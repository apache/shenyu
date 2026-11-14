# AI Governance Plugin × Protocol Coverage (issue #8458)

Which governance read path understands which client/upstream protocol shape.
Protocol names are the ones used across the AI plugins (`AiCommonConfig.protocol`,
`AiProxyProtocol.name()`, profile ids, view protocol hints):

- **openai-chat** — OpenAI Chat Completions
- **openai-responses** — OpenAI Responses
- **anthropic-messages** — Anthropic Messages
- **gemini-generate-content** — Gemini generateContent / streamGenerateContent

Coverage levels: **full** (parsed and governed), **partial** (see note),
**none** (explicitly reported as a gap — never silently treated as covered).

| Plugin / component | openai-chat | openai-responses | anthropic-messages | gemini-generate-content |
|---|---|---|---|---|
| Usage metering (`AiUpstreamUsageTap`) | full | partial¹ | full² | full³ |
| Token limiter (output accounting) | full | full (`response.usage`) | full⁴ | full⁵ |
| PII request masking | full | full | full (system, text blocks, `tool_use.input`) | full (`systemInstruction`, `parts[].text`) |
| PII response masking | full | full | full (`content[]` blocks) | full (`candidates[].content.parts`) |
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
3. Gemini: `usageMetadata.promptTokenCount/candidatesTokenCount/totalTokenCount`,
   replacing the snapshot on every chunk that carries it (the final chunk is
   authoritative). Gemini SSE has no terminal marker; stream-completion evidence
   stays unset rather than guessing.
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

Protocol detection for the request view is single-sourced in
`AiClientProtocolShape` (ai-common): `messages` + top-level `system` →
anthropic-messages, `messages` → openai-chat, `input` → openai-responses,
`contents` → gemini-generate-content. An Anthropic request without a top-level
`system` is structurally indistinguishable from openai-chat and is classified
as such (its text blocks still surface through the openai-chat readers).

Follow-up: the byte-level SSE framing duplicated between `AiUpstreamUsageTap`
and the token limiter's `ResponseUsageExtractor` could be extracted into one
ai-common decoder once a third consumer appears.
