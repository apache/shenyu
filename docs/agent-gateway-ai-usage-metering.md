# Agent Gateway AI usage metering

This contract is shared by terminal observation, audit delivery, and the future
cost ledger/budget consumers. It observes the raw provider response exactly once
before protocol conversion; consumers must not subscribe to or parse the body
again. The existing AI Proxy adapters and response tap remain the only parser.

## Per-attempt facts

Each actual upstream attempt emits one immutable `AiAttemptEvent` with:

- trusted logical-request identity and the selected target;
- attempt number and kind (`PRIMARY`, `RETRY`, `FALLBACK`);
- provider, client protocol, provider-native target protocol, and
  requested/resolved model when known;
- streaming mode, bounded outcome, upstream HTTP status, and duration;
- one final usage snapshot for that attempt.

Repeated attempt completion is idempotent. Cancellation freezes the active
attempt and ignores late usage. Callback failures cannot change traffic.
Identifier/model strings are event facts, not unrestricted metric labels.

The current routed inventory is:

| Route | Buffered | Streaming | Usage extraction |
| --- | --- | --- | --- |
| OpenAI Chat native | JSON | SSE | `prompt_tokens`, `completion_tokens`, `total_tokens`, cached-input fields, bounded provider `*_tokens` fields |
| OpenAI Responses client → Chat upstream | converted JSON | converted SSE | observed from the pre-conversion Chat response, so converted output cannot fabricate usage |
| OpenAI Responses native | JSON | SSE | native input/output and nested terminal response usage |
| Anthropic Messages native (explicit opt-in) | JSON | SSE | message_start and sparse cumulative message_delta updates |
| Gemini |  |  | no routed provider in this baseline; unsupported rather than claimed |

OpenAI-compatible providers and DeepSeek reuse the Chat dialect. DeepSeek's
`prompt_cache_hit_tokens` is normalized to the cached-input fact. Other bounded,
nonnegative provider-specific `*_tokens` fields remain provider-labelled facts.

## Usage semantics

`AiUsage` distinguishes provider-reported, estimated, unavailable, invalid,
unsupported, and bounded-capture-exceeded values. A gateway estimate is a valid
contract state but is not emitted by the current routed parsers. Missing means
`null`, never zero; a provider-reported zero remains zero.

Current supported routes use cumulative snapshots: for SSE, a later complete
usage event replaces the earlier snapshot instead of being summed. No supported
route claims incremental semantics; adding one requires an explicit adapter and
per-protocol aggregation rule before values are summed.

Logical terminal events retain the last attempt snapshot and a bounded
cross-attempt summary. The summary counts attempts with and without a reported
snapshot, sums each independently reported category with signed-long overflow
marked explicitly, and reports category/report coverage. Per-attempt events are
authoritative; the future ledger must use them as its price inputs.

This contract does not own prices, currency, invoices, reservations, budget
debit, or settlement. Those remain in their dedicated issues and consumers.

Native Anthropic Messages is explicitly enabled with `shenyu.plugins.ai.proxy.anthropic.enabled=true` and selected with `protocol=anthropic-messages`. It shares the existing provider-profile transport and credentials, without Chat/Anthropic conversion or a separate SDK. Its ambiguous message envelope is excluded from automatic protocol detection. Input, cache-read and cache-write remain separate provider facts. Sparse cumulative updates replace present values, including zero, rather than summing repeated events. Existing attempt/terminal constructors stay available; they do not invent missing native-route facts or aggregate totals.
