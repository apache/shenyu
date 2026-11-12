# AI Evaluation Sample Export & Feedback Joins (issue #8256)

A bounded, explicitly authorized pipeline that exports versioned evaluation
samples of an operator-owned LLM workload and joins authenticated feedback to
them. This is the first delivery of #8256: **metadata-only by construction** — no
prompt, completion or other request content is ever captured — with an
asynchronous exporter that cannot delay or alter a business response.

## Enabling (two layers must opt in)

| Layer | Switch | Default |
|---|---|---|
| Gateway-wide | `shenyu.plugins.ai.evaluation.enabled` | `false` — no manager, no exporter thread, no state |
| Per route (selector handle `evaluation`) | `enabled`, `sampleRate` ("1" = all), `includeIdentity` | `false` — the route produces no samples |

Sampling uses a deterministic 100-slot probability gate (same mechanism as the
logging family's count sampler); a rate outside `[0, 1]` is rejected at
construction. `includeIdentity` independently authorizes the trusted tenant,
subject and target fields; without it they are never exported (the recorded
tenant still authorizes joins internally, see below).

## What a sample carries (contract `eval-sample-v1`)

Body-free metadata only: opaque `sampleId` (gateway-minted, no authorization
semantics), request/selector/rule ids, configuration version, terminal outcome,
http status, elapsed millis, attempt count, and the provider-reported usage
figures with their quality status — unknown facts are exported as `null`, never
as zero or an invented value. There are no prompt/model/policy version slots
yet: no source for them exists in the gateway, so none is fabricated; the slots
land with the #8051 integration. The sample never carries request content and
never holds a reference to the exchange — the exporter retains only immutable
versioned records.

## Export (bounded, asynchronous, drop-visible)

`record()` on the request path is O(1): a cached route lookup, a sampler check
and a bounded-queue offer — an overflow **drops and counts** (accepted/dropped
counters), never blocks. A dedicated daemon thread drains the queue in batches
into the sink; a failing sink drops its batch rather than replaying, and the
exporter never grows beyond its capacity. The default sink writes one structured
line per record (`AI_EVALUATION|type|schemaVersion|payload`) for an
operator-owned offline runner; an application-owned sink replaces it
(`@ConditionalOnMissingBean`), and ES/Kafka adapters are follow-ups on the same
interface.

## Feedback joins (contract `eval-feedback-v1`)

`joinFeedback` authorizes through the opaque sample reference **plus** the tenant
the gateway recorded for that sample — a request id is never an authority:

- unknown, forged or expired reference → `UNKNOWN_SAMPLE` (nothing is created)
- tenant mismatch → `TENANT_MISMATCH`
- blank ids or a label set beyond 8 entries / 256 chars per value → `REJECTED_INVALID`
- same `judgeId` again → idempotent label update (`UPDATED`); distinct judges
  append up to the per-sample bound (default 4) → `REJECTED_BOUND` beyond it

Accepted and updated joins are exported through the same bounded pipeline.
The join registry is bounded (`registryCapacity`) and its entries expire with
`joinRetentionMillis` (default 30 min) — feedback that arrives after expiry is an
explicit `UNKNOWN_SAMPLE`, and "missing" versus "pending" versus "joined" stays
distinguishable by the runner from the exported event stream.

## Behavior guarantees

With the feature disabled the gateway holds no evaluation state and its behavior
is byte-identical to today. With it enabled, overload, sink failure,
cancellation, malformed feedback and duplicates preserve the business response
and keep all state bounded. No additional upstream or model call is ever made to
sample or collect feedback.

## Follow-ups

Content capture behind an independent per-route allow-list with redaction
(deliberately not started: metadata-only here), ES/Kafka sink adapters, prompt
model and policy version references (#8051/#8052), streaming samples, and
content hashing with scoped keying — none of it implicitly enabled by logging or
audit configuration.
