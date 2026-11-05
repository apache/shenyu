<!--
Licensed to the Apache Software Foundation (ASF) under one or more
contributor license agreements. See the NOTICE file distributed with
this work for additional information regarding copyright ownership.
The ASF licenses this file to You under the Apache License, Version 2.0
(the "License"); you may not use this file except in compliance with
the License. You may obtain a copy of the License at
    http://www.apache.org/licenses/LICENSE-2.0
Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.
-->

# AI usage ledger

The optional `shenyu-spring-boot-starter-ai-usage-ledger` records normalized AI
attempt observations and operator-price estimates. It does not reserve, debit or
settle a token budget. It does not claim to be a provider invoice.

## Enable and price

Add the starter dependency at the same ShenYu version as the application.
It is disabled by default. Example application YAML (prices below are fixtures,
not a provider quotation):

```yaml
shenyu:
  ai:
    usage-ledger:
      enabled: true
      namespace: production
      queue-capacity: 1024
      store-capacity: 10000
      retention: PT24H
      exports-enabled: false
      prices: >-
        [{"version":"example-v1","source":"operator-approved-example",
          "provider":"openai","model":"deployed-model",
          "clientProtocol":"openai-chat","currency":"USD",
          "from":"2026-10-31T00:00:00Z",
          "inputPerMillion":2,"outputPerMillion":10}]
```

Fixed input/output estimates cover OpenAI Chat (`openai`, `openai-chat`),
OpenAI Responses (`openai`, `openai-responses`) and native Anthropic Messages
(`anthropic`, `anthropic-messages`), for JSON and SSE. Install a separate schedule
for each provider/protocol/model; missing schedules do not fall back to Chat
prices. There is no built-in vendor price list. The actual selected model must
match exactly. Each rate has a source, immutable revision,
ISO currency and inclusive `from` / optional exclusive `until`. Overlapping
schedules for the same provider/model/client protocol fail before installation.
Prices are decimal amounts, not binary floating point. Configuration rejects
unknown fields, duplicate JSON fields, trailing input and invalid rates.

An application can supply a `LedgerPriceBook` bean and atomically replace its
snapshot. Up to 64 active schedules and 1024 distinct revisions per book lifetime
are retained; a removed revision cannot be reused with different content.
No live configuration HTTP endpoint is installed. Persist revision governance
outside the process when it must survive restart.

Every priced row freezes its original version, source, rates, effective interval,
lookup time, currency and quantities. Changing the book never rewrites rows.
`UsageLedgerService.revalue` requires an application-authorized administrative
scope and an explicit valuation revision/time; it appends a separate
`REVALUATION` referencing the original. There is no HTTP mutation route.
Read permission alone is not permission to invoke revaluation or deletion.

## Evidence and populations

Schema version 1 records have three purposes:

- `ATTEMPT`: one actual started primary, retry or fallback attempt. Only these
  rows contribute original token and cost totals.
- `LOGICAL`: one request terminal snapshot with expected attempt count, for
  coverage/correlation. Its last-attempt usage is deliberately not billed again.
- `REVALUATION`: an explicit alternative valuation; excluded from original totals.

The idempotency key hashes length-prefixed purpose, namespace, tenant,
**gateway-generated internal request ID** and attempt sequence. This is not an
RPC ID, wire request ID, trace ID, reservation ID or authentication credential.
Applications creating observation contexts must supply a fresh internal ID for
each logical execution. Redelivery must preserve that ID and sequence. Same ID
with differing facts is a conflict, never an overwrite.

The recorder consumes existing `AiAttemptEvent` / `AiTerminalEvent` hooks.
It neither reads bodies nor subscribes to a response. Existing
`AiUpstreamUsageTap` collects upstream usage before protocol response conversion.
This module adds no JSON/SSE usage parser and does not copy the unmerged token
reservation implementation from #7848. This draft depends on #7955 for the
shared normalized usage and actual route contract, and on #8002 for its audit baseline.
`LedgerUsageResolver` accepts only cumulative snapshots: estimates retain their
estimated origin and incremental/unknown aggregation is explicitly unsupported.

The engine passes the actual upstream target protocol to that same tap before
client conversion. Chat reads `prompt_tokens` / `completion_tokens`; Responses
reads `input_tokens` / `output_tokens`, including nested terminal
`response.completed`, `response.incomplete` and `response.failed` SSE events.
Provisional Responses events are not additional charges.

Anthropic JSON reads native `usage`. Streaming reads `message_start.message.usage`
and sparse cumulative `message_delta.usage`: a present count replaces its prior
value, including zero; absent fields preserve the preceding native snapshot.
Counters are never added across repeated stream events. Missing fields remain
unknown. The four-argument `AiUsage` constructor stays available; cache-read and
bounded extra-category facts are compatible metadata extensions. This does not
create an additional rollup debit or replace #7911's Token Limiter settlement path.

Anthropic `input_tokens` counts regular, uncached input; cache-read and
cache-write are separate quantities. OpenAI input includes its cache subset.
Provider input subtotals retain these documented meanings, not a universal
cache-inclusive denominator. Cache counts are not implicitly added to input
or total, and a missing provider total is never synthesized.

Enable native Messages with `shenyu.plugins.ai.proxy.anthropic.enabled=true`.
Select `provider=anthropic` and `protocol=anthropic-messages` explicitly in the
existing AI Proxy configuration, with a provider base URL, model, credentials
and positive `maxTokens` override or native `max_tokens` request field.
The profile reuses the HTTP transport, `/v1/messages`, `x-api-key` and
`anthropic-version: 2023-06-01`, without another SDK or credential channel.
The ambiguous Messages envelope is excluded from automatic body-only detection,
so enabling it preserves existing Chat routing. Native responses pass through
unchanged. Chat-to-Anthropic conversion, native tool-bearing requests and beta
endpoints are outside this profile.

Additional fixture price entries (examples, not vendor prices):

```json
[
  {"version":"responses-example-v1","source":"operator-approved-example",
   "provider":"openai","model":"deployed-responses-model","clientProtocol":"openai-responses",
   "currency":"USD","from":"2026-10-31T00:00:00Z","inputPerMillion":4,"outputPerMillion":20},
  {"version":"anthropic-example-v1","source":"operator-approved-example",
   "provider":"anthropic","model":"deployed-claude-model","clientProtocol":"anthropic-messages",
   "currency":"USD","from":"2026-10-31T00:00:00Z","inputPerMillion":7,"outputPerMillion":30}
]
```

The protocol semantics are grounded in the official
[OpenAI response usage model](https://github.com/openai/openai-python/blob/main/src/openai/types/responses/response_usage.py),
[Anthropic usage model](https://github.com/anthropics/anthropic-sdk-python/blob/main/src/anthropic/types/usage.py)
and [Anthropic cumulative delta model](https://github.com/anthropics/anthropic-sdk-python/blob/main/src/anthropic/types/message_delta_usage.py).

| Observation | Ledger representation |
| --- | --- |
| Provider reports input/output zero | `REPORTED` / `PROVIDER_REPORTED`, zero quantities; zero estimate only with a matching price |
| Missing/malformed/unsupported usage | Null quantities with explicit quality; null row amount |
| Missing price/model/category contract | `UNKNOWN_PRICE` or `UNSUPPORTED_CATEGORY`; null row amount |
| Explicit gateway quantity estimate | `ESTIMATED` / `GATEWAY_ESTIMATE`; never relabeled provider-reported |
| Provider quantities plus operator rate | Monetary `ESTIMATED`, not invoice-reconciled cost |
| Terminal/cache-served request with no attempts | No new observed provider work; not proof of zero tool/retrieval/task cost |

All actual attempt work is counted once, including measured usage in failed
primary responses discarded before retry. Fallback cost is the declared
`FALLBACK` subset; total request cost still includes failed primary/retry work.
Reported total tokens are evidence, not an additional input/output charge.
An actual future repair generation must have its own attempt identity; no repair
is synthesized from validation failures or delivery retries.

`possibleUnobservedConsumption` is conservative: missing retained attempt/terminal
coverage, unavailable/estimated usage, failed/cancelled attempts or interrupted
logical outcomes set it. Streaming attempts also set it because the current
shared observation contract has no proof of a final usage frame. In a real
client disconnect the existing HTTP writer may terminate with a source
`SUCCESS` snapshot; preserve that evidence, do not infer successful client
delivery or final billing from the assigned HTTP status. Received quantities and
a computable estimate remain visible, with exposure explicit.

Provider cached tokens, long-context tiers, negotiated discounts, taxes, FX,
MCP tool charges and invoices are not priced by this initial fixed-rate contract.
Positive reported cached quantities or positive additional categories produce
`UNSUPPORTED_CATEGORY`; they are not silently charged at a regular rate.
Long-context cohorts need a declared model capacity or explicit measured-token
threshold. Byte length is not a token count, and no automatic long-context ratio
is invented. Exact/semantic gateway cache hits never replay an answer's old
usage as new provider work.

## Trusted attribution and observers

The default `LedgerAttributionResolver` takes namespace only from deployment
configuration and tenant/subject, selected model, operation and configuration
generation from the trusted request context. Missing namespace stays null.
Application, feature and internal API-key ID remain unknown unless an application
resolver supplies them. Attempt requested/resolved model and native protocol come
from the actual shared route snapshot; absent route facts stay unknown. Client
protocol is recorded separately and is not relabeled as native protocol. Applications with trusted richer
metadata can supply an explicit resolver; do not derive identity from headers,
payloads, trace metadata or raw credentials. Application namespaces must be
authoritative, especially if multiple ShenYu namespaces share a deployment.

Only bounded immutable metadata enters the ledger: no prompts, response text,
tool arguments, cookies, authorization values, provider key or raw exception
messages. A resolver is a trusted code extension, not a general redaction filter;
it must never put secrets into metadata fields.

The starter installs one explicit `AiCallObserver.Composite` that records once
and forwards once to each identity-distinct existing application observer
(maximum 16). AI Proxy and Agent Gateway select that composition explicitly,
without a hidden `@Primary`. Ordinary ambiguous observers or multiple canonical
composites still fail configuration. Callback exceptions do not change business
responses. This preserves existing audit/metrics observers and uses the #8002
audit bridge from the declared baseline without creating another audit transport.

## Delivery, bounded storage and loss

Delivery reuses `AbstractLogCollector` and its `LoggingQueueMetrics`, including
existing queue depth/capacity/admission/drop metrics. There is one bounded queue
(default 1024, range 1–16384), not a second broker/exporter framework. Sink calls
run on the existing logging worker rather than the gateway event loop.

`UsageLedgerCollector.failedRecords()` counts store failures/full/conflicting
records; duplicates are successful idempotent delivery.
`AiUsageLedgerRecorder.invalidRecords()` counts rejected metadata mappings and
`LedgerObserver.failedDelegateCallbacks()` counts failing application callbacks.
These operator APIs and existing queue metrics are local diagnostics, not
tenant-visible global counters. Do not add tenant/model identifiers as
unbounded metric labels.

The default `InMemoryUsageLedger` is a **volatile bounded reference sink**:
1–32768 rows (default 10000), TTL 1 minute–7 days (default 24 hours), monotonic
expiry, atomic append/dedup/conflict checks. Capacity rejects new rows instead
of evicting live idempotency entries. Duplicates do not refresh retention.
Idempotence lasts only while the original is retained: expiry, explicit deletion
or restart ends that guarantee. Namespace/tenant/app-scoped deletion does not
affect another tenant.

Delivery is **best effort**, without retries, WAL, durable acknowledgement or an
exactly-once promise. Overflow, mapping failure and sink rejection lose records.
A crash may lose queued rows and the default entire store; no process-local
counter can reconstruct that exact loss. The opaque store epoch changes after
restart and is returned with exports, enabling an external monitor to detect
reset. Persist exported checkpoints externally if reset visibility is required.
An empty retained view is not proof of no historical consumption.

Shutdown uses the existing collector close/flush behavior. Applications must
close producers before the collector; late observations are not a durable
handoff. Custom `UsageLedgerStore` implementations must enforce bounded I/O,
retention, query caps and atomic idempotency, honor their own timeout/shutdown,
and own external connection lifecycle. The existing synchronous shutdown flush
does not guarantee a bounded close against a blocking custom store. A durable
billing database or crash recovery is not supplied by this starter.

## Authorized query/export

`UsageLedgerService.export(scope, limit)` returns at most 1000 rows and summaries
over at most 32768 retained authorized matches. It returns match count,
`truncated`, `retainedViewComplete` and store epoch. A capped result is not an
uncapped historical total. Token subtotals use `BigInteger`, money uses
`BigDecimal`. Currency groups are never summed through implicit FX.
`UNPRICED` is a missing-currency group, not an ISO currency.

Cost summaries include original-attempt counts, known estimated subtotal,
unknown-attempt count and the fallback subset. They also break down the same
retained facts by authorized namespace/tenant/application/feature and actual
provider/model/client protocol/currency. Missing dimensions remain null.
These are bounded query groups, not metric series. A known subtotal of zero
with unknown attempts must not be displayed as zero total cost.

A scope requires an authoritative namespace and authenticated tenant. Optional
application null explicitly grants all applications in that tenant, not all
tenants. Normal scopes cannot read unattributed rows. Store diagnostics and
custom-store administration belong to the operator, not this route.

Optional GET `/shenyu/ai/usage-ledger?limit=100` is enabled only with
`exports-enabled=true` and an application-provided `LedgerExportAuthorizer`.
The application must establish an authenticated WebFlux `Principal` and map it
to the server-owned scope. There is no default all-tenant authorizer.
Missing principal returns 401, denied scope 403, invalid/unknown query parameters
400, and backend failure a sanitized 503. The route accepts no tenant/namespace
selection from headers or query arguments. Authentication adapters and browser
security policy remain application-owned.

## Reproduction

Run from the repository with JDK 17:

```text
./mvnw -B -ntp -pl shenyu-spring-boot-starter/shenyu-spring-boot-starter-ai-usage-ledger -am test -DskipRemoteResources=true
```

`ledger-reconciliation.json` contains fixed expected totals independent of
network or live prices. Tests cover primary/retry/fallback, duplicate delivery,
zero/unknown quantities, absent prices, cancellation, frozen historical revisions
and independent revaluation. Additional tests cover concurrent deduplication,
TTL/capacity, sink failure/queue overflow, observer composition, authenticated
tenant-isolated HTTP exports and the real existing WebClient/AI Proxy JSON/SSE
path compared with the ledger disabled.

These are deterministic local upstream fixtures, not a paid provider invoice
reconciliation or a production traffic benchmark.
