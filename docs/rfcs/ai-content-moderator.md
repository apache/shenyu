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

# RFC: judge-agnostic AI content moderation

Status: **proposal for discussion**, not an implemented plugin or an approved API.
Issue: [#7623](https://github.com/apache/shenyu/issues/7623).
Source baseline: `69ed0a256`.

Issue #7623 asks for design agreement before implementation and Admin registration.
This RFC makes the policy, streaming, and resource contracts reviewable first.
The module names, fields, defaults, and rollout stages below are proposals.

## Decisions requested

| Decision | Recommended starting point | Consequence / alternative |
| --- | --- | --- |
| Name | `aiContentModerator`, module `shenyu-plugin-ai-content-moderator` | One plugin with judge adapters, rather than one plugin per moderation vendor. |
| Order | Default `196`, subject to rechecking at implementation time | Default order precedes `sensitiveWord` (197), `agentGateway` (198), and `aiProxy` (199). Admin sort overrides must preserve response-decoration order. Dictionary-rejected requests can incur a judge call. |
| Availability policy | `failClosed=false`, explicit rule override | Unavailable judgements may pass traffic, but must remain visibly uninspected. Strict enforcement requires `failClosed=true`. |
| Enforcement action | `block` by default; `log` for observation rollouts | `log` records a risky result without changing the original payload. |
| Streaming default | `buffered`, with finite byte and text limits | Wait for the complete bounded response. Reject overflow instead of silently inspecting only a prefix. |
| Initial protocol | OpenAI Chat Completions text; plain HTTP request text | Other wire profiles require their own extraction and refusal fixtures before being enabled. |
| Circuit breaking | Timeout and bounded admission first | A shared breaker can follow measured demand; installing the Resilience4j plugin does not automatically put its classes on this plugin's classpath. |

Maintainers should resolve this table before the first runtime implementation PR.
Changing one choice requires updating the examples and acceptance cases together.

## Current integration points

These are observations about the baseline, not new capabilities:

- [PluginEnum](../../shenyu-common/src/main/java/org/apache/shenyu/common/enums/PluginEnum.java)
  assigns `sensitiveWord=197`, `agentGateway=198`, and `aiProxy=199`; 196 is unused.
- [ShenyuWebHandler](../../shenyu-web/src/main/java/org/apache/shenyu/web/handler/ShenyuWebHandler.java)
  sorts by `PluginData.sort` when present, falling back to `getOrder()`, and
  invokes the reactive chain. An enum value alone does not enforce relative
  order. The moderator must install its response decorator before invoking the
  downstream response producer.
- [AiProxyPlugin.writeResponse](../../shenyu-plugin/shenyu-plugin-ai/shenyu-plugin-ai-proxy/src/main/java/org/apache/shenyu/plugin/ai/proxy/enhanced/AiProxyPlugin.java)
  writes the engine's response through `writeAndFlushWith`. Supporting only
  `writeWith` would leave this path unmoderated.
- [ServerWebExchangeUtils.rewriteRequestBody](../../shenyu-plugin/shenyu-plugin-base/src/main/java/org/apache/shenyu/plugin/base/utils/ServerWebExchangeUtils.java)
  reads a String and replays it through a decorator. It has no per-call byte-limit
  argument; using it alone would not enforce this RFC's request limit.
- [ResponseBodyCaptureUtils](../../shenyu-plugin/shenyu-plugin-ai/shenyu-plugin-ai-common/src/main/java/org/apache/shenyu/plugin/ai/common/utils/ResponseBodyCaptureUtils.java)
  currently returns an empty String. It cannot supply a response for moderation.
- [OpenAiProtocolAdapter](../../shenyu-plugin/shenyu-plugin-ai/shenyu-plugin-ai-common/src/main/java/org/apache/shenyu/plugin/ai/common/protocol/OpenAiProtocolAdapter.java)
  serves the proxy's request conversion. It is not a general moderation extraction
  contract. Keep moderation independent of proxy-engine implementation classes.

Proposed runtime placement is
`shenyu-plugin/shenyu-plugin-ai/shenyu-plugin-ai-content-moderator`, with its own
starter and a rule-handle DTO in `shenyu-common`. Use ShenYu's existing
`@SPI` / `@Join` discovery conventions. The request-only path must work on a
matched HTTP route without an AI proxy dependency.
The proposed `196` is an order code, not an allocated Admin database plugin ID.
The implementation must validate the effective ordering, including Admin sort
overrides, before advertising response coverage. Reordering the moderator after
the response producer must surface as a configuration/capability error, not a
silently successful response check. Admin seed IDs belong to the follow-up.

## Judge and policy boundaries

`ContentModerationJudge` receives an immutable moderation input and judge settings
and returns a reactive result. The input contains direction, extracted text,
protocol profile, and bounded context. It does not expose `ServerWebExchange`, a
response writer, arbitrary inbound headers, or permission to continue the chain.

The two proposed adapters are:

- `openAiCompatibleJudge`: call a configured chat endpoint; parse a strictly
  configured structured result from the judge's message content.
- `httpApiJudge`: call a configured vendor HTTP endpoint; map configured response
  fields into the same result without importing a vendor SDK or executing scripts.

A judge result describes `safe` or `risky`, an optional bounded category, and an
optional normalized risk score in `[0, 1]`. The rule owns the action. If a rule
sets a score threshold, its adapter must provide a risk score with the documented
meaning; a classification confidence is not automatically a risk probability.
A missing/invalid required score is unavailable, not a safe result. With a
configured threshold, `riskScore >= riskThreshold` determines the risky policy
result; the raw label is diagnostic, not a second vote. Without a threshold,
use the normalized `safe` / `risky` classification. For example, score 0.93 at
threshold 0.8 is risky, and score 0.2 is safe. A label-only judge can be used
with no threshold; it cannot satisfy a rule that requires a numeric score.

An empty publisher, timeout, non-success response, malformed result, or unknown
classification becomes `UNAVAILABLE`. Do not convert judge failures to `PASS`.
The moderator maps the result and rule action as follows:

| Verdict | Trigger | Outcome |
| --- | --- | --- |
| `PASS` | A successful safe judgement | Continue with the original content. |
| `FLAGGED` | A risky judgement with `action=log` | Continue with the original content and record the decision. |
| `BLOCKED` | A risky judgement with `action=block` | Deny before release, or terminate the committed stream as described below. |
| `UNAVAILABLE` | No usable judgement, including admission/call-budget exhaustion | Apply `failClosed`; record the reason even if traffic proceeds. |

No matching content / an unsupported representation is a separate inspection
coverage outcome, not a fifth judge verdict. `unmatched=skip|warn|error` makes the
bypass visible; proposed default is `warn`. No judge call means no `PASS` metric.
A `failClosed=false` bypass remains `UNAVAILABLE` with `coverage=uninspected`.

Keep each exchange on one immutable policy/configuration generation. A rule
update affects new exchanges; it must not change thresholds halfway through a
stream. Removal stops admitting new work and releases retired adapter resources
after their bounded in-flight work finishes or is cancelled.

## Configuration sketch

This example is a proposed rule handle, not configuration supported by a released
version. The endpoint reference resolves to operator-managed plugin/starter
configuration, including credentials; it is not a URL supplied by a caller.

```json
{
  "policyVersion": 1,
  "judge": "openAiCompatibleJudge",
  "judgeModel": "guard-model",
  "judgeEndpointRef": "moderation-primary",
  "protocol": "openai-chat",
  "checkRequest": true,
  "checkResponse": true,
  "action": "log",
  "failClosed": false,
  "unmatched": "warn",
  "denyStatusCode": 403,
  "streamCheckMode": "buffered",
  "judgeTimeoutMillis": 1000,
  "riskThreshold": 0.8,
  "maxRequestBytes": 262144,
  "maxBufferedResponseBytes": 262144,
  "maxTextCodePoints": 32768,
  "maxEventBytes": 65536,
  "maxHeldFrames": 256,
  "maxJudgeCallsPerExchange": 128,
  "maxInspectionDurationSeconds": 120,
  "maxBatchTextCodePoints": 512,
  "maxBatchDelayMillis": 100,
  "maxContextCodePoints": 2048,
  "maxJudgeRequestBytes": 262144,
  "maxJudgeResponseBytes": 16384
}
```

Validate the supported `policyVersion`, enums, positive finite limits, threshold
range, endpoint reference, and extraction paths at configuration admission.
Compile extraction paths once per
policy generation. Reject `checkRequest=false` plus `checkResponse=false` and
`final_packet` plus `action=block`. Reject unsupported profile/mode combinations.
A bad new generation must not silently become a bypass; expose configuration
failure through the established configuration-error path and retain the previous
valid generation where the runtime supports that contract.

The deployment sets hard ceilings; rule limits may lower them but cannot remove
them. The numeric limits above are starting proposals, not benchmark-derived
production defaults. Endpoint admission also needs a finite shared maximum of
concurrent judges (proposed: 16 per configured endpoint) and a finite pending
count, initially zero. Protect concurrently buffered exchanges (proposed: 32
total per gateway) as well: limiting judge calls alone does not bound waiting
response buffers. Both quotas are deployment-wide per gateway,
with configured endpoint identity rather than arbitrary request data as keys.
A rule's batch plus context code points must fit its text budget. Deployment
ceilings also bound endpoint/policy generations retained during configuration
rotation; a stream must not keep creating new adapter state.

## Request processing and extraction

1. Match a rule and capture its immutable policy. Check a declared oversized
   `Content-Length` before subscribing to the body.
2. Count actual bytes for chunked or missing-length bodies before aggregation;
   stop at `maxRequestBytes`, release retained buffers, and cancel upstream reads.
   A header-only size check is insufficient.
3. Decode and extract within the byte and Unicode-code-point budgets. For an
   OpenAI Chat text profile, inspect configured textual message content in role
   order, including text parts in supported content arrays. Do not silently treat
   image/audio/tool-result content as inspected text.
4. Await the request verdict, then invoke the downstream chain at most once and
   replay the original accepted bytes. Moderation does not rewrite a prompt.

Profiles define the roles, fields, arrays, and tool data they cover. A deployment
that chooses only the last user message must be shown that limited scope; it must
not be described as whole-conversation moderation. Custom JSONPath overrides
cannot expand a profile's refusal/streaming support automatically.

The proposed plain HTTP profile is named `text`: request-only, UTF-8
`text/plain` (charset absent or UTF-8), inspecting the entire nonblank text
within its limits. Other media types, charsets, and absent Content-Type are
unsupported representations. It does not guess that arbitrary JSON, binary,
form, or multipart data is text. `text` plus `checkResponse=true` is invalid.
Known image/audio parts or unimplemented tool-call representations in an AI
message apply `unmatched`; they must not be hidden by a successful text judgement.

The initial response profile covers OpenAI Chat message text and incremental
`choices[*].delta.content`. Additional profiles for Responses, Anthropic, and MCP
are implementation follow-ups with explicit text/tool/multimodal coverage. They
must not be enabled through a speculative path table. If content encoding is not
supported by the bounded decoder, apply `unmatched`; never parse compressed bytes
as if they were JSON. A future decoder needs its own decompressed-byte limit.

## Response and streaming contract

Install one module-local response decorator covering `writeWith`,
`writeAndFlushWith`, empty completion, cancellation, and errors. In request-only
mode, leave the response untouched. Do not buffer an unbounded `Flux` or turn a
stream into an unrestricted `collectList` / `join`.

The parser must tolerate arbitrary `DataBuffer` boundaries, split UTF-8 code
points, split SSE lines/events, several events in one buffer, comments, terminal
frames, and a short final tail. Transport chunks are not moderation batches.
All retained raw bytes, decoded text, frame objects, and context count toward
explicit limits. A single oversized event and many content-less events must not
bypass the byte/frame caps.

| Mode | Release boundary | Coverage and exhaustion |
| --- | --- | --- |
| `buffered` | Hold the complete finite response and judge at completion; release the original bytes only after its decision/policy permits it. | Applies to the entire extracted response within the configured caps. Overflow rejects/terminates; inspecting a safe prefix does not authorize the remainder. |
| `realtime` | Serialize batches; each batch includes all its newly extracted text in its own judgement before that batch is released. Bounded prior context may also be supplied. | Coverage is the configured context/batch, not unlimited conversation history. A later denial cannot retract earlier approved batches. |
| `final_packet` | Forward content as it arrives and inspect a bounded retained final window at completion. | Audit-only, explicitly partial/windowed when truncated. It cannot claim prevention or enforce a denial after bytes have been sent. |

For a `final_packet` response, risky/unavailable results, malformed framing, and
inspection-budget exhaustion are observations only: preserve the original
response even with `failClosed=true`. Stop inspection and discard retained audit
state when its limit is reached, while forwarding subsequent original bytes.
Its separately enabled request check still obeys the request enforcement policy.
The final judge call remains part of an owned, timeout-bounded reactive lifecycle;
connection completion may wait for it even after content was visible. Do not
claim zero added completion latency or create an unbounded background audit queue.

For `realtime`, proposed batching closes on the first of a configured text/event
threshold or a bounded batch timer, then waits for its judgement; at most one
judgement per exchange is in flight. Use serial demand/prefetch bounds so a slow
judge cannot accumulate an unbounded queue. Flush the last nonempty batch at end
of stream even when shorter than the usual threshold. Adjacent content-less
frames need no judge call, but their ordering and retained-frame budget still
apply. In enforcing modes, hold leading metadata frames until the first release
decision; for an all-metadata response, apply `unmatched` at completion.

In enforcing modes, raw byte/text/frame overflow rejects by default (413 for a
request or 502 for a response before commit, protocol termination after commit);
it is not a judge-availability error.
Availability, call-budget, or inspection-deadline exhaustion follows `failClosed`.
An explicit fail-open release is uninspected and recorded as such; a previous
batch's `PASS` must never authorize the current batch. Bounded bypass flushing
switches the affected direction into passthrough for the remainder of the
exchange: flush all not-yet-released original bytes, including the pending batch,
its tail, and held metadata, exactly once in order. Never replay an already
released prefix. Stream subsequent bytes without retaining moderation buffers or
starting more judge calls for that direction. Record the bypass transition and
uninspected coverage; it does not change the original wire protocol. Request-side
availability bypass does not disable the separately configured response check.

When a response completes with no inspectable text, `skip`/`warn` releases the
held metadata/terminal frames with no judgement; `error` rejects before commit or
terminates after commit. If text exists, hold its terminal frame until the final
batch is decided. An empty body and a content-less framed response both use this
coverage rule. Comments/heartbeats remain subject to byte/frame/deadline budgets.

Client cancellation cancels body reads and the outstanding judge request,
releases every retained buffer once, and prevents later callbacks from writing.
Use discard handling for queued buffers and test pooled-buffer reference counts.
Do not subscribe/fire-and-forget to judge publishers from a response callback.

## Denial and failure responses

This matrix is normative for request checks and enforcing response modes
(`buffered` / `realtime`). The audit-only `final_packet` response exception above
takes precedence. "Before commit" includes headers: no status change is promised
after they are sent.

| Condition | Request / response before commit | Committed response |
| --- | --- | --- |
| Successful safe judgement, or risky with `action=log` | Preserve and release the accepted original content; record PASS or FLAGGED. | Release only the batch permitted by its own decision. |
| Risky with `action=block` | `denyStatusCode` (proposed 403). | Cancel upstream; profile-specific termination, never retract earlier output. |
| Unavailable judge, admission/call budget, or inspection deadline | `failClosed=true`: 503. Otherwise passthrough with UNAVAILABLE/uninspected. | `failClosed=true`: terminate. Otherwise latch passthrough with UNAVAILABLE/uninspected. |
| Missing text or unsupported representation | `unmatched=skip` or `unmatched=warn`: passthrough, uninspected (warn records a warning). `error`: request 415 / response 502. | `error`: terminate. Otherwise passthrough, uninspected. |
| Malformed recognized JSON, UTF-8, or SSE framing | Request 400 / response 502; no successful judgement. | Terminate; do not call malformed data safe. |
| Raw/text/event/frame capacity exceeded | Request 413 / response 502, independent of `failClosed`. | Cancel upstream and terminate. |
| Client cancellation | Cancel reads/judges and release resources; no new response write. | Same; late judge results cannot write. |

Check declared size first, then cumulative raw size before copying or aggregation, then
framing/decode/text limits, then serialized judge request/response limits before
parsing their contents. If several limits apply to the same data, the first
exceeded limit terminates processing once; it must not allocate past another
ceiling to classify the error. Duration/admission limits never relax byte caps.
An invalid judge response belongs to UNAVAILABLE; invalid protected wire data
belongs to the malformed-input row.

A judge outage is not evidence of forbidden content; do not count an unavailable
fail-closed result as a risky-content denial.

Use the existing response-writing utilities and serialize typed objects. Plain
HTTP errors can use
[ResponsiveException](../../shenyu-plugin/shenyu-plugin-api/src/main/java/org/apache/shenyu/plugin/api/exception/ResponsiveException.java)
and [WebFluxResultUtils](../../shenyu-plugin/shenyu-plugin-api/src/main/java/org/apache/shenyu/plugin/api/utils/WebFluxResultUtils.java).
Recognized AI routes require the profile's error envelope rather than assuming a
generic gateway result matches the SDK protocol. For example, an OpenAI-style
pre-commit rejection can use this proposed error body:

```json
{
  "error": {
    "message": "Content moderation rejected this request",
    "type": "content_policy_violation",
    "code": "moderation_blocked"
  }
}
```

After commit, HTTP status and headers cannot be changed. Stop acquiring model
content, cancel the upstream, and use a tested profile-specific refusal/error
termination if supported. If none can be emitted safely, terminate the response
and record the failure; do not inject a gateway JSON document into SSE or label
an unfinished completion as successful. Refusal framing for each profile is an
implementation gate tested against protocol fixtures, not an assumption about
all OpenAI-compatible vendors. `final_packet` only records the result.

A moderation denial is a terminal policy outcome. It must not look like a
retryable model transport failure, trigger provider fallback, or execute the
business chain a second time. The real proxy-writer fixture must cover those
error/fallback boundaries in addition to testing a standalone decorator.
Generated denial output bypasses the moderator's own inspection path to avoid
recursion. Never echo the matched text, judge prompt, credentials, or raw vendor
response to the client. Remove obsolete content length/encoding/digest headers
when replacing a response, and recompute headers for the actual denial payload.

## Judge transport, limits, and observations

Use a reused reactive HTTP client, one attempt per judgement initially, a finite
response-body cap, timeout, and cancellation propagation. Direct judge calls do
not execute the gateway plugin chain. Endpoint origin and credentials come from
trusted deployment configuration; callers cannot choose them or forward their
Authorization/Cookie headers to the judge. Redirect behavior must not bypass the
configured endpoint boundary. Validate templates and structured result paths
without evaluating code. Budget serialized judge request bytes as well as source
text: escaping and a prompt template can expand the payload.

Parsing/normalization has a bounded input. Any substantial blocking/CPU adapter
work needs explicit bounded isolation at the computation boundary; a
`subscribeOn` before asynchronous HTTP results does not relocate later response
parsing by itself. HTTP I/O needs no unconditional extra scheduler hop. Do not
add a thread pool or breaker per request. There are no implicit application
retries in the initial adapter: each logical judgement has one HTTP attempt,
and a retry/fallback may not be hidden by the HTTP-client configuration.
A rotated endpoint gets a new immutable adapter generation. Drain or cancel its
old in-flight work by the existing deadlines; close only resources owned by that
generation, never a shared gateway connection pool.
Defer any additional resilience-library dependency until its ownership and
shared state are approved.

Record direction, verdict, coverage (`inspected`, `windowed`, or `uninspected`),
bounded reason, judge latency, and released/held byte counts. Reasons include timeout, malformed
result, no text, unsupported profile, resource limit, admission rejection, and
cancellation. Proposed deny/failure counters are `ai_sec_request_deny`,
`ai_sec_response_deny`, and `ai_sec_judge_failure`. Counter registration belongs
to the existing metrics integration; the moderator exposes bounded exchange
attributes/observations rather than adding a mandatory metrics plugin dependency.
No prompt text, arbitrary model response, URL, request ID, or free-form category
belongs in a metric label. Category values must map to a configured finite set.

A bounded metadata-only decision event may support logging, but an audit event
failure must not rewrite a completed request verdict or block business traffic.
No cross-request judgement cache is proposed: a safe decision depends on the
policy generation, direction, full inspected text, and judge configuration.

## Acceptance and implementation sequence

Tests below are requirements for future implementation, not tests run by this
RFC. Use local HTTP judge fixtures and fixed protocol samples, with no vendor
credentials or public service dependency.

| Area | Required observation |
| --- | --- |
| Rule/configuration | Invalid limits, templates, score semantics, modes, and endpoint references are rejected; a policy update does not mutate an in-flight stream. |
| Request replay | Known and chunked oversize requests stop before unbounded allocation; accepted bytes/headers survive; the chain runs once. |
| Judge outcomes | Safe/risky/log/block, empty result, malformed score, non-success status, timeout, admission exhaustion, and cancellation stay distinguishable. |
| Actual proxy path | Both writer methods are covered, plus an AiProxyEngine-to-decorated-response fixture so bypassing the decorator fails a test. Admin sort overrides must fail coverage validation when they reverse the required order. |
| Frame boundaries | Split UTF-8/SSE/JSON, multi-event chunks, no-text frames, terminal frames, empty completion, and short tails preserve ordering and reach their documented decision. |
| Pre-release gating | Withhold each judge response and assert that the corresponding unapproved batch is not visible; an earlier safe result cannot release the next risky batch. |
| Limits/demand | Oversized single events, many empty frames, long streams, exhausted call budgets, and a slow judge keep retained memory/queues bounded. |
| Denial | Before/after commit, valid profile framing, no sensitive-text echo, and no false success on truncated/error streams. |
| Lifecycle | Client disconnect, rule removal, endpoint rotation, and shutdown cancel work and release pooled buffers/connections without double release. |
| Observe/fail-open | Original payload is preserved and explicit uninspected/FLAGGED observations are emitted; no clean verdict is fabricated. |

These are development milestones, not promises of a release at each step.
A mode/profile is advertised only after its complete writer/framing fixtures
pass. Do not expose a half-implemented response mode through a working-looking
Admin form; reject unsupported configurations. Existing gateways without the
starter cannot enforce the policy, so deployment inventory must verify module
support before routing protected traffic through a mixed-version fleet.

Proposed stages, each with its own reviewable PR:

1. Agree this contract, numeric ceilings/defaults, and the decisions table.
2. Add module/starter/DTO/SPI, both judge adapters, bounded request and non-streaming
   response support, and fixture-based contract tests. Keep unsupported streaming
   modes rejected until their implementation lands.
3. Add bounded `buffered` streaming and the real proxy-writer tests; then introduce
   `realtime` and audit-only `final_packet` with their separate demand/framing tests.
4. Add protocol profiles only as their extraction/refusal fixtures pass. Track
   Admin rows/forms, multi-dialect install/upgrade data, and rollout documentation
   separately, following the sensitive-word registration precedent.
5. Benchmark local controlled judge delay/throughput, concurrent protected
   exchanges, retained heap, cancellation recovery, and both slow/long SSE
   workloads. Publish p50/p99, judge calls per request, and inspected/bypassed
   denominators before recommending production limits.

The semantic classifier's accuracy and its resistance to adversarial content
remain properties of the chosen judge and deployment. A successful protocol or
resource-bound test does not establish universal content safety.
