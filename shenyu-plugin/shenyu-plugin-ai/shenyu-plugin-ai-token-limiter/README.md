# AI token limiter

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

## Opt-in budget reservations

By default, the plugin checks recorded `completion_tokens` before forwarding a
request and records response usage afterwards. In-flight requests do not reserve
capacity, so concurrent requests can exceed the remaining completed-usage budget.

`RedisTokenBudgetStore` and the `META-INF/scripts/ai_token_*.lua` scripts implement
independent admission, known final usage settlement and completion without usage
for #7702. Fixed window selection and rule configuration validation are also
implemented and connected to the opt-in plugin request path. Administrator rule
form metadata, initialization/upgrade scripts, operational metrics and a dedicated
Redis-backed CI job are included.

The component takes a selected fixed window, a stable request ID, a positive
budget and a positive reservation amount. It returns `RESERVED`,
`ALREADY_RESERVED`, `ALREADY_SETTLED`, `ALREADY_RELEASED`, `USAGE_UNKNOWN`,
`WINDOW_NOT_CURRENT` or `REJECTED`.
Existing windows are not extended, and retries with the same ID do not reserve
twice. Invalid state and Redis errors propagate to the caller. Lua numeric
inputs are restricted to exact integers no larger than `9007199254740991`.

Settlement takes the original window key, request ID and known final usage
(including zero). In one atomic operation, it subtracts the original reservation
from `reserved`, adds actual usage to `used` and records `s:<requestId>`. The
original `r:<requestId>` and settlement marker remain until the window expires
to prevent retries from reserving or charging again. A retry with different
final usage fails without modifying counters. Actual usage above the reservation
is recorded in full; reservation estimates alone cannot guarantee a hard limit
on actual consumption. Counter overflow beyond the exact integer range fails
without partial writes.

Settlement returns `SETTLED`, `ALREADY_SETTLED`, `RESERVATION_NOT_FOUND` or
`WINDOW_NOT_FOUND`. It never creates a missing window, extends its TTL or moves
usage into another window. The caller remains responsible for selecting a valid
window at admission and retaining that key for settlement. Unknown usage must
not be represented as zero.

## Rule configuration and fixed windows

`AiTokenLimiterHandle` supports `reservationEnabled` (default `false`) and
`reservationTokens` (no implicit estimate). Legacy JSON without these fields
keeps reservation disabled. An explicitly enabled rule must provide a positive
reservation no greater than its token budget; its budget and window must also
fit the exact Lua integer range (window seconds are converted to milliseconds).
Admin rejects invalid reservation settings before saving or publishing rules,
including imports. Admin and gateways share the same validation contract;
gateways also reject invalid updates before replacing the cached rule. The plugin
uses reservation admission only when `reservationEnabled` is explicitly true.
The administrator form exposes a disabled-by-default reservation choice and an
optional estimate input; the estimate becomes mandatory when reservations are
enabled. Initialization scripts and each supported 2.7.1-to-2.7.2 upgrade script
include matching fields and boolean dictionary choices. Existing rule JSON is
not rewritten by the migration.

For example, this rule configuration reserves an estimated 20 completion
tokens per request against a 100-token budget:

```json
{
  "aiTokenLimitType": "uri",
  "tokenLimit": 100,
  "timeWindowSeconds": 60,
  "reservationEnabled": true,
  "reservationTokens": 20
}
```

`currentWindow(scope, size)` reads Redis `TIME`, then selects an epoch-aligned
fixed interval `[start, end)`. It does not create budget state. The key includes
a SHA-256 hash of the scope, the window size and start. This keeps scopes and
different window sizes separate, uses one Cluster hash tag per scope and keeps
untrusted braces out of that tag. Changing window size selects a separate budget.

The public `reserve(window, ...)` operation rechecks Redis time atomically with
admission and sets a newly created hash to expire at the absolute window end.
If selection became stale before admission, `WINDOW_NOT_CURRENT` is returned
without writing any state. The caller must select the current window again
before retrying. Future windows also reject early admission. Gateway clock skew
does not affect selection; the clock script uses a routing key with the same
scope hash tag as admission. The package-local key/TTL overload exists for script tests and
does not provide this boundary protection.

The caller retains the admitted window key through completion rather than
selecting a new window when a response arrives. A response spanning a window
boundary therefore never charges a new window or revives the old one. Fixed
windows can admit traffic on both sides of a boundary; they are not a rolling
window limit, and reservation estimates are not hard actual-consumption limits.

## Requests completing without usage

`completeWithoutUsage` requires an explicit evidence-based outcome:

| Outcome | Required evidence | Budget effect |
| --- | --- | --- |
| `NOT_FORWARDED` | Caller confirms the request never reached upstream execution | Release the reservation once and record zero usage |
| `USAGE_UNKNOWN` | Upstream execution is possible, but reliable final usage is unavailable | Retain the entire reservation until reliable settlement or window expiry |

Cancellation, an error or successful completion without usage does not establish
that upstream execution was avoided. Streaming disconnects also use
`USAGE_UNKNOWN` unless reliable final usage was received. At the gateway plugin
boundary, starting the downstream plugin chain must be treated as possible
forwarding; the plugin uses the conservative outcome from that point onward.
The store accepts the caller's evidence; it does not observe
network forwarding or classify Reactor signals itself.

An `f:<requestId>` marker records completion without usage. A released request
retains its original reservation identity and zero settlement marker, so retrying
the same ID returns `ALREADY_RELEASED` without allocating capacity again. An
unknown request returns `USAGE_UNKNOWN` on admission retry, and cannot be changed
to `NOT_FORWARDED`. Known late usage can still settle a held reservation exactly
once, including a reliably reported zero. Actual settlement takes precedence
over a racing unknown completion; unknown completion never refunds recorded
usage. Retrying contradictory positive usage after a confirmed release fails.

Completion returns `RELEASED`, `USAGE_HELD`, `ALREADY_SETTLED`,
`RESERVATION_NOT_FOUND` or `WINDOW_NOT_FOUND`. These operations never extend the
window or recreate an expired hash. No independent lease releases unknown
reservations early: the window TTL is their maximum retention time, including
when a gateway crashes before recording a completion outcome. This deliberately
trades capacity for conservative exposure accounting until that window expires.
All request markers expire with the hash. The state is window-scoped, but a
hard per-window cardinality cap is not implemented yet.

## Plugin request lifecycle

Each subscription receives a new internal request identity. The plugin snapshots
the rule, selects a Redis-time window and reserves before invoking the downstream
chain. Insufficient budget returns HTTP 429 without forwarding. A stale window
is reselected once; repeated staleness or Redis errors fail admission without
falling back to the legacy check. In-flight requests retain the original window
even if the rule or current window changes later.

Reservation cleanup is composed with `Mono.usingWhen`, including cancellation.
Successful completion waits for accounting; the new path does not launch a
separate accounting subscription. Reliable usage, including zero, is settled
once. Otherwise, downstream success without usage, failure or cancellation holds
the reservation. Before downstream invocation, cancellation or admission failure
attempts release for that internal ID. If Redis applied admission but its reply
was lost, or cancellation cleanup raced a Redis write, residual capacity remains
bounded by window expiry. A gateway crash has the same expiry fallback.

The forwarding flag is changed atomically against cleanup, so cancellation cannot
release a request and then let that request start forwarding. Cleanup has a
five-second timeout. Cleanup failures are logged; an original downstream error
is preserved with cleanup failure suppressed. A cleanup failure after successful
downstream processing propagates as an error, though an already committed HTTP
response cannot be replaced. Cancellation cleanup is asynchronous and logs failures.

Reservation response observation uses the existing bounded 64 KiB decoded tail
and gzip decoder. It distinguishes reliable integer usage from absent, invalid
or unsupported usage. Complete JSON responses within that buffer are inspected
before completion propagates, and parsed streaming usage is retained until the
request terminates rather than incremented for every usage frame. Parser limits,
including usage fragmented across independent flush publishers or a JSON body
larger than the retained tail, conservatively result in held capacity when no
reliable usage was captured. This change does not replace the streaming parser.
The legacy response accounting path remains the default.

## Metrics

Enable ShenYu's existing metrics plugin before the AI token limiter to export:

| Metric | Meaning |
| --- | --- |
| `shenyu_ai_token_budget_events_total{event}` | Budget events using only bounded enum labels |
| `shenyu_ai_token_reserved_tokens_total` | Cumulative estimated tokens in acknowledged admissions; not currently held tokens |
| `shenyu_ai_token_actual_tokens_total` | Cumulative reliable reported completion tokens, observed once during cleanup |

Events are `admitted`, `rejected`, `usage_reported`, `settled`, `released`,
`usage_held`, `window_expired`, `reservation_missing`, `admission_error` and
`accounting_error`. Reported usage remains observable even when settlement fails
or the original window has expired; `settled` separately confirms that Redis
accepted reconciliation. A reported zero produces `usage_reported` with a zero
token increment, whereas unknown usage produces `usage_held`. Cancellation before
an admission acknowledgment can produce `released` without an observed `admitted`.
Errors count failed operations, so admission and cleanup can each report failure
for one request. These counters are operational observations, not billing records.

Rule IDs, raw resolved identities, paths and request IDs are never metric labels.
The callback uses the existing request-context metrics mechanism, so the AI
plugin does not depend directly on the metrics plugin. If metrics are absent or
the callback throws, admission and accounting continue with the same policy.

## Running the budget tests

Run Java unit tests from the repository root:

```bash
./mvnw -pl shenyu-admin,shenyu-plugin/shenyu-plugin-metrics,shenyu-plugin/shenyu-plugin-ai/shenyu-plugin-ai-token-limiter -am test \
  '-Dtest=AiTokenLimiterHandleTest,TokenBudget*Test,RedisTokenBudgetStoreTest,AiTokenLimiterPlugin*Test,AiTokenLimiterRuleHandlerTest,AiTokenLimiterReservationTest,AiTokenBudgetMetricsTest,AiTokenReservationConfigurationTest,AiTokenLimiterRuleHandleValidatorTest,RuleServiceTest' \
  -Dsurefire.failIfNoSpecifiedTests=false -DskipTests=false
```

The real Redis suite is opt-in. With a dedicated local Redis instance listening
on `127.0.0.1:16379`, run:

```bash
./mvnw -pl shenyu-admin,shenyu-plugin/shenyu-plugin-metrics,shenyu-plugin/shenyu-plugin-ai/shenyu-plugin-ai-token-limiter -am test \
  '-Dtest=AiTokenLimiterHandleTest,TokenBudget*Test,RedisTokenBudgetStore*Test,AiTokenLimiterPlugin*Test,AiTokenLimiterRuleHandlerTest,AiTokenLimiterReservation*Test,AiTokenBudgetMetricsTest,AiTokenReservationConfigurationTest,AiTokenLimiterRuleHandleValidatorTest,RuleServiceTest' \
  -Dsurefire.failIfNoSpecifiedTests=false -Dshenyu.test.redis.port=16379 -DskipTests=false
```

The integration suite creates two independent Redis connection factories. It
tests concurrent admission, settlement and completion, idempotent retries,
unknown usage retention, confirmed release, TTL preservation, late completion
after expiry, shared-clock window selection, stale and future window rejection,
absolute window expiry, window isolation, invalid state and exact integer boundaries.
Plugin integration tests also drive concurrent requests across two Redis clients,
JSON and gzip response accounting, downstream failure, cancellation and late
responses after deterministic expiry of the original window. Each test uses UUID keys
and deletes only its own keys; it does not flush the database or stop Redis.
Without `shenyu.test.redis.port`, the Redis integration suites are skipped. The
`ai_token_budget` job in `.github/workflows/ci.yml` provisions Redis 7.2, sets the
property explicitly, checks metrics and admin seed parity, and uploads failure
reports. Relevant path changes trigger the job and its result gates the aggregate
`build` check. Admin seed parity tests execute on H2 (including repeat H2 startup
seeding and Oracle DDL with Oracle identifier folding); native SQL dialect validation remains covered by the existing native
SQL matrix workflow.
