# shadow-traffic

## Purpose

Define the long-lived behavior contract for sampled HTTP shadow traffic.

Proposal Issues:
- https://github.com/apache/shenyu/issues/8696

## Requirements

### Requirement: Sampled HTTP shadow traffic with bounded response comparison

The gateway MUST expose an opt-in `shadow` plugin at order 207 that mirrors a sampled, bounded copy of eligible HTTP requests to an independent shadow destination and compares the shadow response with the primary response, while the primary response remains authoritative. Selectors and rules MUST be the explicit allowlist. The rule handle MUST carry every bound: target, method allowlist, sample rate, concurrency, queue capacity, attempt timeout, request and response body bounds, compared headers, ignored JSON paths, forwarded headers and shadow-only extra headers. The plugin MUST reject non-HTTP requests, methods outside the allowlist, WebSocket upgrades and `text/event-stream` requests with a counted reason. Sampling MUST be deterministic per request and rule. Request and response bodies MUST be observed without retaining, delaying, releasing or replaying primary buffers, and every copy MUST be bounded. Dispatch MUST be a non-blocking offer made after the primary exchange terminates; a full queue MUST drop the sample with a counted reason and MUST NOT slow the primary. The shadow call MUST use an independent client and the rule's timeout. Cookies, authorization and hop-by-hop headers MUST NOT be forwarded unless named by the operator; every shadow request MUST carry `X-Shenyu-Shadow: true`; reports MUST name outcomes, statuses, latencies and a detail (status pair, header name or JSON path) and MUST NOT contain response values or credentials. Comparison MUST distinguish match, normalization, mismatch, truncation and shadow failure. An invalid or disabled rule MUST remove its runtime instead of applying partially. Absent the plugin or an enabled rule, gateway behavior MUST be unchanged.

#### Scenario: Explicit allowlist

- **WHEN** no enabled shadow selector and rule match a request
- **THEN** the request is not mirrored and the primary request and response are unchanged

#### Scenario: Method and streaming exclusions

- **WHEN** the method is outside the configured `methods`, or the request is a WebSocket upgrade or `text/event-stream`
- **THEN** the request is skipped and counted as `METHOD_NOT_ALLOWED` or `STREAMING_REQUEST`

#### Scenario: Deterministic sampling

- **WHEN** the same request is evaluated repeatedly against the same rule
- **THEN** the sampling decision is identical; a rate of 0 never mirrors and a rate of 1 always mirrors

#### Scenario: Bounded and complete request bodies

- **WHEN** the request body exceeds `maxRequestBodyBytes`, or the primary does not read the complete declared body
- **THEN** no shadow call is made and the skip is counted as `BODY_TOO_LARGE` or `BODY_INCOMPLETE`

#### Scenario: Primary independence

- **WHEN** the shadow target is slow, failing or unavailable
- **THEN** the primary response status, headers and body are unchanged, no shadow error reaches the primary, and the shadow call is bounded by the rule timeout

#### Scenario: Bounded dispatch

- **WHEN** the rule's queue is full
- **THEN** the sample is dropped and counted as `QUEUE_OVERFLOW` without growing memory or blocking the caller

#### Scenario: Response comparison

- **WHEN** both responses are captured
- **THEN** statuses and configured headers are compared, JSON bodies are compared structurally with `ignoreJsonPaths` normalization, other bodies are compared byte for byte, and the outcome is `MATCH`, `NORMALIZED`, `MISMATCH` or `TRUNCATED`

#### Scenario: Shadow failure

- **WHEN** the shadow call fails or times out
- **THEN** the outcome is `SHADOW_FAILED` and the primary response is unchanged

#### Scenario: Redaction

- **WHEN** the primary request carries cookies or authorization headers that the operator did not name
- **THEN** they are not forwarded, and no report contains response values, header values or credentials

#### Scenario: Rule lifecycle

- **WHEN** a rule is updated, disabled or removed
- **THEN** the previous runtime is disposed and queued work released; an invalid handle removes the runtime instead of applying partially

#### Scenario: Compatibility

- **WHEN** the plugin starter is absent or no shadow rule is enabled
- **THEN** routing and all existing plugin behavior are unchanged

Source SPEC comments:
- https://github.com/apache/shenyu/issues/8696#issuecomment-7237222824
