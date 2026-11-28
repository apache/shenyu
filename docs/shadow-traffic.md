# Shadow traffic

The `shadow` plugin mirrors a bounded, sampled copy of selected HTTP requests to
an independent shadow destination and compares the outcomes, while the primary
response remains authoritative.

The plugin mirrors requests and compares the shadow response with the primary
one. It runs in two stages: the first delivery added configuration, request
mirroring and bounded dispatch; response capture, comparison and reporting are
included now. Performance acceptance under #7650 remains a follow-up.

## Position in the chain

The plugin runs at order `207`, after the proxy plugin selected an upstream
(`divide`, `200`) and after the URI plugin rewrote the path (`205`), but before
the HTTP client plugin sends the primary request (`210`). It observes the
request exactly as it is about to be forwarded.

## Configuration

Selectors and rules define the explicit allowlist: a request is mirrored only
when a `shadow` selector and rule match it. The rule handle carries every bound:

| Field | Default | Meaning |
| --- | --- | --- |
| `enabled` | `false` | Mirrors requests for this rule |
| `target` | none | Absolute `http` or `https` destination. User info, query and fragment are rejected |
| `methods` | `GET, HEAD, OPTIONS, TRACE` | Methods eligible for mirroring. A write is mirrored only when its method is named |
| `sampleRate` | `0` | Fraction of eligible requests in `[0, 1]`, decided deterministically per request and rule |
| `maxConcurrent` | `8` | Shadow calls in flight for this rule |
| `queueCapacity` | `64` | Queued shadow calls before a sample is dropped |
| `timeoutMillis` | `1000` | Budget for one shadow attempt |
| `maxRequestBodyBytes` | `65536` | Request body bytes copied for the shadow call. A larger body is skipped, never truncated and sent |
| `maxResponseBodyBytes` | `65536` | Response body bytes copied on either side for comparison. A larger body is reported as truncated |
| `compareHeaders` | `Content-Type` | Response headers compared between the two responses |
| `ignoreJsonPaths` | none | JSON paths ignored during body comparison, for example `$.timestamp` |
| `forwardHeaders` | `Accept, Content-Type` | Request headers forwarded to the shadow target |
| `extraHeaders` | none | Static headers added to every shadow call, for example shadow-only credentials. Values are never logged |

Example rule handle:

```json
{
  "enabled": true,
  "target": "http://candidate:8080",
  "methods": ["GET"],
  "sampleRate": 0.05,
  "maxConcurrent": 8,
  "queueCapacity": 64,
  "timeoutMillis": 1000,
  "maxRequestBodyBytes": 65536,
  "maxResponseBodyBytes": 65536,
  "compareHeaders": ["Content-Type"],
  "ignoreJsonPaths": ["$.timestamp"],
  "forwardHeaders": ["Accept", "Content-Type"],
  "extraHeaders": {"X-Shadow-Token": "<shadow-only-credential>"}
}
```

## Behavior

- The primary request never waits for the shadow call. Mirroring is dispatched
  only after the primary exchange terminates, with the primary response already
  written.
- The shadow copy is taken while the primary reads its body. The plugin does not
  buffer, delay, release or replay primary buffers, so primary backpressure and
  buffer ownership are unchanged.
- The shadow destination receives the raw path and query exactly as the primary
  request carried them; already encoded characters are not encoded again.
- Cookies, authorization and hop-by-hop headers are not forwarded unless an
  operator names them. Every shadow request carries `X-Shenyu-Shadow: true`.
- A slow, failing or unavailable shadow target cannot delay, replace or alter
  the primary response.
- The primary response body is observed while it is written, without buffering,
  delaying or releasing primary buffers.
- A full queue drops the sample; a shadow attempt honours `timeoutMillis`;
  removing or re-binding a rule releases its queue.

## Comparison

The shadow response is compared with the primary response after the primary
exchange terminated:

- Status codes must be equal.
- Every header in `compareHeaders` must have the same values on both sides.
- JSON bodies are compared structurally, ignoring `ignoreJsonPaths`; other
  bodies are compared byte for byte.
- Latency is measured separately for both sides and never used as a match
  criterion.

| Outcome | Meaning |
| --- | --- |
| `MATCH` | Status, compared headers and body are equal |
| `NORMALIZED` | The bodies differ only in ignored JSON paths |
| `MISMATCH` | Status, a compared header or the body differs |
| `TRUNCATED` | At least one body exceeded the comparison bound |
| `SHADOW_FAILED` | The shadow call failed or timed out |

Reports are written to the `LoggingShadowReporter` logger. A report names the
outcome, rule, statuses, latencies and the redacted detail (a status pair, a
header name or a JSON path); response values, header values and credentials are
never written.

## Skip reasons

| Reason | Meaning |
| --- | --- |
| `NOT_HTTP` | The request is not an HTTP request |
| `METHOD_NOT_ALLOWED` | The method is outside `methods` |
| `STREAMING_REQUEST` | WebSocket upgrade or `text/event-stream` |
| `BODY_TOO_LARGE` | The request body exceeded `maxRequestBodyBytes` |
| `BODY_INCOMPLETE` | The primary did not read the complete declared body |
| `PRIMARY_CANCELLED` | The client disconnected before the primary completed |
| `INVALID_TARGET` | The target cannot be combined with the request path |
| `QUEUE_OVERFLOW` | The rule's bounded queue is full |

## Enabling

Add `shenyu-spring-boot-starter-plugin-shadow` to the gateway. The plugin is
active by default and does nothing until a `shadow` rule is enabled. Disable the
whole plugin with `shenyu.plugins.shadow.enabled=false`.

The plugin needs its `plugin`, `selector` and `rule` rows in Admin. The schema
seeds the plugin and its menu; the dashboard form for the rule handle is a
follow-up in the dashboard project, so the handle can be configured through the
Admin API until then.

## Non-goals

- Recording or replaying traffic; see the recording/replay work for that.
- Mirroring streaming responses, WebSocket sessions or non-HTTP protocols.
- Replacing the primary response with the shadow response.
- Promising identical responses from inherently nondeterministic services.

## Verification

The module tests cover:

- configuration validation: target form, bounds and header injection;
- sampling determinism and the rate boundaries;
- eligibility: non-HTTP requests, the method allowlist, WebSocket upgrades and SSE;
- bounded request capture without moving the primary read position, and truncation;
- bounded response capture without replacing the primary body;
- comparison outcomes: match, status mismatch, header mismatch, JSON path and array
  length mismatches, normalization, truncation, non-JSON bytes, invalid JSON, and a
  detail that never carries response values;
- a real shadow target (WireMock): mirroring with the shadow marker header,
  `MATCH`/`MISMATCH` reporting, a timeout reported as `SHADOW_FAILED`, non-blocking
  dispatch, queue overflow and rejection after close.

Not covered here: live-cluster measurement of the primary path (#7650), response
decompression limits (the platform HTTP client applies its own bounds) and an
official integration/e2e suite run. Those remain follow-ups.
