# Shadow traffic

The `shadow` plugin mirrors a bounded, sampled copy of selected HTTP requests to
an independent shadow destination and compares the outcomes, while the primary
response remains authoritative.

The first delivery mirrors requests only. Response comparison and difference
reporting are staged follow-ups; this document describes the contract that is
implemented today and the bounds it enforces.

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
- A full queue drops the sample; a shadow attempt honours `timeoutMillis`;
  removing or re-binding a rule releases its queue.

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
