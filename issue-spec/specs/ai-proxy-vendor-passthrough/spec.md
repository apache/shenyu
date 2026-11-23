# AI Proxy vendor-native passthrough

## Purpose

Let a vendor SDK send its native HTTP request through an AI Proxy selector using
its own credentials. This local implementation tracks [#8454](https://github.com/apache/shenyu/issues/8454).
The public Proposal, Design and confirmed SPEC references are pending.

## Requirements

### Selector and credentials

- When the mode is absent or `converted`, the existing provider, credential and
  conversion rules apply.
- When the mode is `passthrough`, the matched selector must specify a valid
  HTTP(S) base URL. An unknown mode or invalid target must fail before forwarding.
  A request must use the mode and target from the same selector update.
- The upstream receives the caller's vendor credentials, including credentials
  in the query. A configured provider key and the proxy-key lookup do not apply.

### HTTP forwarding

- The upstream authority comes from the selector. The gateway copies the method,
  raw path and raw query. An optional literal prefix is removed once at a segment
  boundary. An exact prefix match resolves to the base directory. A mismatch
  returns 400.
- Forward end-to-end headers and their repeated values. Remove connection-specific
  headers, including fields nominated by `Connection`, and derive the outbound
  `Host` from the upstream address. Forward caller-supplied trace headers under the same rules. Tunnels and protocol upgrades are unsupported.
- Read the inbound request once, forwarding chunks with backpressure. Bound the upload to 5 MiB or the
  configured smaller transport budget. An unknown-length upload can reach the
  vendor before the gateway detects overflow.
- Forward the vendor's HTTP status, end-to-end response headers and body
  bytes, including errors, redirects and SSE. A client disconnect must cancel
  the upstream operation. Preserve HTTP-client deadlines and response budgets.

### Compatible policies

- Only a vendor HTTP 429 may trigger a same-endpoint retry. Retain a bounded
  byte copy during the first upload. Require its completion, permission under
  the status replay policy, an available streaming retry and time to honor
  Retry-After within the shared deadline. Forward the final vendor response.
  Transport failures, 5xx, cancellation and downstream writes never trigger replay.
- Passthrough bypasses model overrides, conversion, fallback,
  structured-output repair and the AI response cache.
- Reject potentially overlapping enabled AI content-policy and passthrough
  selectors in the same namespace on configuration writes. With stable paths,
  allow selectors whose fixed URI segments diverge, such as `/claude/a/**`
  and `/claude/b/**`. If an incompatible
  policy runs during configuration synchronization, reject the request before
  contacting the vendor. Ordinary routes can still append prompt messages.
- Keep logical request and attempt observation. Read usage from understood Chat
  Completions and Responses JSON or SSE without changing the body. Report
  unsupported native formats without synthesizing token counts. The token
  limiter retains ownership of admission and quota accounting.
