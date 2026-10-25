# shenyu-plugin-ai-pii

The pii detection plugin detects personally identifiable information on the message text
fields of AI protocol payloads and applies the action of the matched rule: **block** the
direction, **redact** the matched ranges, or **audit** the observation without the detected
value. It is aimed at AI gateways where prompts must not leak identifiers to a model
provider and model answers must not leak identifiers to the callers, but it also applies
to plain HTTP routes that carry the supported payloads.

It is a standalone plugin next to `sensitiveWord`: the sensitive word plugin keeps its
reject-only dictionary semantics, this plugin brings typed detectors, protocol-aware
extraction and configurable actions.

## How it works

- Ordering: the plugin runs at order **196**, in front of `aiProxy` (199) which terminates
  the chain, so it sees the traffic before the model is called.
- Detectors are deterministic: `EMAIL`, `PHONE_CN` (digit anchored), `NATIONAL_ID_CN`
  (mod 11-2 checksum validated) and `CREDIT_CARD` (Luhn validated, repeated-digit strings
  rejected). An admin can add bounded custom regexes, one `TYPE=REGEX` per line; a custom
  type may not shadow a builtin one. Every detector runs under a per-direction deadline
  enforced by a deadline-aware `CharSequence`, so a pathological pattern cannot pin a
  worker thread.
- Protocol profiles extract only the message text fields, so a redaction never touches
  tool definitions, function arguments or other members: `openai-chat`
  (`messages[].content` / `choices[].message.content`) and `openai-responses` (`input` /
  `output[].content[].text`). A payload whose shape matches no profile is an unsupported
  enforcement mode, see the failure policy below.
- Redaction happens on the parsed JSON tree and only inside the extracted fields, so the
  JSON structure and the escaping of everything else are preserved. Overlapping detections
  are merged on the original coordinates before anything is replaced, the merged range
  takes the placeholder of the match that starts first.
- The request side never scans on the netty event loop, and `stream: true` under a
  response scope is rejected **before** the model is called when the rule is fail closed.
- The response side covers both `writeWith` and `writeAndFlushWith` (the AI proxy writes
  through `writeAndFlushWith`).
- Actions: `BLOCK` answers 403 with a message that never echoes the detected value,
  `REDACT` replaces the matched ranges with `<TYPE>` placeholders, `AUDIT` forwards the
  payload unchanged and records `{type, count, direction, action, ruleId}` through a
  `PiiAuditSink` bean (default: structured logging without the value). When detections of
  different types overlap, the strictest action wins: `BLOCK` over `REDACT` over `AUDIT`.
- Concurrency: detectors and compiled rules are immutable and shared, every request
  builds its own matches and redactions, so concurrent requests never share redaction
  state.

## Bounded first release

- Server sent events and compressed responses (`Content-Encoding` other than `identity`)
  cannot be scanned. With a fail closed rule they are rejected, otherwise they are passed
  through **unchanged** (a compressed body is never decoded and re encoded).
- Streaming requests (`stream: true`) under a response scope: rejected up front when the
  rule is fail closed, otherwise the response scan is skipped with a warning.
- A real upstream error envelope (a non null `error` object) is never rewritten. A
  `"error": null` member is not an error: successful `openai-responses` completions carry
  it and are scanned normally.
- `maxBodySize` (default 1 MiB) bounds both directions. On the response side a fail
  closed rule stops reading as soon as the bound is exceeded; a fail open rule reads the
  body to the end — a truncated pass through would corrupt the response — and writes it
  back unscanned.
- Fail open means *not detected*, never *checked and clean*: every skipped mode logs a
  warning.
- Reversible tokenization and non OpenAI protocols are out of scope of this release, and
  no universal pii detection is claimed.

## Mutual exclusion

Do not enable this plugin together with `aiResponseTransformer` (66) on the same traffic:
the transformer replaces the whole response body with a generated one and the redaction
applied here is overwritten.

## Configuration

Rule level (`handle` of the rule, rendered by the console rule form; every field is a
delimited string so the generic form can edit it):

| field | type | default | description |
| --- | --- | --- | --- |
| `detectors` | string | empty = all builtins | builtin detector types, separated by commas |
| `customPatterns` | string | empty | one `TYPE=REGEX` entry per line, may not shadow a builtin type |
| `actions` | string | empty | per type overrides, `TYPE=ACTION` separated by commas |
| `defaultAction` | string | `REDACT` | action for a type without override |
| `scope` | string | `request` | `request`, `response` or `request,response` |
| `maxBodySize` | long | `1048576` | largest scanned body in bytes, `0` means unlimited |
| `failClosed` | boolean | `false` | reject unsupported modes instead of passing them through |
| `detectorTimeoutMs` | long | `500` | detector budget per direction in milliseconds |

Example (redact phones and emails in both directions, block national ids, never fail
open):

```json
{
  "detectors": "EMAIL,PHONE_CN,NATIONAL_ID_CN",
  "customPatterns": "ORDER_ID=ORD-\\d{8,12}",
  "actions": "NATIONAL_ID_CN=BLOCK,ORDER_ID=REDACT",
  "defaultAction": "REDACT",
  "scope": "request,response",
  "maxBodySize": 1048576,
  "failClosed": true,
  "detectorTimeoutMs": 500
}
```

A wrong configuration never silently disables the rule: unknown detector types, an
unusable `scope` or a non positive `detectorTimeoutMs` are logged and fall back to the
safe default, and a rule whose handle is cleared drops its compiled policy.
