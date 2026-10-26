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
  type may not shadow a builtin one. Custom patterns are limited to 32 nonblank entries,
  2,048 characters per regex, and 32,768 characters for the complete configuration
  (Java UTF-16 character counts). Blank lines are ignored for the entry count; duplicate
  and malformed nonblank entries still count. The limits are checked before splitting
  or compiling patterns. Exceeding a limit rejects the configuration rather than
  truncating it or installing a partial custom policy. Every detector runs under a
  per-direction deadline enforced by a deadline-aware `CharSequence`; the deadline
  bounds matching work, not regex compilation.
- Protocol profiles extract the message text of the payload: `openai-chat`
  (`messages[].content` / `choices[].message.content`) and `openai-responses` (`input` /
  `output[].content[].text`). Within a `content` part array, besides the plain `text` of a
  part, the nested text of a `document` part (`source.data` of a `text` source) and of a
  `file` part (a textual `file_data`, base64 decoded for scanning and re encoded after a
  redaction) is extracted too, and so are the `arguments` of a tool call
  (`tool_calls[].function.arguments`, the legacy `function_call.arguments`, and the
  `arguments` of a responses `function_call` item). Binary content — an `image_url`,
  `input_audio`, a base64 `document` source and a non textual `file_data` — is left out of
  scope, since it cannot be meaningfully regex scanned; tool definitions and other nontext
  members are never reported. The extraction containers and text member types are
  validated before scanning. Unknown or malformed shapes are unsupported enforcement
  modes: fail-closed rules reject them and fail-open rules warn and pass them through.
  Validation is limited to extraction shape, not the complete provider schema. Valid
  textless messages, such as image-only content, remain supported.
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
- Streaming requests (`stream: true`) under a response scope are rejected up front
  when the rule is fail closed. In fail-open mode, if request scanning is also enabled,
  the request scan observes the flag and skips response inspection with a warning.
  With `scope=response` alone, fail-open rules do not read the request body to inspect
  the flag: unsupported SSE responses pass through based on their response content
  type, while supported non-streaming JSON responses are still scanned.
- A real upstream error envelope (a non null `error` object) is never rewritten. A
  `"error": null` member is not an error: successful `openai-responses` completions carry
  it and are scanned normally.
- `maxBodySize` (default 1 MiB) bounds both directions. On the response side the bound
  is applied while the body flows through: a body that stays under the bound is scanned
  once complete; a fail closed rule whose bound is exceeded stops reading, cancels the
  upstream and answers 403 before any byte was sent; a fail open rule switches to
  unscanned streaming at that point — the buffered prefix is written, followed by the
  overflowing chunk and every later chunk as they arrive, so the memory stays bounded
  and the client receives the prefix without waiting for the body to complete.
- The request side is additionally bounded by the codec in memory limit
  (`spring.codec.max-in-memory-size`, independent of `maxBodySize`): a request that
  exceeds the codec limit can no longer be forwarded unchanged and is rejected with a
  clear reason.
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
| `maxBodySize` | long | `1048576` | largest scanned body in bytes, `0` explicitly means unlimited; negative values warn and fall back to 1 MiB |
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
unusable `scope`, a negative `maxBodySize` or a non positive `detectorTimeoutMs` are
logged and fall back to the safe default. A custom-pattern size or count violation
rejects rule compilation; an existing compiled policy is retained rather than
partially replaced. A configured rule without a compiled policy rejects traffic
instead of silently bypassing inspection. Clearing the handle drops its compiled
policy and disables inspection for that rule.
