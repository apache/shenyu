# AI Proxy credential failure fallback

AI Proxy can optionally use its existing fallback target when an upstream
returns an explicitly declared provider credential/account failure. It does
not retry the rejected primary credential before considering fallback.

The feature is disabled by default. Ordinary 401/403 responses continue to be
forwarded, and ordinary 429/5xx and transport retry behavior is unchanged.

## Configuration and trust

Configuration is bound under `shenyu.plugins.ai.proxy.retry` at gateway startup.
It is not an Admin selector setting. Engine creation validates the declarations
after binding and takes an immutable copy. Updating these settings requires
recreating the Engine, normally by restarting or redeploying the gateway.
Changing an existing properties object does not change an active Engine.

The following endpoint and values are **synthetic examples**, not a documented
error contract of OpenAI or another provider:

```yaml
shenyu:
  plugins:
    ai:
      proxy:
        retry:
          credential-failure-fallback:
            enabled: false
            declarations:
              - endpoint: https://credential-fixture.example/v1/chat/completions
                protocol: openai-chat
                status: 401
                type: error.code
                value: credential_revoked
              - endpoint: https://credential-fixture.example/v1/chat/completions
                protocol: openai-chat
                status: 401
                type: error.type
                value: authentication_failed
```

Before setting `enabled: true`, supply the actual complete HTTPS request
endpoint, its registered target protocol, and a status/field/value combination whose upstream
contract exclusively means provider-owned credential/account failure. A code
that also denotes resource permissions, region restrictions or request policy
is not suitable. A provider name or OpenAI-compatible request format alone does
not establish an error contract. Client requests cannot add or override these
trusted declarations.

| Setting | Default and constraints |
| --- | --- |
| `enabled` | `false`; enabling requires at least one valid declaration |
| `declarations` | Empty list; entries and list cannot be null |
| `endpoint` | Full HTTPS request URL with an absolute path; no credentials, query, fragment or wildcard |
| `protocol` | Exact registered upstream protocol ID, after protocol routing |
| `status` | Exactly 401 or 403 |
| `type` | `error.code` by default; only `error.code` and `error.type` are accepted |
| `value` | Exact nonblank string, at most 128 characters, no control characters |

All supplied entries are validated even when disabled. Multiple rules may share
the same normalized endpoint/protocol/status: any matching rule classifies the
response. Only duplicate endpoint/protocol/status/type/value combinations fail
Engine creation. An omitted type and explicit `error.code` are equivalent.
Invalid declarations are reported by index without echoing their content.

`value` replaces the unreleased draft's `error-code` setting; it is not an alias.
Migrate draft configurations to `value`. The `type` selector describes the JSON
field location, not a provider name or a free-form JSONPath expression.

Matching uses the **prepared upstream request URI**, not the client path, the
configured base URL alone, or a provider label. Host case and the default HTTPS
port are normalized. Raw path case, percent encoding and trailing slashes are
significant; DNS aliases and path prefixes do not match. An actual URI with a
query or another unsupported component is not classified.

The rules apply across selectors using that Engine, but do not grant access to
any target. Configure the fallback and its credential through the existing
[managed fallback configuration](ai-proxy-fallback-targets.md). Changing upstream
identity still requires the fallback's own explicit credential.

## Accepted evidence and body handling

Only primary HTTP failures before output, with an available resolved fallback
and an exact declaration, are inspected. The response must have an unambiguous
`application/json` or `application/*+json` media type and no unsupported content
encoding. Missing content type, wildcard types and non-identity encoding are
not classified.

The body must finish normally within **16 KiB** and be one strict JSON object:

```json
{"error":{"code":"credential_revoked","message":"synthetic example"}}
```

The body is parsed once, then each applicable rule compares the string at its
selected `error.code` or `error.type` field with its own `value`, case-sensitively.
A match on any rule is sufficient, even if another rule does not match. Fields
and values from different rules are never mixed. The message is ignored.
Missing/nonstring fields, malformed JSON, duplicate fields, trailing tokens,
empty bodies and unknown values do not match. There is no JSONPath, regex or
script matcher and no automatic classification of all providers' 401/403 codes.

16 KiB limits the bytes retained for inspection; it is not a new response size
limit. If inspection exceeds the budget, the gateway stops classifying and
forwards the buffered prefix, the crossing chunk and the remaining body. It
never truncates the response or subscribes to the network body a second time.
If reading fails, the original prefix and error are retained. The transport's
existing response limits and the execution's existing overall deadline still
apply. Inspection adds no waiting on disabled or nonmatching paths; eligible
small errors wait for completion before a classification decision can be made.

## Decision matrix

| Condition | Behavior |
| --- | --- |
| Disabled | Existing forwarding; no new body inspection |
| Trusted status/field/value match and fallback replay permitted | Skip further primary retries; call existing fallback at most once |
| Unknown, ambiguous, malformed or oversized error | Keep original forwarding behavior |
| No fallback or replay policy forbids switching | No extra upstream call; retain forwardable primary response |
| Unauthorized, disabled request selection, missing credential binding or incompatible target | Existing validation behavior; the new option does not bypass it |
| Invalid gateway/client proxy key | Rejected before any upstream call |
| Output already emitted, caller cancelled or total deadline expired | No prohibited additional call; existing termination behavior |
| Fallback fails | No nested fallback or fallback retry |
| Structured-output repair fails | Existing repair termination behavior |
| Other statuses or transport failures | Existing policy remains in force |

If the first primary attempt returns the declared failure, eligible recovery is
one primary call followed by at most one fallback call. If an earlier attempt
was already retried for another reason, its history is preserved; classification
stops **further** retries of the rejected primary credential.

The existing replay modes still apply. `NEVER` blocks recovery;
`PROVIDER_DECLARED` requires the fallback provider's declaration and the
existing request safety check. The fallback shares the logical request deadline
and cancellation lifecycle. Both nonstreaming and streaming requests are
supported before output; the gateway does not mix output from two upstreams.

## Observations

Existing attempt observations retain the primary HTTP failure independently of
a successful fallback. The closed reasons `CREDENTIAL_FAILURE` and
`FALLBACK_CREDENTIAL` identify classification and an actual fallback transition.
They do not contain keys, raw provider error bodies or arbitrary error-code
labels. This feature does not add provider health pools, account probing,
credential rotation or in-flight configuration revocation.
