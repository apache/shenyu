# ai-proxy-credential-fallback

## Purpose

Define the long-lived behavior contract for this capability.

Proposal Issues:
- https://github.com/apache/shenyu/issues/8514

## Requirements

### Requirement: Declared credential failure fallback

**Explicit startup-scoped classification**

`shenyu.plugins.ai.proxy.retry.credential-failure-fallback.enabled` MUST default
to false, with no declarations. A declaration MUST specify an exact HTTPS
request endpoint, registered target protocol, status 401 or 403, a `type` selector
and a nonblank `value` of at most 128 characters with no control characters.
`type` MUST default to `error.code` and MUST accept only `error.code` or
`error.type`. The endpoint MUST NOT contain credentials, query, fragment or
wildcards. Multiple rules MAY share a normalized endpoint/protocol/status;
only duplicate endpoint/protocol/status/type/value combinations MUST fail
validation. Omitted type and explicit `error.code` MUST be equivalent. Enabled settings
MUST contain at least one declaration. All supplied declarations MUST be
validated after configuration binding, when the Engine is created, even if
currently disabled. The Engine MUST retain an immutable copy.

Endpoint comparison MUST normalize host case and default HTTPS port only;
raw paths, path case, encoding and trailing slashes MUST remain significant.
Provider names and client-supplied recovery rules MUST NOT establish trust.
The operator MUST declare only a code whose upstream contract exclusively
identifies provider-owned credential/account failure.

#### Scenario: Default behavior

- **WHEN** the option is absent or false and the upstream returns 401/403
- **THEN** existing forwarding applies without new body inspection

#### Scenario: Authoritative validation

- **WHEN** Spring binds invalid declarations or Java code constructs an Engine with them
- **THEN** Engine creation fails with a bounded, input-free reason
- **AND** later mutation of the source settings cannot change an existing Engine's rules

**Bounded and unambiguous evidence**

Only a complete JSON object within 16 KiB, with the textual field selected by
at least one applicable rule exactly matching that rule's value, MAY establish
credential failure. The body MUST be parsed once for the rule group. Field and
value pairs MUST NOT be mixed between rules. JSON media types
MUST be unambiguous; unsupported content encodings MUST remain unclassified.
Duplicate fields, trailing tokens, wrong types and parse failures MUST NOT match.

#### Scenario: Multiple rules for one upstream scope

- **WHEN** code and/or type rules share the same endpoint, protocol and status
- **THEN** a match on any complete field/value pair establishes classification
- **AND** missing, nonstring or mismatching fields do not match that rule
- **AND** multiple matching rules still produce at most one fallback call

#### Scenario: Unknown, oversized or interrupted response

- **WHEN** the error cannot be classified, exceeds the inspection budget or fails during reading
- **THEN** forwarding retains the original status, forwardable headers and byte sequence, followed by the original terminal signal
- **AND** the original body is subscribed at most once, inspection memory is bounded and cancellation reaches the original upstream

**Reuse the authorized fallback and replay constraints**

Classification MUST apply only to primary status failures before output. A
known-rejected primary credential MUST NOT be retried as a prerequisite to
fallback. The current managed fallback, its own credential and capability
checks, replay policy, one logical deadline and cancellation MUST remain in
force. Fallback MUST run at most once and MUST NOT loop or retry. Repair-phase
status handling and unrelated status/transport policies MUST remain unchanged.

#### Scenario: Primary credential failure and healthy fallback

- **WHEN** the first primary attempt returns a declared failure and authorized replay to the configured fallback is allowed
- **THEN** exactly one primary and one fallback request occur, each with its own credential
- **AND** this applies to nonstreaming and streaming requests before output

#### Scenario: Work is prohibited

- **WHEN** fallback is unavailable or disallowed, replay is forbidden, output was emitted, the caller cancels or the overall deadline expires
- **THEN** no prohibited additional upstream call starts and existing rejection/termination semantics remain

#### Scenario: Other authentication and permission failures

- **WHEN** gateway authentication fails or upstream evidence denotes model/resource permissions, request policy or an unknown error
- **THEN** the credential-failure path does not bypass that failure

**Bounded observations**

A classified primary failure and an actual credential fallback MUST use closed
attempt reasons. A successful fallback MUST NOT erase the failed primary
attempt. Logs, events and exceptions MUST NOT contain credentials, raw provider
error bodies or unbounded provider-code labels.

Source SPEC comments:
- https://github.com/apache/shenyu/issues/8514#issuecomment-6954159600
