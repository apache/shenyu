# logging-credential-headers

## Purpose

Define the long-lived behavior contract for this capability.

Proposal Issues:
- https://github.com/apache/shenyu/issues/8460

## Requirements

### Requirement: Credential header masking in logging plugins

Logging plugins MUST replace the complete logged value of Authorization, Proxy-Authorization, Cookie, Set-Cookie, ShenYu-Authorization and token with a single *** by default, matching header names case-insensitively. The policy MUST apply to request and response headers, including headers captured on error paths, in both the shared logging pipeline and loggingConsole. Plugin-global maskCredentialHeaders MUST default to true; explicit false MUST disable this policy. Plugin-global maskedHeaders MUST add names to the built-in list rather than replace it. Missing or null settings and an empty additional list MUST retain built-in protection. Configured names MUST be trimmed and matched case-insensitively; null and blank entries MUST be ignored. Each request MUST retain its captured policy across configuration replacement, and subsequent requests MUST use the updated policy. Masking MUST NOT modify forwarded HTTP headers or change existing rule-based keyword/body masking. Malformed JSON and wrong JSON types remain subject to existing deserialization behavior, without a new fallback guarantee.

#### Scenario: Defaults without rule masking

- **WHEN** a request is logged with credential header masking settings omitted and rule keyword masking disabled
- **THEN** the six built-in credential header names are masked in logged requests and responses in the common and Console paths

#### Scenario: Mixed case and multiple values

- **WHEN** a protected header has mixed casing and multiple values
- **THEN** the complete logged value is one *** and none of the original values remain in that header field

#### Scenario: Ordinary and empty headers

- **WHEN** headers contain unprotected names or the header collection is empty
- **THEN** ordinary values retain their existing format and an empty collection formats without failure

#### Scenario: Additional names and null entries

- **WHEN** maskedHeaders contains X-Api-Key, whitespace-padded names, null and blank entries
- **THEN** custom names are trimmed and matched without case sensitivity, null/blank entries are ignored and built-in protection remains active

#### Scenario: Missing and null configuration

- **WHEN** maskCredentialHeaders is missing or null and maskedHeaders is missing, null or empty
- **THEN** the built-in deny-list remains enabled

#### Scenario: Explicit opt-out

- **WHEN** maskCredentialHeaders is false
- **THEN** this policy leaves both built-in and custom header values unchanged, while existing rule keyword/body masking can still apply

#### Scenario: Configuration reload during a request

- **WHEN** configuration changes after a request enters a logging plugin
- **THEN** that request uses its captured policy for both directions, subsequent requests use the new policy and plugin-specific overrides do not affect other plugin IDs

#### Scenario: Normal and error paths preserve traffic

- **WHEN** normal or error processing captures credential headers
- **THEN** logged header fields are masked under the captured policy and live HTTP header values remain unchanged

#### Scenario: Console lifecycle

- **WHEN** a Console plugin with a cached masking override is disabled or removed
- **THEN** its cached override is removed and an absent configuration falls back to secure defaults

Source SPEC comments:
- https://github.com/apache/shenyu/issues/8460#issuecomment-6814335706
