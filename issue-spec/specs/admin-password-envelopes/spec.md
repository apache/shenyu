# admin-password-envelopes

## Purpose

Define the long-lived behavior contract for this capability.

Proposal Issues:
- https://github.com/apache/shenyu/issues/8519

## Requirements

### Requirement: Authenticated Admin password envelopes

The Admin MUST expose only public encryption metadata at /platform/secretInfo and accept the exact version-1 envelope for login and self-service password changes using RSA-OAEP SHA-256/MGF1-SHA256 and AES-256-GCM. It MUST bind authenticated requests to the advertised key, operation and decoded change-password target, reject malformed, oversized, altered, stale or incorrectly bound envelopes without exposing submitted credentials, and retain existing DTO validation, principal-ID authorization, password policy, password hashes and token invalidation. Legacy requests MUST remain compatible by default; explicitly required mode MUST reject legacy JSON and GET/HEAD login while retaining OPTIONS. Production private keys MUST be external, persistent and shared across Admin instances. Application encryption does not replace HTTPS or prevent replay within the allowed age window.

#### Scenario: Sanitized public metadata

- **WHEN** a client requests /platform/secretInfo
- **THEN** key and iv remain empty and the response advertises public SPKI/key ID, protocol, server time and request age with no-store

#### Scenario: Valid credential envelopes

- **WHEN** valid login or same-user password-change envelopes arrive
- **THEN** the authenticated original fields reach unchanged controllers and existing checks

#### Scenario: Exact password restoration

- **WHEN** a password contains Unicode, is long, or looks like legitimate Base64 ciphertext
- **THEN** the existing verifier receives exactly the original password once without RSA plaintext-size limits

#### Scenario: Invalid envelope input

- **WHEN** JSON fields, canonical Base64, key/tag, age, operation, actual path binding or body bounds are invalid
- **THEN** reject generically before the controller without logging or returning submitted credential values or underlying exceptions

#### Scenario: Default legacy compatibility

- **WHEN** required mode is omitted or false
- **THEN** existing legacy JSON credential and GET login clients retain their contract

#### Scenario: Explicit required mode

- **WHEN** required mode is true
- **THEN** legacy POST/PUT and GET/HEAD login credentials are rejected while OPTIONS preflight remains available

#### Scenario: Original password and identity checks

- **WHEN** a new password is weak, an old password is wrong, or a caller targets another account
- **THEN** the original validation, service error or immutable principal-ID authorization remains authoritative

#### Scenario: Persistent shared key

- **WHEN** Admin instances use the same configured PKCS8 key
- **THEN** they advertise the same key ID and accept matching envelopes; invalid or undersized configured keys fail safely

#### Scenario: Coordinated key rotation

- **WHEN** operators replace the single active private key
- **THEN** they pause metadata and credential traffic, drain accepted requests and the configured age window plus skew allowance, restart all instances with the same new key, verify matching key IDs and resume traffic; seamless mixed-key rolling rotation is not supported

#### Scenario: Compatible rollback order

- **WHEN** operators revert an envelope dashboard, Admin or transport key
- **THEN** they first restore required=false across Admin instances before a legacy dashboard, remove envelope-only clients or pause their credential traffic before a legacy Admin, and use a coordinated pause/drain/restart for key rollback without resetting stored credentials or business data

Source SPEC comments:
- https://github.com/apache/shenyu/issues/8519#issuecomment-6968319733
