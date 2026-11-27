# jwe-http-payload

## Purpose

Define the long-lived behavior contract for this capability.

Proposal Issues:
- https://github.com/apache/shenyu/issues/8659

## Requirements

### Requirement: Protect bounded JSON exchanges with compact JWE

An opted-in HTTP POST route MUST independently support request decryption and response encryption with trusted deployment keys. Decryption MUST preserve the original JSON bytes. Encryption MUST retain the upstream status for a supported body and MUST NOT expose plaintext on transformation failure. Other plugins and unmatched routes remain unchanged.

#### Scenario: JSON round trip

- **WHEN** a client sends a bounded application/jose request on a two-direction route
- **THEN** the downstream receives the exact JSON bytes and the client receives an encrypted JSON response with the upstream status

#### Scenario: Independent directions

- **WHEN** only one direction is configured
- **THEN** only that direction is transformed

#### Scenario: Fail closed

- **WHEN** the envelope is invalid, a buffer exceeds its bound, or an upstream representation cannot be encrypted
- **THEN** the request is rejected or an empty gateway error is returned, without exposing the protected body

Source SPEC comments:
- https://github.com/apache/shenyu/issues/8659#issuecomment-7220918184
