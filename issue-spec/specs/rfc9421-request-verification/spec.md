# rfc9421-request-verification

## Purpose

Define the long-lived behavior contract for this capability.

Proposal Issues:
- https://github.com/apache/shenyu/issues/8660

## Requirements

### Requirement: Verify signed HTTP requests before routing

Matched requests MUST verify with the trusted Ed25519 key for the matched rule before routing or rewriting. The signature MUST cover method and target URI, and for bodies, Content-Type and a verified SHA-256 Content-Digest. Requests outside the validity window MUST be rejected. If replay protection is enabled, forwarding requires a successful atomic nonce claim after verification. Unmatched legacy rules are unchanged.

#### Scenario: Valid signed body

- **WHEN** a request has a valid signature, digest and time window
- **THEN** the original bytes reach the downstream chain

#### Scenario: Rejected message

- **WHEN** coverage, signature, digest or timestamps are invalid
- **THEN** no nonce is reserved and the chain is not called

#### Scenario: Concurrent delivery

- **WHEN** two gateway instances sharing replay state verify the same signed nonce
- **THEN** at most one reaches the upstream; store errors also prevent forwarding

#### Scenario: Required replay protection without a store

- **WHEN** a signature is otherwise valid and its rule requires replay protection but the gateway has no shared store
- **THEN** return 503 without forwarding; verification-only rules can still run without Redis

#### Scenario: Rewrite a verified route

- **WHEN** the original target and body verify and the route then rewrites the upstream path
- **THEN** forwarding uses the rewritten path; a signature for a different original target is rejected before rewriting

Source SPEC comments:
- https://github.com/apache/shenyu/issues/8660#issuecomment-7220915200
