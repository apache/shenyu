# opt-in-local-uri-registration

## Purpose

Define the long-lived behavior contract for this capability.

Proposal Issues:
- https://github.com/apache/shenyu/issues/8497

## Requirements

### Requirement: Local URI registration for the opt-in Java client

Applications using @EnableClientRegister MUST publish local upstream URIs through the existing registration pipeline only in local discovery mode (the default). Each eligible instance identity (namespace, RPC type, application, context path, host, port) MUST be enqueued once rather than once per API or refresh; a failed enqueue MUST remain retryable without blocking other namespaces. URI-specific processor filtering MUST apply. Registry discovery MUST retain metadata behavior without local URI publication. Existing repository failback and lifecycle handling MUST retain the published identity; queued work MUST NOT be represented as an accepted Admin acknowledgement. HTTP and WebSocket URIs MUST retain the configured protocol, defaulting to http:// and ws:// respectively. Existing configuration implementations remain compatible with the optional protocol accessor. Global opt-in registration disable is owned by client-registration-validation (#8504).

#### Scenario: Local instance with multiple APIs and namespaces

- **WHEN** an enabled opt-in servlet application with two eligible APIs starts in local discovery and two namespaces
- **THEN** metadata for both APIs is published in both namespaces; one URI per namespace preserves the configured live host, port, RPC type, app, context and protocol. An actual Admin/bootstrap route returns the backend response for the HTTP fixture

#### Scenario: Repeated refresh and processor exclusion

- **WHEN** the same APIs are rediscovered, or a URI processor excludes all of a bean's APIs
- **THEN** rediscovery does not multiply URI work; excluded APIs do not cause a URI registration

#### Scenario: Registry discovery

- **WHEN** the configured discovery mode is not exactly local
- **THEN** the local URI registrar is absent, while supported metadata is still published

#### Scenario: Unresolved ports and retryable failures

- **WHEN** an HTTP/WebSocket port is temporarily unresolved (0 or -1), a port is malformed or out of range, or one namespace's publication fails
- **THEN** an unresolved port skips local URI publication for this refresh without failing startup. No malformed or out-of-range port is published. A failed enqueue does not prevent other namespaces from enqueueing and can be retried on rediscovery. Existing HTTP failback retries a failed transport identity. Actual bound-port resolution is tracked in #8426

#### Scenario: Lifecycle identity

- **WHEN** successfully registered URIs are heartbeated and taken offline
- **THEN** the payloads retain each URI's namespace, RPC type, application, context, host, port and protocol. Test evidence distinguishes HTTP attempts from successful acknowledgements

Source SPEC comments:
- https://github.com/apache/shenyu/issues/8497#issuecomment-6905917087
