# opt-in-local-uri-registration

## Purpose

Define the long-lived behavior contract for this capability.

Proposal Issues:
- https://github.com/apache/shenyu/issues/8497

## Requirements

### Requirement: Local URI registration for the opt-in Java client

Applications using @EnableClientRegister MUST publish local upstream URIs through the existing registration pipeline only in local discovery mode (the default). Each eligible instance identity (namespace, RPC type, application, context path, host, port) MUST be enqueued once rather than once per API or refresh; failed enqueue MUST remain retryable. URI-specific processor filtering MUST apply. Disabled registration MUST create no registration pipeline. Registry discovery MUST retain metadata behavior without local URI publication. Existing repository failback and lifecycle handling MUST retain the published identity; queued work MUST NOT be represented as an accepted Admin acknowledgement. Legacy defaults and public registration SPI remain unchanged.

#### Scenario: Local instance with multiple APIs and namespaces

- **WHEN** an enabled opt-in servlet application with two eligible APIs starts in local discovery and two namespaces
- **THEN** metadata for both APIs is published in both namespaces; one URI per namespace preserves the configured live host, port, RPC type, app and context and an actual Admin/bootstrap route returns the backend response

#### Scenario: Repeated refresh and processor exclusion

- **WHEN** the same APIs are rediscovered, or a URI processor excludes all of a bean's APIs
- **THEN** rediscovery does not multiply URI work; excluded APIs do not cause a URI registration

#### Scenario: Registry discovery and disabled registration

- **WHEN** discovery is non-local, or shenyu.register.enabled is false
- **THEN** registry mode publishes supported metadata and no local URI; disabled mode constructs no registration pipeline and publishes no registration traffic

#### Scenario: Malformed configuration and retryable failures

- **WHEN** the configured port is malformed, publication throws, or one namespace's HTTP registration fails transiently
- **THEN** the malformed instance is not admitted as a usable URI; a failed enqueue can be tried on refresh and existing failback retries the failed HTTP identity without suppressing successful namespaces

#### Scenario: Lifecycle identity

- **WHEN** successfully registered URIs are heartbeated and taken offline
- **THEN** the payloads retain each URI's namespace, RPC type, application, context, host and port; test evidence distinguishes HTTP attempts from successful acknowledgements

Source SPEC comments:
- https://github.com/apache/shenyu/issues/8497#issuecomment-6905917087
