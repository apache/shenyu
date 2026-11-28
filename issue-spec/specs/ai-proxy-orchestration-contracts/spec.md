# ai-proxy-orchestration-contracts

## Purpose

Define the long-lived behavior contract for this capability.

Proposal Issues:
- https://github.com/apache/shenyu/issues/8465

## Requirements

### Requirement: cohesive AI Proxy planning and execution contracts

The AI Proxy MUST separate request planning from reactive attempt execution, MUST keep retry and fallback policy outside Protocol, Provider, Converter, and Transport implementations, and MUST expose explicit Provider capabilities and request-scoped response adaptation while preserving built-in wire, configuration, replay-policy, streaming, cancellation, commitment, and observation behavior.

#### Scenario: prepare an exchange without transport execution

- **WHEN** a valid native or converted AI request is planned
- **THEN** the planner prepares routing, conversion, Provider request, response adapters, and replay metadata without opening an upstream connection

#### Scenario: coordinate retry and fallback with cohesive state

- **WHEN** an eligible upstream status or transport failure occurs before downstream output commitment
- **THEN** the execution pipeline applies retry, Retry-After, shared-deadline, replay-safety, and fallback decisions without moving policy into an extension implementation

#### Scenario: preserve committed streaming behavior

- **WHEN** streaming output has reached downstream or the active subscription is cancelled
- **THEN** the pipeline starts no replay, preserves backpressure, and propagates cancellation with per-subscription conversion state

#### Scenario: prepare through the explicit Provider contract

- **WHEN** the planner invokes a registered Provider
- **THEN** the Provider explicitly declares capabilities and returns the upstream request with its request-scoped response adapter through prepareRequest

#### Scenario: keep orchestration ownership focused

- **WHEN** the Engine plans and executes one logical request
- **THEN** `AiExchangePlanner` owns decoding, routing, conversion, Provider preparation and prepared metadata; `AiExecutionCoordinator` owns subscription-local execution and caching; and the attempt orchestrator, executor and response consumer own retry/fallback transitions, one upstream subscription, response commitment, backpressure and cancellation

#### Scenario: organize implementation types by responsibility

- **WHEN** planning and execution implementation types are organized into focused packages
- **THEN** request preparation belongs to `service.planning`, logical-call coordination and retry/fallback/repair transitions belong to `service.orchestration`, one-attempt transport and response ownership belong to `service.execution`, failure detection and classification belong to `service.failure`, and structured-output classification and repair-request construction belong to `service.structured`

#### Scenario: construct the Engine through one supported public contract

- **WHEN** a programmatic integration constructs an `AiProxyEngine`
- **THEN** it uses `AiProxyEngine.builder(protocols, providers, routeResolver, transport)` and optional policy setters without requiring direct construction of planning, orchestration, execution, or failure implementation types

#### Scenario: allow internal collaboration across focused packages

- **WHEN** planning, orchestration, execution, or failure implementation types must collaborate across Java package boundaries
- **THEN** those types may use Java `public` visibility only as needed for internal composition, while remaining implementation details rather than supported SPI, extension points, or compatibility contracts

#### Scenario: migrate Java extension implementations explicitly

- **WHEN** an implementation migrates from the previous experimental Java API
- **THEN** it replaces Provider `buildRequest` with explicit `capabilities` and `prepareRequest`, replaces Engine constructor overloads with the builder, moves `service.AiProxyConfigService` to `service.config.AiProxyConfigService`, and updates event and observation imports to their focused packages

#### Scenario: deny exact caching by default

- **WHEN** a custom Protocol or Provider does not explicitly declare exact-response-cache compatibility for the target protocol
- **THEN** the prepared exchange is not eligible for exact response caching

#### Scenario: count cache bypasses at the configured cache boundary

- **WHEN** the exact response cache component is configured and a request is forwarded without a cache lookup or fill
- **THEN** the cache bypass counter increases, including for requests rejected by stream, fallback, Protocol, Provider, route, request-shape, authorization, or fill-capacity eligibility checks

#### Scenario: keep built-in compatibility during Java API consolidation

- **WHEN** the orchestration and Java extension contracts are consolidated
- **THEN** existing Admin and selector configuration, byte-preserving native paths, built-in routing, replay eligibility, response metadata, and forwarded error paths retain established behavior

#### Scenario: isolate attempt observation from request results

- **WHEN** an attempt listener or logical-call observer throws while receiving bounded lifecycle facts
- **THEN** the request result remains unchanged and the event contains no prompt, response, credential, or endpoint data

Source SPEC comments:
- https://github.com/apache/shenyu/issues/8465#issuecomment-6816631104
