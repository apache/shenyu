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

#### Scenario: keep built-in compatibility during Java API consolidation

- **WHEN** the orchestration and Java extension contracts are consolidated
- **THEN** existing Admin and selector configuration, byte-preserving native paths, built-in routing, replay eligibility, response metadata, and forwarded error paths retain established behavior

#### Scenario: isolate attempt observation from request results

- **WHEN** an attempt listener or logical-call observer throws while receiving bounded lifecycle facts
- **THEN** the request result remains unchanged and the event contains no prompt, response, credential, or endpoint data

Source SPEC comments:
- https://github.com/apache/shenyu/issues/8465#issuecomment-6816631104
