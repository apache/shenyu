# selector-discovery-read

## Purpose

Define the long-lived behavior contract for this capability.

Proposal Issues:
- https://github.com/apache/shenyu/issues/8525

## Requirements

### Requirement: Selector-scoped Admin discovery reads

The Admin GET /discovery endpoint MUST support an optional nonblank selectorId for selector-level discovery (level=0) and an optional nonblank discoveryType for plugin-level discovery (level=1). A selector-scoped definition MUST be attached to the requested regular selector through discovery_rel and discovery_handler, with matching plugin and namespace. A type-scoped definition MUST match the requested type, plugin and namespace. Return the existing DiscoveryVO or null. Unscoped calls MUST keep their existing result for zero or one matching definition. Ambiguous or invalid requests MUST return code 400 in the Admin result envelope and give separate explanations for blank values and unsupported levels. This read-only API returns the discovery definition. Existing selector detail reads continue to provide upstream data. Implementations of the original three-argument DiscoveryService method MUST keep serving unscoped reads through the default overload. Unsupported scoped reads MUST fail explicitly.

#### Scenario: Independent selector configurations

- **WHEN** two regular selectors in the same plugin and namespace each reference a level-0 discovery
- **THEN** GET /discovery with either selectorId returns only the corresponding definition even if more than one relation points to that same definition

#### Scenario: Missing and out-of-scope selectors

- **WHEN** the selector is missing, unbound, belongs to another namespace or is not linked to the requested plugin
- **THEN** the selector-scoped lookup returns null without falling back to a plugin-wide record

#### Scenario: Existing unambiguous reads

- **WHEN** the caller omits both discriminators and the existing plugin, level and namespace query matches zero or one row
- **THEN** the original null or single DiscoveryVO response remains valid

#### Scenario: Actionable ambiguous or invalid reads

- **WHEN** multiple distinct definitions match, or a discriminator is blank or used at an unsupported level
- **THEN** the response contains code 400 with guidance to specify selectorId for an ambiguous level-0 read or discoveryType for an ambiguous level-1 read. If a scoped read still matches multiple definitions, it reports duplicates.

#### Scenario: Multiple plugin discovery types

- **WHEN** a plugin has separate level-1 definitions for distinct discovery types in the same namespace
- **THEN** a request with discoveryType returns the matching definition or null for an unconfigured type. It does not fall back to a different definition.

Source SPEC comments:
- https://github.com/apache/shenyu/issues/8525#issuecomment-6975669687
