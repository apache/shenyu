# selector-discovery-read

## Purpose

Define the long-lived behavior contract for this capability.

Proposal Issues:
- https://github.com/apache/shenyu/issues/8525

## Requirements

### Requirement: Selector-scoped Admin discovery reads

The Admin GET /discovery endpoint MUST support an optional nonblank selectorId for selector-level discovery (level=0). A matching definition MUST be attached to the requested regular selector through discovery_rel and discovery_handler, with matching plugin and namespace. Return the existing DiscoveryVO or null. Calls without selectorId MUST keep their existing result for zero or one matching definition. An ambiguous lookup or an invalid selectorId/level combination MUST return a clear code 400 in the Admin result envelope. Do not pick an arbitrary definition. This read-only API returns the discovery definition. Existing selector detail reads continue to provide upstream data.

#### Scenario: Independent selector configurations

- **WHEN** two regular selectors in the same plugin and namespace each reference a level-0 discovery
- **THEN** GET /discovery with either selectorId returns only the corresponding definition even if more than one relation points to that same definition

#### Scenario: Missing and out-of-scope selectors

- **WHEN** the selector is missing, unbound, belongs to another namespace or is not linked to the requested plugin
- **THEN** the selector-scoped lookup returns null without falling back to a plugin-wide record

#### Scenario: Existing unambiguous reads

- **WHEN** the caller omits selectorId and the existing plugin, level and namespace query matches zero or one row
- **THEN** the original null or single DiscoveryVO response remains valid

#### Scenario: Actionable ambiguous or invalid reads

- **WHEN** multiple distinct definitions match, or selectorId is blank or used with a level other than 0
- **THEN** the response contains code 400 and a fixed explanation, including advice to pass selectorId when an unscoped selector-level query is ambiguous

Source SPEC comments:
- https://github.com/apache/shenyu/issues/8525#issuecomment-6975669687
