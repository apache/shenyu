# opt-in-sort-first-matching

## Purpose

Define the long-lived behavior contract for this capability.

Proposal Issues:
- https://github.com/apache/shenyu/issues/8553

## Requirements

### Requirement: Opt-in sort-first gateway matching

The gateway property shenyu.match-sort-first MUST be a boolean and default to false. With true, when enabled selectors or conditional rules match, the gateway MUST prefer the lowest sort, then the highest existing specificity. With false or omission, it MUST prefer the highest existing specificity, then the lowest sort. AND specificity is the number of conditions and OR specificity is zero. An exact tie MUST retain the first encountered candidate. Existing selector eligibility, continuation and the full-flow last-enabled-rule behavior MUST be preserved. Match-cache settings and updates MUST yield the same winner as uncached matching for the current candidate data.

#### Scenario: Compatibility by default

- **WHEN** the property is absent or false and a lower-sort match competes with a more specific match
- **THEN** the gateway selects the more specific match and only compares sort after a specificity tie

#### Scenario: Explicit priority

- **WHEN** the property is true and overlapping candidates have different sort values
- **THEN** the lowest-sort eligible match wins even when another candidate has more conditions

#### Scenario: Deterministic ties

- **WHEN** the primary key ties
- **THEN** the secondary key breaks the tie and the first encountered candidate wins if both keys tie

#### Scenario: Existing match eligibility

- **WHEN** an inactive or nonmatching candidate has the lowest sort or a selected full-flow selector has several rules
- **THEN** inactive and nonmatching candidates remain ineligible and the full-flow path still uses the last enabled rule

#### Scenario: Sort updates and caching

- **WHEN** a selector or rule sort update reaches the gateway
- **THEN** subsequent matching uses the new candidate order with either cache setting and existing cache safety rules

#### Scenario: Selector-scoped cached rules

- **WHEN** two requests share a URI but evaluate to different selectors and the first request warms a positive rule cache entry
- **THEN** the next request selects a rule from its own selector even if the earlier rule also matches the URI

#### Scenario: Invalid deployment value

- **WHEN** the gateway starts with a value that cannot bind to a boolean
- **THEN** Spring rejects the invalid configuration during binding

Source SPEC comments:
- https://github.com/apache/shenyu/issues/8553#issuecomment-7121340086
