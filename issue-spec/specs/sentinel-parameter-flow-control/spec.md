# sentinel-parameter-flow-control

## Purpose

Define the long-lived behavior contract for this capability.

Proposal Issues:
- https://github.com/apache/shenyu/issues/8620

## Requirements

### Requirement: Opt-in Sentinel parameter flow control

A Sentinel rule MUST allow opt-in QPS limiting per nonblank configured header or exchange attribute value. Ordinary mode MUST remain the default. The two flow modes MUST reuse the threshold and be mutually exclusive. Circuit breaking MUST remain scoped to the existing resource. Invalid opt-in configuration MUST be rejected without replacing active enforcement at the gateway. Disabling, clearing or deleting a rule MUST remove its parameter enforcement without affecting other rules.

#### Scenario: Independent quotas in one rule

- **WHEN** requests with two configured parameter values match the same rule and one value exhausts its quota
- **THEN** only excess requests for that value are denied by parameter flow control; the other value retains its quota

#### Scenario: Missing or unusable value

- **WHEN** the selected value is missing, blank, ambiguous, oversized or unsupported
- **THEN** `SHARED_BUCKET` enforces a separate shared quota or `SKIP` omits only parameter flow control

#### Scenario: Legacy configuration and mode changes

- **WHEN** the parameter switch is absent or disabled, or the rule is switched or removed
- **THEN** ordinary settings still work as before and obsolete parameter enforcement is not retained

Source SPEC comments:
- https://github.com/apache/shenyu/issues/8620#issuecomment-7188040050
