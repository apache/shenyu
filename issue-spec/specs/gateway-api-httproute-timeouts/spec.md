# gateway-api-httproute-timeouts

## Purpose

Define the long-lived behavior contract for this capability.

Proposal Issues:
- https://github.com/apache/shenyu/issues/8413

## Requirements

### Requirement: HTTPRoute timeout budgets

ShenYu MUST distinguish the overall HTTPRoute request deadline from the backend attempt deadline, preserving legacy routes, streaming and retry ownership.

#### Scenario: Configured route

- **WHEN** either valid v1.5.1 timeout is supplied
- **THEN** publish the two budgets independently, treating zero as disabled, an omitted attempt budget as 3000 ms, and an omitted overall budget as unbounded by the route

#### Scenario: Invalid and legacy configuration

- **WHEN** a timeout is malformed or the backend budget exceeds an explicit finite request budget
- **THEN** report UnsupportedValue and withdraw stale configuration; correcting it restores routing. Omitted or empty timeout configuration follows the legacy path.

#### Scenario: Deadline through response completion

- **WHEN** an attempt or request exceeds its finite deadline
- **THEN** cancel its actual upstream work, apply request expiry across retries and backoff, return 504 on terminal timeout before commitment and terminate a committed stream without replay. Stop attempt timing at backend body completion.

Source SPEC comments:
- https://github.com/apache/shenyu/issues/8413#issuecomment-6785307075
