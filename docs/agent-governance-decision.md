# Agent governance decision contract

Agent Gateway governance consumers use one versioned decision contract before introducing MCP quotas or LLM budgets. A decision identifies its consumer, policy/configuration versions, shadow or enforce mode, result, bounded reason, correlation id, ledger generation and effective time. Tenant, subject and trace id are not labels; they remain in the existing trusted request context and metadata-only audit event.

## Results and modes

- `ALLOW` means policy permits the call.
- `WOULD_DENY` is valid only in shadow mode. It is reported and counted in an isolated shadow ledger but must not reject traffic or debit production quota/budget state.
- `DENY` is valid only in enforce mode and is the only result that may reject traffic.

Authentication, authorization and fundamental resource-limit failures are mandatory denies in both modes. They are represented as enforce decisions even when the policy itself is shadow, so shadow mode cannot bypass security or base resource protection.

## Isolated shadow state and transitions

Each shadow ledger generation is immutable. Configuration replacement creates a new generation and closes the old ledger to new evaluations; existing in-flight reservations continue settling or cancelling against the generation that admitted them, and no state is moved or double-settled between ledgers. The bounded reference ledger reserves capacity for shadow `ALLOW` evaluations and records subsequent `WOULD_DENY` evaluations with bounded reason counters, accepted/observed/overflow/settled/cancelled/expired values, and explicit close. Overflow drops only shadow evaluation state, never traffic. Actual usage continues through the existing metering and terminal-event owners.

Shadow reservations have a bounded TTL. Expiry only releases that reservation's capacity and records an expired count; it does not reject traffic or debit a production policy. Cancellation and settlement are idempotent and mutually exclusive for the same reservation.

Shadow/enforce comparison is explicit: `SAME_EFFECT`, `SHADOW_STRICTER`, or `ENFORCE_STRICTER`. Comparisons require the same consumer, versions, ledger generation and correlation id, and are observations for rollout; they do not retroactively change an execution decision.

`AgentGovernanceContractLabels` is the metric boundary and exposes only consumer, mode, result and reason. `AgentGovernanceAuditAdapter` maps shadow would-deny to an audit `ALLOWED` terminal with a `SHADOW_*` reason; enforce deny maps to `REJECTED` with the bounded reason.

## Non-goals

This contract does not implement Redis quota admission, token reservation, cost settlement, provider usage parsing, or execution-point wiring. Those belong to #7957 and #7958 and must consume this vocabulary rather than invent local switches.
