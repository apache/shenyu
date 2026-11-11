# Planning ShenYu changes

Read the applicable source, configuration, tests and current issues/PRs before
authoring a plan. Reuse existing work and distinguish merged behavior from
proposals. For ordinary issues use IssueSpec search's discussion scope; default
Proposal search alone does not cover ShenYu's existing backlog.

Write the user-visible problem, intended behavior, scope, compatibility and
non-goals. Give each SPEC observable WHEN/THEN scenarios, including invalid
input and relevant failure cases. Keep questions for genuinely unresolved
decisions; settle routine reversible choices using repository evidence.

Trace changed contracts through shared DTOs, Admin validation/persistence/events,
data sync, gateway caches/consumers and starters. Include maintained SQL dialects
and upgrade paths when persistence or seed metadata changes. Public SPI,
plugin names, defaults and supported legacy behavior need explicit treatment
when changed.

Design the smallest complete implementation and link TASKs to its SPECs.
TASK coverage needs both the SPEC ID in `Covers` and its canonical comment URL
in `Related Comments`; use IssueSpec's `link` command to maintain that relation.
Record verification for externally observable behavior, compatibility and
resource ownership. Add PROCESSes only for a concrete coordination need.
