# Implementing ShenYu changes

Read the complete current Design, the selected SPECs/TASKs and applicable
decisions. Resolve contradictory requirements before dependent work. Capture
the relevant revision and keep implementation, durable specs and tests aligned
when scope changes; do not turn a stale summary into the source of truth.

Follow AGENTS.md's Maven-wrapper, compatibility and surgical-change guidance.
Use existing plugin/SPI/configuration mechanisms. Complete the affected
Admin-to-gateway contract, including reload and invalid-update behavior.

For reactive work exercise subscription, completion, error, cancellation,
backpressure and buffer ownership as applicable. Verify cancellation reaches
the original upstream operation, not merely a wrapper future. Consider
configuration races, credentials and lifecycle cleanup when they are affected.

Run focused meaningful checks first and broaden for concrete risk or repository
requirements. Report exact commands, actual results and important unrun checks.
Update the selected TASKs and durable behavior specifications without claiming
that a planning check proves tests, human approval or merge readiness.
