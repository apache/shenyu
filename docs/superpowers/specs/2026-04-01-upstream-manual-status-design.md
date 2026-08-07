# Upstream Manual Status Design

## Goal

Add a persisted manual upstream control flag that lets Admin force a discovery upstream offline without being overwritten by heartbeat recovery, and make Gateway honor that flag during upstream selection.

## Background

Today `discovery_upstream.upstream_status` is used for automatic liveness. Admin-triggered manual offline and automatic health recovery share the same status channel, so a heartbeat or recovery path can bring a manually disabled upstream back into traffic.

## Chosen Approach

Use a separate manual status field.

- Persist `manual_status` on `discovery_upstream` with default `NONE`.
- Represent manual control with a shared enum `NONE` and `FORCE_OFFLINE`.
- Keep `upstream_status` for automatic health only.
- Make Admin manual APIs write only `manualStatus`.
- Let heartbeat or recovery logic skip `status=true` updates when `manualStatus == FORCE_OFFLINE`.
- Include `manualStatus` in discovery sync payloads and Gateway cache objects.
- Filter `FORCE_OFFLINE` upstreams before load-balancer selection.

This keeps automatic and manual state independent and avoids hidden coupling.

## Alternatives Considered

### Reuse `upstream_status`

Rejected because heartbeat and health check would continue to overwrite manual operations.

### Keep manual state only in Gateway memory

Rejected because it would not survive restarts or sync across Admin and Gateway nodes.

## Data Model

Add `manual_status varchar(32) not null default 'NONE'` to `discovery_upstream`.

Shared enum:

- `NONE`
- `FORCE_OFFLINE`

`/upstream/online` resets the field to `NONE`.

## Admin API

Add a new Admin controller rooted at `/upstream` with:

- `POST /upstream/offline`
- `POST /upstream/online`

Request body will identify the upstream by `selectorId` and `url`.

Behavior:

- Look up the related discovery handler by selector id.
- Update only `manual_status`.
- Publish a fresh `DISCOVER_UPSTREAM` event built from current DB data so gateways receive the new flag immediately.

## Status Update Rules

Automatic writers keep their current responsibility for `upstream_status`.

Additional rule:

- If a write intends to mark an upstream alive (`status=true`) and the record is `FORCE_OFFLINE`, skip the status update.

This protects the manual offline decision from heartbeat recovery without blocking automatic offline transitions.

## Sync Contract

Extend `DiscoveryUpstreamData` and all transfer paths to include `manualStatus`.

Admin event producers and Gateway sync consumers will continue to use the same payload shape, now with one extra field.

## Gateway Behavior

Extend cached upstream objects with `manualStatus`.

Gateway will filter out `FORCE_OFFLINE` upstreams before selection. This ensures:

- Manually offline nodes are never chosen.
- Existing health-check metadata can still be retained.
- Re-enabling an upstream only requires Admin to push a new sync event with `manualStatus=NONE`.

## Testing Strategy

- Admin service tests for manual status update and status recovery short-circuit.
- Sync/transfer tests for `manualStatus` propagation.
- Load-balancer tests for filtering `FORCE_OFFLINE`.
- Divide discovery handler test for mapping sync payload to cached upstream manual status.
