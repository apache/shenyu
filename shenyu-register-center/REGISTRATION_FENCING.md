# Registration boot-session fencing

Tracked by [#8040](https://github.com/apache/shenyu/issues/8040). This document
describes the opt-in contract implemented by `boot-session-fencing-v1` and the
policy for legacy, mixed and provider-managed registrations. It is not an
upstream-accepted specification.

## Guarantee

An Admin-validated boot session owns one business instance address. Once the
target is protected, a request from an earlier process (heartbeat, offline,
instance update or registration retry) is rejected before it changes the active
selector handle, the discovery upstream, the address-only diagnostic cache or the
published configuration event.

Not covered:

- Configuration packets that the previous owner committed legitimately *before*
  the takeover and that are merely delivered out of order on the network. This PR
  does not add a global snapshot revision or gateway receive-order guarantee.
- Restoring an older database backup. The `generation` stored in the same
  database is not a disaster-recovery fence.
- Automatic lease expiry. Without a lease, an unreleased crashed owner keeps
  conflicting until an operator performs an explicit replacement.

## Enabling

Admin:

```yaml
shenyu:
  register:
    fencing:
      enabled: true
```

The capability header is advertised only when this switch is on. Clients keep
using the legacy v1 endpoints when the property is absent, and no client falls
back automatically.

HTTP client (`shenyu.register` properties):

```properties
registrationFencingDomains=domainA|targetA|http://admin-a:9095,http://admin-a2:9095|||10.0.0.10|8080|http|ns-a|orders;domainB|targetB|http://admin-b:9095|||10.0.0.11|8081|http|ns-b|payments
registrationFencingStartupId=00000000-0000-4000-8000-000000000001
registrationFencingExpectedGeneration=0
```

Each entry is `domainId|targetId|server1,server2|startupId|expectedGeneration|host|port|protocol|namespaceId|serviceName`.
The host, port, protocol, namespace and service name are required so each URI
operation is sent only to its matching target. The service name is normalized
with the same context-path rule used when Admin creates a selector, so `orders`
matches the selector named `/orders`. Servers inside one entry are aliases of
the same shared database; omitting the server field uses the global `serverList`.
A client keeps an independent session and sequence per target. Startup UUID and
expected generation fields must match the values used by an operator
replacement; an omitted startup UUID is generated for the current process.

## Operator flow

| Operation | Endpoint | Notes |
| --- | --- | --- |
| Pre-provision a target | `POST /registration-fencing/targets` | existing Admin/root principal; binds an address already present in the selector's discovery upstream and one business principal |
| Explicit takeover | `POST /registration-fencing/targets/{targetId}/replace` | CAS on current owner and generation plus a fixed `requestId` |
| Disable | `POST /registration-fencing/targets/{targetId}/disable` | terminates the session, keeps the target and tombstone |
| Acquire | `POST /shenyu-client/registration-sessions/acquire` | business principal; accepts only `domainId`, `targetId`, `startupId`, `expectedGeneration` |
| Mutate | `POST /shenyu-client/registration-sessions/{sessionId}/mutate` | synchronous `REGISTER` / `HEARTBEAT` / `UPDATE` / `OFFLINE`, ordered by `clientSequence` |

`OFFLINE` is terminal: the session stays readable for a replay of the exact
committed sequence, but a later `REGISTER` cannot revive it. A disabled target
no longer protects its selector, so the normal management operations become
available again; management edits, discovery rebinding and deletion are rejected
while the target is enabled.

## Per-registration-type policy

- **URI lifecycle** (`REGISTER`, `HEARTBEAT`, instance `UPDATE`, `OFFLINE`,
  draining): the boot session and sequence path; legacy writers are rejected at
  the actual write point for an enabled target.
- **Metadata**: shared service configuration. A fenced selector is provisioned
  by the operator; automatic metadata writes that would touch it are rejected,
  and a fencing client does not send its automatic metadata.
- **Discovery config**: provider/selector-level configuration. Automatic
  rebinding and queued rebinding tasks are rejected for a protected selector.
- **MCP tools**: metadata/rule-level configuration, not instance membership. The
  automatic write to a protected selector is rejected.
- **API documentation**: an independent document resource. It is not claimed to
  be instance-fenced; the service does not write selector members.
- **gRPC / WebSocket**: adapters of the same URI service. Their actual shared
  write points use the same guard; no new transport protocol is introduced.
- **Provider callbacks and background probes**: provider-managed targets must be
  isolated from fenced selectors; conflicting writes are rejected.

## Compatibility

- Legacy headerless/v1 endpoints keep their success body and behaviour. A target
  without a fencing record is unchanged and gains no stale-request guarantee.
- An old Admin without the capability cannot serve the new endpoints. A new
  client fails explicitly instead of silently downgrading; legacy mode must be
  chosen deliberately, and it has no fencing guarantee.
- An old Admin binary cannot read the added state; it must be stopped before the
  cluster enables protection.
- A clean restart performs `OFFLINE`, which ends the session. The next process is
  granted through an explicit replacement; a crash without `OFFLINE` requires the
  same operator action.
