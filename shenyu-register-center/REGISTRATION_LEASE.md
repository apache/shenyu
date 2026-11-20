# Registration lease (`registration-lease-v1`)

Tracked by [#8039](https://github.com/apache/shenyu/issues/8039) under the
registration contract family ([#8038](https://github.com/apache/shenyu/issues/8038)).
This document describes the opt-in server-issued registration lease: an Admin that
owns the liveness of a business instance grants it a lease at the first beat,
renews it on every accepted beat, and marks the instance offline when the lease is
not renewed within its deadline. It is not an upstream-accepted specification.

## Guarantee

For an instance whose liveness is tracked through the legacy `/instance/beat`
endpoint, a lease makes the expiry deadline explicit and server-owned: the Admin
answers every capability-aware beat with the lease identity, the decided heartbeat
interval and the absolute epoch-millis deadline after which the instance is marked
offline. A lease renewal is idempotent — duplicate, delayed or replayed beats with
the same lease id can never push the deadline past the last server receipt — and a
successful renewal only advances liveness: it never overrides readiness admission,
boot-session fencing or URI health-probe outcomes, which live in their own state.

Not covered:

- Fenced business addresses (`boot-session-fencing-v1`). Their liveness is owned by
  the boot session; automatic release of an unreleased crashed owner remains the
  follow-up named in `REGISTRATION_FENCING.md`.
- Instances registered through an external registry (Nacos, etcd, ZooKeeper). Those
  registries own membership and expiry; the lease applies only to the direct HTTP
  registration path and never removes or resurrects a registry-owned instance.
- Persisted lease state. The lease lives in the memory of the Admin that owns the
  liveness checks (the cluster master, which receives forwarded beats and rebuilds
  from the database on failover). An Admin restart invalidates every outstanding
  lease; clients recover by re-registering, the same path an expired lease takes.

## Enabling

Admin:

```yaml
shenyu:
  register:
    lease:
      enabled: true
```

When the switch is off, no capability is advertised and `/instance/beat` keeps its
exact legacy v1 behavior. The capability is advertised on the
`X-Shenyu-Register-Capabilities` response header of every registration endpoint,
next to `boot-session-fencing-v1` when that is also enabled.

Interval bounds (`shenyu.register.lease.*`): `interval` (default 5s, issued to
clients that do not propose one), `min-interval` (2s) and `max-interval` (60s) clamp
every client proposal, and `expiry-factor` (default 3) sets the offline deadline to
`receipt + expiryFactor * interval`.

## Wire format

A lease-capable client adds the capability to its request header
(`X-Shenyu-Register-Capabilities: registration-lease-v1`) and two optional fields to
the beat payload: `leaseId` (the presented lease, blank on first registration or
re-registration) and `proposedIntervalSeconds`. Legacy Admins ignore the unknown
fields, so the payload stays v1-compatible.

The response depends on the request:

- Without the capability header, the response is the exact legacy plain text
  (`success` / `parameter error`).
- With the capability header, the response is a JSON document
  `{code, leaseId, intervalSeconds, expiryDeadlineMs, message}`. `OK` grants or
  renews the lease; `STALE_LEASE` (unknown lease id) and `EXPIRED` are terminal for
  the presented lease — the client drops it and re-registers with a blank lease id;
  `LEASE_NOT_ENABLED` and `INVALID_REQUEST` are rejections that leave the liveness
  state untouched.

A rejected or legacy beat never refreshes the liveness timestamp, so a client that
lost its lease cannot keep its instance alive without re-registering. A
re-registration beat (blank lease id) attaches to the current lease of that instance
without resetting its deadline — only renewals that present the matching lease id
extend the deadline.

## Client contract

The Java reporter keeps its fixed 5-second scheduler and gates each Admin by the
server-issued interval; because the smallest negotiable interval is 2s and the
offline deadline is three intervals, the tick granularity can never miss a deadline.
A renewal is compact: the identity plus the lease id, with the full instance
observation attached only on (re-)registration and once every 12 consecutive
renewals, so a beat stays a liveness proof instead of an accidental full
re-registration. Transport failures defer the next attempt by the current interval
plus up to 10% jitter, bounded; there is no failback registration loop.

## Compatibility

Legacy clients never send the capability header and receive the exact legacy
response. Legacy Admins (or a disabled lease) receive capability headers from
upgraded clients and answer the legacy marker, which the client treats as the v1
cadence. A mixed Admin cluster negotiates per server: the client keeps independent
lease state, interval and login credentials per Admin url.
