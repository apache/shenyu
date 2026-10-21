# Instance state contract

The Admin API, instance charts, dashboard, and `instance_info.instance_state` use the same persisted codes:

| Code | API/chart name | Dashboard meaning |
| --- | --- | --- |
| `0` | `DELETED` | The instance exceeded the deletion timeout and is retained as an expired record. |
| `1` | `ONLINE` | A heartbeat was received within the liveness timeout. |
| `2` | `OFFLINE` | The liveness timeout elapsed, but the deletion timeout has not elapsed. |

`DELETED` describes a liveness state, not physical deletion of the database row. A subsequent accepted heartbeat allows the checker to return the instance to `ONLINE` using the existing identity.

Unrecognized numeric codes are displayed as `UNKNOWN`, rather than being relabeled as `DELETED`. Unknown is a presentation fallback, not a new stored code. The dashboard also uses that fallback for absent or malformed values; Admin continues its existing handling of legacy null states.

## Compatibility

The backend has always defined code `0` as `DELETED`. Older dashboard versions called it “unknown”; schema comments repeated that label. The fix corrects those labels and leaves all stored numeric values and timeout transitions intact. Do not convert historical `0` records to a new code. Upgrade SQL updates only the column description, preserving the existing column type and constraint.

Deploy the coordinated dashboard build with this contract. Charts continue to expose the canonical state names above. No data rewrite or new state migration is required.

## Bundled dashboard provenance

The Admin static resources were generated from `apache/shenyu-dashboard` commit `f239397e67c970c25a769f3b41ad3aaa7aca7a72`, based on `master@370cc40095ef4aa820449b7a9b736c7c98b6e9ab` (the same dashboard baseline previously bundled by Admin).

Build: Node.js `22.23.1`, npm `12.0.0`; `npm ci --no-audit --no-fund --allow-remote=all --ignore-scripts`, then `npm run build`. The install uses the checked-in lockfile; no dependency or lockfile was changed.

Generated JavaScript: `index.72158727.js`; SHA-256 `64ee3e37e174959916dc130b618347de618326aad1df2b3fe526c8537164f5b3`. The files under Admin's `static` directory match the build output byte for byte.
