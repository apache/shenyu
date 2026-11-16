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

The Admin static resources were generated from dashboard source commit `2ea489ba2865dd41629e4b99935b4a4cec8efe2a` (tree `4a0123984992a9de39458a35785069c8b5fd3f9e`), based on `apache/shenyu-dashboard@5a9e5ae0734fb3ca90b6287479af478dc7480797`. This is the dashboard baseline already shipped by ShenYu master `80559e842c5098ded24ded8390b5d361db0ef4e9` through [ShenYu #8280](https://github.com/apache/shenyu/pull/8280).

Only the instance-state changes from dashboard source companion [#727](https://github.com/apache/shenyu-dashboard/pull/727), commit `f239397e67c970c25a769f3b41ad3aaa7aca7a72`, are applied to that baseline. The source patch changes the English and Chinese state labels and instance renderer, with regression tests. It retains the shipped MCP initial state, removed API-document route, and policy time-formatting guards.

Build: Node.js `22.23.1`, npm `10.9.4`; `npm ci --offline --no-audit --no-fund`, then `NODE_OPTIONS=--openssl-legacy-provider npm run build`. The install uses the checked-in lockfile; no dependency or lockfile was changed.

Generated JavaScript: `index.d6934e98.js`; SHA-256 `9265d5fc61bfcd0dcafd0df4f74d39d68e041450af9bd92f37b13342d0b09b67`. The active JavaScript and HTML match the build output byte for byte. CSS, favicon, and both images are byte-identical to the previous master bundle; HTML changes only the JavaScript filename. Reversing only the four compiled state-label changes makes the JavaScript byte-identical to the previous `index.600a112f.js`.

Validation: all 54 dashboard test suites / 325 tests passed, full lint passed with existing warnings, and the production build passed. The five targeted backend test classes passed 86 tests with no failures, errors, or skips; the Maven reactor compiled successfully and passed Checkstyle. Native database upgrade scripts and a live Admin heartbeat/dashboard deployment were not run.
