# Discovery upstream labels

Divide discovery upstreams can carry labels in their existing `props` JSON:

```json
{
  "warmup": 10,
  "gray": false,
  "healthCheckEnabled": true,
  "labels": {
    "release": "stable",
    "region": "east"
  }
}
```

Labels must be a JSON object with nonblank string keys and values. Numeric,
boolean, null, array and object values are rejected rather than converted to
strings. Admin validates discovery-upstream writes, proxy-selector editor writes
and configuration imports before persistence or notification.

The existing `props` string carries labels through configuration synchronization;
its other fields and nested extension objects are preserved. Gateway handlers
read scalar metadata separately from labels, so a scalar `release` property can
coexist with `labels.release` without overwriting either value.

Updates replace the submitted properties; they do not merge with previous
properties. Omitting `labels` or submitting `"labels": {}` clears previous labels.
Submitting `"labels": null` is invalid. When editing labels, include any other
properties that should remain configured.

Labels are exposed as immutable snapshots on cached `Upstream` instances. Updates
refresh labels and scalar metadata on existing healthy or unhealthy instances,
preserving their health-check and request statistics. Cache updates distinguish
logical targets with the same address but different service groups or versions.

Labels are descriptive data in this change. Existing node selection, legacy
`gray` filtering, health checks, retries and metrics retain their behavior.
