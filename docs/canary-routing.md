<!--
Licensed to the Apache Software Foundation (ASF) under one or more
contributor license agreements. See the NOTICE file distributed with
this work for additional information regarding copyright ownership.
The ASF licenses this file to You under the Apache License, Version 2.0
(the "License"); you may not use this file except in compliance with
the License. You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.
-->

# Divide Canary routing and migration

## Overview and availability

Divide can separate healthy HTTP upstreams into **Stable** and **Canary** pools using labels. A rule first checks request conditions, then assigns an eligible identifier to a pool using a percentage. Load balancing selects a node inside that pool.

This guide covers development builds containing [upstream labels](https://github.com/apache/shenyu/pull/7847), [Canary configuration](https://github.com/apache/shenyu/pull/7963), [retry partition preservation](https://github.com/apache/shenyu/pull/8076), and [Canary metrics](https://github.com/apache/shenyu/pull/8090). Check that your Admin and gateway distribution includes these changes before enabling the configuration. Older releases, including 2.7.1, do not provide this configuration.

The examples use the default Canary decision service. Custom decision services may implement different assignment behavior.

```text
Match Divide selector and rule
  -> Evaluate Canary conditions and identifier bucket
  -> Filter healthy upstreams by the selected labels
  -> Apply initial empty-Canary-pool policy
  -> Apply circuit breaker and load balancing within the resolved pool
  -> Forward the request and perform configured retries
  -> Record one final Canary request outcome
```

## Quick start

### Prepare two upstream versions

Enable [Divide](https://shenyu.apache.org/docs/plugin-center/proxy/divide-plugin/) and configure an HTTP selector and rule following [Selectors and Rules](https://shenyu.apache.org/docs/user-guide/admin-usage/selector-and-rule/). For this example, both must match `/version`, and no earlier rule should intercept that path. Use [LOCAL discovery](https://shenyu.apache.org/docs/user-guide/discovery/discovery-mode/) for a repeatable first test.

Run two HTTP services, reachable from the gateway, with `GET /version` returning different bodies:

| Upstream | Response body | Labels |
| --- | --- | --- |
| `127.0.0.1:8081` | `stable` | `{"release":"stable"}` |
| `127.0.0.1:8082` | `canary` | `{"release":"canary"}` |

These loopback addresses assume the services and gateway share a network namespace. Use reachable service addresses for containers or separate machines. Verify both upstreams are healthy before testing.

### Add upstream labels

Labels belong to each discovery upstream's **`props.labels`**, separate from `metadata` and the legacy `gray` flag. Merge them into the existing properties. For example, the parsed properties for the Stable node are:

```json
{
  "labels": {
    "release": "stable"
  }
}
```

For the Canary node, use `"release": "canary"`. Keep any existing health-check, warmup, metadata, and `gray` properties when updating a node.

If the dashboard form does not expose nested labels, use the authenticated Admin API: `POST /discovery-upstream` creates a node, or updates it when its existing `id` is supplied. Preserve the full upstream DTO, including `id`, `discoveryHandlerId`, `namespaceId`, `protocol`, `url`, `status`, and `weight`. Its `props` field is a **JSON string**, so the property above is represented in the request as:

```json
{
  "props": "{\"labels\":{\"release\":\"stable\"}}"
}
```

This is a field illustration, not a complete API request. `PUT /discovery-upstream/{discoveryHandlerId}` replaces the entire node list for that handler; do not use it as a partial label update. Omitting `labels` or supplying an empty label object in updated properties clears the node's labels.

### Configure the rule handle

Put `canary` inside the Divide rule's **`handle`**, not inside its selector or its outer rule-matching conditions. Save the following example as `canary-handle.json`:

```json
{
  "loadBalance": "random",
  "timeout": 3000,
  "retry": 0,
  "retryStrategy": "current",
  "canary": {
    "enabled": true,
    "conditions": [],
    "matchMode": 0,
    "percentage": 10,
    "stickyKey": {
      "paramType": "header",
      "paramName": "X-Canary-Key"
    },
    "stableLabels": {
      "release": "stable"
    },
    "canaryLabels": {
      "release": "canary"
    },
    "fallbackPolicy": "STABLE"
  }
}
```

For an existing production rule, merge the `canary` object into its handle and review the retry settings; preserve unrelated handle fields. The example disables retries to make initial routing easy to verify.

The Admin API updates a rule through `PUT /rule/{id}`. The DTO's `handle` is also a JSON string. Starting with an existing valid update request saved as `rule-update.json`, encode the handle with `jq`:

```shell
jq --slurpfile handle canary-handle.json \
  '.handle = ($handle[0] | tojson)' \
  rule-update.json > rule-update-with-canary.json
```

Submit `rule-update-with-canary.json` through your authenticated Admin API client. Preserve the outer rule's IDs, namespace, enabled state, matching conditions, and other required fields. Admin validates the configuration before saving; configuration synchronization then updates the gateways. Confirm the update has reached all gateways before measuring results.

### Verify routing

First test the boundaries by setting `percentage` to `0`, then `100`: with both pools healthy and the example's empty conditions, `/version` should return `stable`, then `canary`. Restore `percentage` to `10` and send different identifiers:

```shell
for i in $(seq 1 1000); do
  curl --fail --silent --show-error \
    -H "X-Canary-Key: user-$i" http://localhost:9195/version
  printf '\n'
done
```

Repeat one identifier to check that its partition stays the same while the rule ID, configuration, and pool availability remain unchanged. No particular identifier is guaranteed to select Canary. The percentage applies to identifier buckets, so the number of requests need not be exactly 10%; a frequently used identifier can skew the request ratio. Without the header, this 10% configuration selects Stable.

## Configuration reference

| Field inside `canary` | Default | Meaning and validation |
| --- | --- | --- |
| `enabled` | `false` | Whether to assign traffic to Canary. A present configuration with `false` selects the labeled Stable pool. |
| `conditions` | Absent | An absent or empty array allows all requests that already matched the Divide rule to participate. Otherwise, conditions are evaluated before percentage assignment. |
| `matchMode` | `0` | `0` means AND; `1` means OR, for the Canary conditions. |
| `percentage` | `0` | Integer from `0` to `100`, applied to eligible identifiers. `0` selects Stable; `100` sends requests passing the conditions to Canary without reading a sticky key. |
| `stickyKey` | Absent | Object with `paramType` and `paramName`. Required when enabled with a percentage from `1` to `99`. A missing or blank extracted value selects Stable. |
| `stableLabels` | Required | Nonempty map of nonblank string keys and values selecting Stable upstreams. |
| `canaryLabels` | Required | Nonempty map of nonblank string keys and values selecting Canary upstreams. |
| `fallbackPolicy` | `STABLE` | `STABLE` or `REJECT`, case-sensitive; controls an empty Canary pool during initial selection. |

Both label maps are required even when `enabled=false`. They must share at least one key with different values, such as `release=stable` and `release=canary`, so a node cannot match both pools. Every configured label must match exactly; additional node labels are allowed. A node missing the required labels belongs to neither pool. Label values must be JSON strings, not numbers or booleans.

Conditions use the existing matching fields: `paramType`, `paramName`, `operator`, and `paramValue`. For example, set `conditions` to the following array to admit only requests with `X-Canary-Eligible: yes` to percentage assignment; all other requests select Stable:

```json
[
  {
    "paramType": "header",
    "paramName": "X-Canary-Eligible",
    "operator": "=",
    "paramValue": "yes"
  }
]
```

Parameter sources and operators must be supported by the Admin dictionaries and installed gateway extensions. `header`, `cookie`, `query`, and `post` sources require `paramName`. Regular expressions and time-condition values are validated before use. See [Selectors and Rules](https://shenyu.apache.org/docs/user-guide/admin-usage/selector-and-rule/) for matching concepts.

## Routing and retry behavior

The default service hashes the rule ID and sticky-key value into a stable bucket. Changing the node topology does not change that bucket. Increasing the percentage admits additional buckets; changing the rule ID or the identifier source can reassign users. This is **partition affinity**, not a guarantee of selecting the same upstream node.

| Situation | Result |
| --- | --- |
| `canary` absent or `null` | Use legacy routing, including the existing `gray` behavior. |
| Configuration present but disabled, percentage is zero, conditions fail, or the required identifier is missing | Select the labeled Stable pool. |
| Canary selected and its healthy labeled pool is empty, policy `STABLE` | Try the labeled Stable pool once during initial routing. |
| Canary selected and its healthy labeled pool is empty, policy `REJECT` | Reject with HTTP 503. |
| Stable selected but its healthy labeled pool is empty | Reject with HTTP 503; do not switch to Canary. |
| Selected pool has nodes but all are blocked by the circuit breaker | Reject with HTTP 503; this does not trigger empty-pool fallback. |
| Request contains `specify-domain` | The existing explicit-target path takes precedence and bypasses Canary selection. |

Initial fallback does not mean fallback after a backend error. A successful initial Canary-to-Stable fallback resolves this request to Stable, even if Canary nodes become available during its retries.

**Current configuration limitation:** a non-null `canary` object with `retry > 0` is accepted only with `retryStrategy: "current"` (also the default). `current` retries the selected node. The validator still rejects `failover` with positive retries, including when `canary.enabled=false`, although partition-preservation support exists in the retry layer. Do not configure Canary failover retries until your deployed version explicitly supports that combination. Setting `retry: 0` disables retries; it does not enable failover.

## Upgrade, migration, and rollback

### Upgrade without changing traffic policy

Back up the Admin database and the relevant selector, discovery-upstream, and rule configurations. Apply the database upgrade scripts required by the source and destination releases; this guide is not a replacement for a full ShenYu version upgrade procedure.

Upgrade Admin and all gateways serving the route to builds with the required features, keeping `canary` absent throughout the rolling upgrade. Older gateways may ignore unfamiliar configuration fields, so enable the new policy only after the whole serving group is compatible and synchronized.

Upgrading does not automatically convert `gray` to `labels`. With no `canary` object, the legacy selection behavior remains:

| Existing selector configuration | Behavior after upgrading without Canary configuration |
| --- | --- |
| Gray nodes configured and at least one is healthy | Only healthy gray nodes are candidates. |
| Gray nodes configured but all are unavailable | No ordinary-node fallback; no eligible upstream. |
| No gray nodes configured | Use the ordinary healthy pool. |

Verify these cases before changing the traffic policy. Adding labels alone does not activate Canary routing.

### Move from `gray` to label-based pools

Add labels without removing existing properties. The following examples show the parsed `props` of two existing nodes before migration:

```json
[
  {"gray": false},
  {"gray": true}
]
```

After adding labels, the same nodes have:

```json
[
  {"gray": false, "labels": {"release": "stable"}},
  {"gray": true, "labels": {"release": "canary"}}
]
```

These arrays illustrate properties only; they are not the batch-update API body. Keep the original `gray` values for legacy rules and rollback. Check that discovery updates or automation continue to preserve the labels. Labels are shared by rules using these upstreams, so review other rules before relabeling a node.

Confirm both labeled pools are healthy and synchronized, then add the rule's `canary` configuration.

The default `STABLE` policy introduces ordinary-version fallback that the legacy gray-only path did not provide. Similarly, replacing an all-gray policy with 10% Canary changes traffic allocation. Neither is an automatic, behavior-preserving conversion.

Increase or decrease the percentage in stages, retaining the rule ID and identifier source. After each synchronized change, check backend responses, request outcomes, and fallback counts. `percentage=100` still respects conditions, so remove eligibility conditions only if every matching request should participate.

### Roll back

| Action | Effect |
| --- | --- |
| Set `canary.enabled=false`, or set its percentage to zero | Send subsequent matching requests to the labeled Stable pool. Labels must remain valid and Stable must have healthy nodes. |
| Remove `canary` or set it to `null`, and restore the backed-up legacy settings | Return to legacy routing. Retained `gray=true` nodes can receive all matching traffic again. |
| Downgrade gateway or Admin binaries | First restore compatible legacy configuration and verify synchronization; then follow the target release's database and binary downgrade procedure. Disabling Canary alone still leaves a configuration older versions do not implement. |

Requests already in progress can continue using their resolved partition. Allow for configuration propagation and in-flight requests when evaluating rollback results.

## Canary metrics

Add `shenyu-spring-boot-starter-plugin-metrics` to the gateway with the same ShenYu version as the other gateway dependencies, then enable the existing Prometheus reporter:

```yaml
shenyu:
  metrics:
    enabled: true
    name: prometheus
    host: 127.0.0.1
    port: 8090
```

Use a bind address reachable by your Prometheus scraper. These metrics share the existing metrics endpoint; no separate Canary reporter or switch is needed. Routing works without metrics enabled. See the [Metrics plugin guide](https://shenyu.apache.org/docs/plugin-center/observability/metrics-plugin/) for dependency and scrape setup.

| Name | Type | Labels | Recorded value |
| --- | --- | --- | --- |
| `shenyu_canary_requests_total` | Counter | `selector`, `rule`, `partition`, `outcome` | One final outcome per external request attributed to Canary routing, including requests assigned to Stable. |
| `shenyu_canary_fallback_total` | Counter | `selector`, `rule`, `reason` | One successful initial selection from Stable after the intended Canary pool was empty. |
| `shenyu_canary_decision_duration_seconds` | Histogram | `selector`, `rule` | Duration of the initial Canary decision in fractional seconds. |

`selector` and `rule` are configuration IDs, not display names. `partition` is `stable` or `canary`: it describes the selected pool, or the intended pool when routing rejects the request before selecting a node. Requests outside Canary routing have no Canary metric series; no `partition="none"` series is emitted. User identifiers, sticky keys, request paths, upstream URLs, and header/cookie values are not metric labels.

The final outcome is classified in this order:

| `outcome` | Meaning |
| --- | --- |
| `cancelled` | The reactive request execution was cancelled. |
| `reject` | Canary routing recorded an explicit rejection, such as an unavailable selected pool. |
| `error` | Execution ended with an error, or the final status was neither 2xx nor 3xx, including 4xx/5xx. |
| `success` | Execution completed with 2xx/3xx, or without an explicit response status. |

Internal retries do not add request counts or repeat decision observations. The decision histogram measures the decision service call, excluding upstream filtering, forwarding, and retry waits. It is not request latency. Its finite bucket boundaries in seconds are `0.000001`, `0.000005`, `0.00001`, `0.00005`, `0.0001`, `0.0005`, `0.001`, `0.005`, `0.01`, `0.05`, and `0.1`, followed by the Prometheus `+Inf` bucket.

The fallback reason is currently `canary_pool_empty`. A fallback is counted when a Stable upstream is successfully selected, regardless of the subsequent backend outcome. It is not counted for retries, both pools being empty, or failure to select a Stable node after circuit-breaker/load-balancer checks. The request outcome for a successful initial fallback is attributed to `partition="stable"`.

The existing `shenyu_execute_latency_millis` metric keeps its name, labels, units, and recording behavior. Existing dashboards and alerts for that metric require no Canary-specific migration; it does not gain a `partition` label.

### Query examples

Scrape the existing metrics endpoint, for example `http://localhost:8090/metrics` with the metrics configuration above. Labeled series appear after an observation; a missing fallback series before the first fallback is expected. Replace `SELECTOR_ID` and `RULE_ID` below with the actual configuration IDs, and add your Prometheus job/cluster filters when multiple environments share a server.

Request rate by partition and outcome:

```promql
sum by (partition, outcome) (
  rate(shenyu_canary_requests_total{selector="SELECTOR_ID", rule="RULE_ID"}[5m])
)
```

Error ratio for each partition, considering only `success` and `error` outcomes. Rejections and cancellations are excluded; this is not an availability ratio for all incoming traffic. The zero-valued alternative handles a partition that has traffic but no error series yet. A partition with no observations may have no series, and a zero denominator can produce `NaN`.

```promql
(
  sum by (partition) (
    rate(shenyu_canary_requests_total{selector="SELECTOR_ID", rule="RULE_ID", outcome="error"}[5m])
  )
  or
  0 * sum by (partition) (
    rate(shenyu_canary_requests_total{selector="SELECTOR_ID", rule="RULE_ID", outcome=~"success|error"}[5m])
  )
)
/
sum by (partition) (
  rate(shenyu_canary_requests_total{selector="SELECTOR_ID", rule="RULE_ID", outcome=~"success|error"}[5m])
)
```

Initial fallback count over five minutes (Prometheus `increase` extrapolates over the range and can return a fractional value):

```promql
sum(increase(shenyu_canary_fallback_total{selector="SELECTOR_ID", rule="RULE_ID", reason="canary_pool_empty"}[5m]))
```

95th-percentile decision duration in seconds:

```promql
histogram_quantile(0.95,
  sum by (le) (
    rate(shenyu_canary_decision_duration_seconds_bucket{selector="SELECTOR_ID", rule="RULE_ID"}[5m])
  )
)
```

## Troubleshooting

| Symptom | What to check |
| --- | --- |
| All requests go to Stable | Enabled state, percentage, eligibility conditions, missing sticky key, or an empty Canary pool with `STABLE` fallback. |
| HTTP 503 during initial selection | Required labels, healthy-node availability in the resolved pool, fallback policy, and circuit-breaker state. An unlabeled ordinary node is not automatically Stable. |
| Configuration rejected | String-valued labels, disjoint label maps, integer percentage, supported parameter/operator names, and the current-only retry limitation. |
| Same user appears on different versions | Consistent rule ID and configuration across gateways, stable identifier extraction, and initial fallback events. |
| No Canary metric series | Metrics enabled and scraped, traffic reached this Divide rule, no `specify-domain` override, and an observation has occurred for that label combination. A fallback series may be absent until a fallback happens. |

For a controlled acceptance check, test 0% and 100%, missing identifiers, repeated identifiers, each initial empty-pool policy, and both rollback actions. Use requests without side effects when testing retry behavior. Compare results after configuration synchronization has completed.
