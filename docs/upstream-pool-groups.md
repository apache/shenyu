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

# Named upstream connection-pool groups

## Overview

A divide rule can select one of a finite set of named upstream connection-pool
groups configured at gateway startup. Requests that use the same upstream origin
but belong to different workloads can therefore use independent bounded
connection and acquisition pools. A slow bulk workload cannot consume the
connection/acquisition capacity reserved for an interactive workload merely
because both call the same backend host.

This is an opt-in feature. With no `shenyu.httpclient.pool-groups` configured,
the gateway keeps its existing single shared pool and request behavior is
unchanged.

## Configuration

Pool groups live under the existing `shenyu.httpclient` prefix, next to the
shared `pool` settings. Only the `FIXED` pool type is accepted for a group; each
group is bounded by its own connection limit and pending-acquire queue.

```yaml
shenyu:
  httpclient:
    pool-groups:
      - name: bulk
        type: FIXED
        maxConnections: 32
        acquireTimeout: 500
        maxPendingAcquires: 32
      - name: interactive
        type: FIXED
        maxConnections: 256
        acquireTimeout: 3000
```

Rules for the group list:

- At most 16 groups.
- `name` is required and unique (case-insensitive). It must not be
  `shenyu-proxy`, the name of the shared default pool.
- `type` must be `FIXED`.
- `maxConnections` must be strictly positive.
- `acquireTimeout` must not be negative.
- `maxPendingAcquires` must be omitted or strictly positive; `-1` and `0` are
  rejected for groups. The shared default pool keeps its historical behavior.
- TLS, proxy, keep-alive, idle handlers, HTTP/2 negotiation and the shared
  Netty loop resources are inherited from `shenyu.httpclient`, exactly like the
  default client.
- The group set is read only at startup. Adding, removing or changing a group
  requires a gateway restart.

To select a group, set `poolGroup` in the divide rule handle:

```json
{
  "loadBalance": "random",
  "retry": 3,
  "poolGroup": "bulk"
}
```

The dashboard form does not expose `poolGroup` yet; use the Admin rule API or
the handle JSON directly.

## Request behavior

The divide plugin publishes the selected group on the exchange before the HTTP
client runs. The Netty and WebClient strategies both resolve that group once per
attempt and use the group's client. The selection is pinned for the whole
request lifetime, including retry/failover attempts and the response body; a
rule update affects future requests only.

An unknown group name, a registry that has already been closed, or a rule that
selects a group on a gateway where the group is not configured produces HTTP
503 for that request. There is no silent fallback to another group or to the
shared default pool. Requests without `poolGroup` always take the shared
default client.

Group selection is per gateway process. Different gateways may configure
different group names; a rule that references a group must only be published to
gateways that define it. The limits are per group provider and per origin
inside that provider, not cluster-wide quotas and not a request rate limiter.
With HTTP/2, one connection multiplexes concurrent streams, so the isolation is
connection/acquisition capacity rather than one connection per RPC.

## Metrics

Set `metrics: true` on a group to publish its Reactor Netty provider meters:

```yaml
      - name: bulk
        type: FIXED
        maxConnections: 32
        metrics: true
```

The meters are native Reactor Netty Micrometer meters registered in the global
Micrometer registry, for example:

- `reactor.netty.connection.provider.active.connections`
- `reactor.netty.connection.provider.idle.connections`
- `reactor.netty.connection.provider.pending.connections`
- `reactor.netty.connection.provider.max.pending.connections`
- `reactor.netty.connection.provider.pending.connections.time`

The provider name (the group name) is carried in the meter tags, so the metric
cardinality is bounded by the configured groups and discovered origins. This
requires `micrometer-core` on the classpath; the shipped bootstrap gets it
through Spring Boot Actuator. It is not exported by the ShenYu Prometheus
`MetricsRegister` endpoint in this delivery. To scrape the meters, add a
Micrometer registry (for example `micrometer-registry-prometheus`) and expose
it through your Micrometer/Actuator setup.

There is no separate Reactor Netty counter for a rejected acquisition. Use the
pending queue depth, its configured maximum, and the pending-acquire timer's
outcome to observe waiting and rejection behavior.

## Upgrade, migration and rollback

The feature is additive. Rolling upgrades should leave `pool-groups` absent on
the new version until all gateways can serve the rules that reference a group;
older gateways ignore an unknown `poolGroup` handle field. To roll back, remove
`poolGroup` from the affected rule handles and restart the gateways without the
`pool-groups` block. Removing the configuration restores the shared default pool
and the previous behavior.

## Fixed-workload isolation measurement

`UpstreamPoolGroupIsolationTest` covers correctness. For a repeatable capacity
measurement, the test sources also contain an opt-in benchmark that compares one
shared provider with two independent providers against the same loopback origin
(8 bulk clients on a 50 ms endpoint, 2 interactive clients on a fast endpoint,
5 seconds per scenario):

```bash
mvn -pl shenyu-plugin/shenyu-plugin-httpclient -am test \
  -Dtest=UpstreamPoolGroupIsolationBenchmark \
  -Dshenyu.poolgroup.benchmark=true -DfailIfNoTests=false
```

Example output on Windows 10, JDK 17.0.18, 16 cores (fixed loopback workload,
not a universal p50/p99 claim):

```text
scenario,bulk_ok,bulk_fail,interactive_ok,interactive_fail,interactive_p50_ms,interactive_p99_ms
shared,274,173293,85,82980,2,815
groups,338,8,28774,0,0,1
```

The shared scenario is one provider of 4 connections shared by both workloads;
the groups scenario uses a bulk provider of 4 and an interactive provider of 8.
The interactive workload is effectively starved in the shared scenario and
unaffected in the grouped scenario, which is the fixed-workload evidence for
the isolation boundary. Re-run the command on your deployment host before
quoting latency numbers; results include loopback and JIT effects.

## Troubleshooting

| Symptom | Check |
| --- | --- |
| HTTP 503 with an unknown pool group message | The rule references a group not defined on this gateway, or the registry is shutting down. Check the exact group name and the gateway's startup configuration. |
| Startup fails with a group validation message | Use `FIXED`, a positive `maxConnections`, an explicit unique name other than `shenyu-proxy`, and omit or use a positive `maxPendingAcquires`. |
| Startup fails because a custom `HttpClient` bean is present | `pool-groups` owns its clients; remove the custom `HttpClient` bean or the group configuration. |
| Metrics are missing | Set `metrics: true` on the group, ensure `micrometer-core` is present, and verify the meter is exported by your Micrometer registry. |
