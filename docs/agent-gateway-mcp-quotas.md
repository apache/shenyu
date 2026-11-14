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

# Agent Gateway distributed MCP quotas

Agent Gateway can enforce a shared invocation-count window and concurrency
limit for actual MCP `tools/call` requests. The first policy dimensions are
trusted tenant and subject, each using `invocation-limit`. Local and federated tools use the same admission
boundary. Tool discovery, resource reads, and other MCP methods do not consume
these quotas.

The feature is opt-in. The application owns the Redis connection and supplies
one `ReactiveRedisTemplate<String, String>` bean; Agent Gateway does not create
a second Redis client or lifecycle.

## Configuration

```yaml
shenyu:
  plugins:
    agent:
      gateway:
        mcp-quota:
          enabled: true
          mode: shadow            # shadow or enforce
          invocation-limit: 1000
          invocation-window: 60s
          concurrency-limit: 100
          lease-ttl: 60s
          fail-open: false
          policy-version: mcp-quota-v1
```

The policy is an immutable startup snapshot. Changing these properties requires
an application restart or recreation of the owning Spring context; runtime rule
updates do not partially replace a running policy. `policy-version` is bounded
diagnostic metadata and does not shard Redis state.

`shadow` evaluates and records isolated would-deny decisions but never rejects
the call and never debits enforce state. `enforce` rejects at the actual tool
execution boundary. A denied tool is never executed. Missing or invalid tenant or
subject is an explicit identity rejection (403 / `-32027`), including in shadow
mode; unrelated callers never share an `unknown` tenant bucket.

## Redis semantics

Admission is one Lua operation. Redis `TIME` controls fixed windows and lease
expiry; gateway clocks never select windows or reclaim live leases.
It atomically checks the tenant and subject invocation counters,
removes expired concurrency leases, checks concurrency, debits the count, and
adds a unique lease token. Redis returns one of invocation-limited,
concurrency-limited, or admitted-with-lease; it never debits or acquires a
lease for a rejected request.

All admission keys share one Redis Cluster hash tag derived from mode and
base64url-encoded tenant; subject components are encoded independently:

- `shenyu:agent:mcp-quota:invocation:{mode:encodedTenant}`;
- `shenyu:agent:mcp-quota:invocation:{mode:encodedTenant}:encodedSubject`;
- `shenyu:agent:mcp-quota:concurrency:{mode:encodedTenant}:encodedSubject`.

Window hashes store the server-computed window and count and expire just after
that window. The concurrency sorted set stores one member per in-flight lease
and expires just after its lease TTL. Cardinality is bounded by active tenant and
subject identities, one active invocation hash per identity,
and at most the configured concurrency count per identity.

Completion, error, cancellation, or subscriber cancellation releases only the
opaque lease returned for that admission. Release is idempotent and cannot
remove another request's lease. Invocation count is never refunded: a retry is
a new admitted invocation, cancellation releases concurrency only, and an
unknown upstream outcome consumes the invocation because its side effect may
have occurred.

There is no lease renewal in this first policy. A lease carries one immutable
expiry and cannot be resurrected. If a gateway disappears, its lease is ignored
and removed after at most the configured `lease-ttl`, plus the documented
key-expiration buffer. Tool deadlines longer than `lease-ttl` are rejected as
policy errors before Redis admission. Set `lease-ttl` at least as high as the
largest tool timeout; enforced execution is also bounded by the lease TTL.
Local and remote capacity admission precede quota consumption, so overload
rejection never consumes an invocation.

## Outages and observability

Redis admission failure is explicit. `fail-open: false` rejects with a policy
error (HTTP 503 / JSON-RPC `-32027`) rather than guessing that admission was
granted. `fail-open: true` executes without a synthetic lease and records a
policy-error decision; concurrency release is not attempted for that request.

Allowed, would-deny, deny, and policy-error outcomes use the Agent governance
decision contract. Metrics expose only bounded decision labels:

```text
shenyu_agent_governance_decisions_total{consumer,mode,result,reason}
```

Tenant, subject, tool name, endpoint, request id, and trace id are not metric
labels. Non-allow decisions can also be emitted as metadata-only audit events
through the application-provided audit observer.
Would-deny decisions are also counted in the governor's bounded, generation-owned
`AgentGovernanceShadowLedger`, available through `shadowSnapshot()`, separately
from enforce Redis state.

## Redis regression checks

The `Agent MCP Redis quota` workflow starts a real Redis node with Cluster slot
enforcement and assigns all slots to it. It requires the Redis tests to run
without skipped cases. Tests use independent connections, skewed gateway times,
actual Redis window/lease boundaries, tenant aggregate limits and tenant
isolation. This verifies script slot correctness; it does not simulate
multi-node Cluster failover.

## Non-goals

This is not a second general-purpose rate-limiter platform. It does not bill
elapsed time, enforce LLM token or cost budgets, replay side-effecting tools,
or add per-tool and per-server dimensions. Those require an explicit policy
extension so key cardinality and isolation remain bounded.
