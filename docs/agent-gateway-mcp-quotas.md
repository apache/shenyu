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
trusted tenant and subject; local and federated tools use the same admission
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
execution boundary. A denied local tool is never executed.

## Redis semantics

Admission is one Lua operation. It atomically checks the invocation counter,
removes expired concurrency leases, checks concurrency, debits the count, and
adds a unique lease token. Redis returns one of invocation-limited,
concurrency-limited, or admitted-with-lease; it never debits or acquires a
lease for a rejected request.

Keys are scoped by mode, tenant, and subject:

- `shenyu:agent:mcp-quota:invocation:{mode}:{tenant}:{subject}:{windowStart}`;
- `shenyu:agent:mcp-quota:concurrency:{mode}:{tenant}:{subject}`.

The invocation key expires one second after its fixed window. The concurrency
sorted set stores one member per in-flight lease and expires one second after
the configured lease TTL. Cardinality is therefore bounded by active tenant and
subject identities, one invocation key per active fixed window per identity,
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
one-second key-expiration buffer. Existing request deadlines remain the
execution-time boundary.

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

## Non-goals

This is not a second general-purpose rate-limiter platform. It does not bill
elapsed time, enforce LLM token or cost budgets, replay side-effecting tools,
or add per-tool and per-server dimensions. Those require an explicit policy
extension so key cardinality and isolation remain bounded.
