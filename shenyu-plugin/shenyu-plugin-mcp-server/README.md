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

# Legacy MCP Server session limits

The ShenYu-owned WebFlux SSE and Streamable HTTP providers support optional
startup limits:

```yaml
shenyu:
  plugins:
    mcp:
      server:
        session:
          max-sessions: 1024
          idle-timeout: 30m
          sweep-interval: 10s
```

`max-sessions: 0` (default) disables the capacity limit. `idle-timeout: 0s`
(default) disables idle expiry. The sweep interval defaults to `1s`, must be
positive, and cannot exceed an enabled idle timeout. Negative, overflowing
or unknown settings fail startup. Configuration changes require restart;
this policy is not an Admin rule or a database-backed setting.

Capacity is **per provider instance on each replica**. SSE and Streamable HTTP
have separate pools even when sharing a server path. Temporary/restored HTTP
sessions consume slots until their request terminates. A failed factory does
not consume capacity. HTTP initialization returns 503 on exhaustion. SSE
admission happens when its response body is subscribed, so exhaustion can
terminate a stream after response headers have been sent; clients must treat
that connection as unsuccessful and reconnect with backoff.

Idle age uses a monotonic clock. An inbound request or notification holds an
activity lease through its completion, error or cancellation; the idle clock
then restarts. Merely keeping an SSE connection open does not extend it, nor
do server broadcasts. Active operations are not killed by idle expiry. Their
execution deadlines belong to the existing request/tool layers. Reclamation
occurs within the idle timeout plus one sweep interval, subject to scheduler
availability. Disconnect, explicit session removal, provider shutdown and
manager destruction clean local maps, exchange bindings and pending waiters.
Cleanup and capacity release are idempotent and independent across sessions.

These are local resource bounds, not a distributed quota or durable session
store. Multi-replica deployments must route a stateful session's SSE and
message requests to the same replica (for example with sticky routing).
Replica loss or idle expiry requires reconnecting/reinitializing. The existing
one-request restoration of unknown Streamable HTTP IDs remains subject to
the same limits; this change does not replay any tool invocation.

SDK-owned servlet transports and stateless Agent Gateway ingress are outside
this lifecycle. SSE output queue and stalled-consumer bounds are separate
work (#8350); a session capacity/idle limit does not bound each output queue.
