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

# Legacy MCP session lifecycle — local candidate for #7954

Status: local design and acceptance contract, not an upstream-confirmed
IssueSpec Design/SPEC. Publication must supply the repository's required
planning links. Outbound recovery remains in #8311. Output queue/stall
control belongs to #8350 and is not implemented here.

## Problem and design

The ShenYu-owned WebFlux SSE and Streamable HTTP transports retain healthy
sessions without a capacity or idle-age bound. Add one lifecycle owner per
transport. It serializes creation/admission, tracks active operations and
monotonic idle age, and owns idempotent removal and local transport close.
No network I/O or asynchronous SDK shutdown is awaited while holding its lock.
The bounded local close cancels active handlers, clears pending response
waiters and removes exchange bindings even when SDK close fails.

## Contract and acceptance

- With a positive maximum, concurrent creation admits at most that many
  sessions per transport, including temporary/restored sessions. Factory
  failure releases capacity. Exhaustion is HTTP 503 before HTTP initialize;
  a direct SSE body subscription terminates with a service-unavailable error.
- With a positive idle timeout, sessions without active inbound operations
  expire after that monotonic interval plus at most one sweep interval.
  Completion/error/cancellation resets idle age. An open SSE stream alone
  does not extend it. Notifications and requests both hold operation leases.
- Idle selection and lease acquisition are atomic. Expiry, disconnect,
  explicit close and provider shutdown release capacity and exchange state
  exactly once. Shutdown rejects further creations and disposes the sweep.
- Unknown Streamable HTTP IDs retain the existing one-request restoration
  behavior, subject to the same limits. No tool call is replayed here.
- Configuration is startup-owned under `shenyu.plugins.mcp.server.session`:
  `max-sessions` (zero disables capacity), `idle-timeout` (zero disables idle
  expiry), `sweep-interval` (positive, no greater than an enabled idle timeout).
  Defaults disable the new limits for compatibility. Invalid configuration
  fails application startup. Manager destruction closes every provider.
- These are local, per-transport bounds, not a cluster-wide quota. A path
  serving both protocols has two pools. Stateful clients need replica affinity;
  reconnect after replica loss follows existing transport behavior. SDK-owned
  servlet transports and Agent Gateway stateless ingress are outside scope.

## Implementation and verification tasks

1. Implement validated policy and lifecycle owner, testing concurrent creation,
   factory failure, idle/active races, cancellation, close failure and shutdown.
2. Wire both transport entry paths and startup configuration; test invalid
   binding, independent providers and compatibility defaults.
3. Reproduce admission and reclamation over real loopback HTTP/SSE and run
   affected module/starter tests, Checkstyle, RAT and diff checks.
