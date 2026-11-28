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

# MCP upstream protocol profiles

The Agent Gateway tools entry accepts MCP `2026-07-28` requests. An operator
selects the outbound protocol separately for each server in the aggregation
configuration. The requirements originate in [ShenYu #8196](https://github.com/apache/shenyu/issues/8196).

## Configuration

Admin and Gateway MUST interpret `aggregation.servers[].protocolProfile`
identically. Omission selects `legacy-2025-06-18`. The only other identifier is
`native-2026-07-28`. Explicit null, non-string, blank and unknown values MUST be
rejected before persistence or catalog replacement. Both profiles use the same
four-field service credential binding and deployment endpoint allowlist.

## Native tools

A native catalog refresh MUST verify that `server/discover` advertises
`2026-07-28` and the tools capability before `tools/list`. Each POST MUST carry
its own protocol version and client capabilities, and mirror its method,
version and applicable tool name in headers. Names requiring encoding use
the MCP Base64 sentinel. The bounded adapter accepts JSON and single-final-frame
SSE. It MUST accept only complete results, surface unsupported results as
errors, and never replay a dispatched call. It MUST never initialize a session,
send a session ID, perform GET recovery or delete a session.

Native resources are unsupported. A mixed catalog with resource aggregation
enabled MUST skip native resource discovery and keep eligible legacy
resources accessible. Native resource operations MUST fail before dispatch.
The legacy profile retains its existing handshake, session and cleanup behavior.

## Isolation and visibility

Rule and identity grants MUST apply to both profiles. An admitted call retains
its original binding and deadline until completion or cancellation, including
through replacement and rollback. Independent gateways MUST be able to call the
same native upstream without a shared protocol session.

Diagnostics MUST describe each active binding's ingress and outbound versions,
outbound profile, supported operations, results, response modes and known
translation limits. These descriptors exclude endpoint credentials, Agent
identity and tool payloads.

## Verification evidence

Versioned real-SDK interoperability evidence (official TypeScript SDK `2.4.0`
across JSON, SSE, legacy default and independent raw SSE paths, preserving
historical `#7424` observations for `#8047`) is recorded in
[interop-evidence-v1.json](interop-evidence-v1.json).
