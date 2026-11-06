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

# Authorized MCP prompts aggregation

Local prompts keep their explicit registration name. Every remote prompt is exposed as `mcpserver.promptname`, including non-colliding names. Duplicate upstream definitions reject discovery; a collision between local and remote names rejects the prompt operation, without automatic rename or fallback. Prompt grants and denies are independent of tools/resources, with deny precedence. Rule and trusted identity permissions intersect for both list and get.

The first candidate supports bounded text prompt messages with user/assistant roles. Unsupported multimedia results fail explicitly rather than dropping content. Arguments are named strings, unknown/missing required arguments fail before admission. Result contents and arguments are never logged or used as identity. Resolving a prompt does not authorize or execute any tools mentioned in its messages.

Remote discovery consumes all bounded upstream pages before publishing one immutable generation; the gateway returns an unpaginated authorized list and rejects inbound cursors. Refresh/removal reuse the catalog generation leases and withdrawal fencing. This preparation integrates prompt discovery into #7952/#8143's existing polling, backoff, monotonic freshness check and atomic publication monitor. No separate notification/recovery scheduler is introduced. The existing inbound 2026-07-28 and outbound 2025-06-18 versions remain fixed; SDK migration is not assumed.

Admission controllers supplied by the application remain application-owned. The catalog does not close them. When a managed catalog is present, its atomic publication callback is the only writer of admission configuration; the plugin data handler validates events without applying the limits a second time.

## Configuration

Register local implementations as `AgentPromptProvider` beans. Metadata is frozen on assembly; each invocation receives an immutable `AgentPromptInvocation` with trusted tenant/subject, request ID, rule generation, deadline and copied arguments. Provider construction does not load arbitrary classes from configuration. Existing deployments have no prompt grants by default.

Remote prompt discovery requires the deployment-owned property `shenyu.plugins.agent.gateway.aggregation.prompts-enabled=true` in addition to the existing aggregation enablement, exact endpoint allow-list and fixed service-credential binding. The upstream must advertise the prompts capability. Endpoints and credentials are never selected from prompt arguments or inbound identity. No new SDK, TLS, session or credential manager is installed.

The rule handle uses the existing Admin JSON configuration and sync path:

```json
{"trafficType":"mcp","mcp":{"allowedPrompts":["localSummary","orders.summary"],"responseMode":"json"}}
```

Admin validates `allowedPrompts` with the same shared parser as the gateway. Trusted JWT/opaque/login claims use `prompt_grants` and `prompt_denies`; explicit principal scope mappings use `prompt-grants` and `prompt-denials`. Key-auth bindings use those same deployment property names alongside their existing namespace/selector/rule references. A tool scope, client `_meta`, a forged tenant header or an unverified API key does not grant prompt access. Denials always win. Grant changes take effect through the existing authority/policy lifecycle, not by mutating an in-flight identity.

## Supported contract and limits

| Boundary | Contract |
| --- | --- |
| Local/remote definitions | At most 16 local and 16 remote prompts across at most 8 servers |
| Names/metadata | Original names up to 128 characters; remote exposed names up to 177 (48-character server + dot + original); each definition up to 16 KiB; descriptions up to 4096 characters |
| Discovery | At most 4 pages per server; repeated, empty or oversized cursors fail the complete generation |
| Arguments | At most 32 declared string arguments; each value at most 16384 characters; required/unknown arguments checked before provider admission |
| Results | 1 to 64 user/assistant text messages and at most 1 MiB UTF-8 JSON, also subject to the existing rule response-byte limit |
| Concurrency | Existing shared global/local/per-server admission and queue limits; absolute request deadline; no prompt-specific retry |
| List changes | Complete snapshots on trusted reload; old in-flight leases may finish, but new requests cannot reacquire a withdrawn generation |

The gateway advertises `prompts` only when the matched rule configures prompts. It does not advertise `listChanged` or implement a GET notification stream. `prompts/list` returns one complete authorized list; an incoming cursor is rejected explicitly. Upstream paging completes before publication. Polling, outage recovery, hard snapshot expiry and configuration removal use #7952/#8143's shared complete-generation lifecycle. Failed refresh withdraws remote access; recovery never retries `prompts/get` or restores revoked identity grants.

An invalid original prompt identifier is omitted without withdrawing valid tools.
For unchanged server/credential bindings, a still-fresh published generation keeps
serving while its replacement is prepared. Removal, disablement, changed
credentials and invalid configuration still withdraw access immediately; stale
snapshots remain unavailable under the shared freshness policy.

Missing authentication is 401, an ungranted get is 403 / JSON-RPC `-32002`, invalid arguments/mirrored headers/cursors are 400, rejected capacity is 429, and an elapsed deadline is 504. Invalid prompt results fail with 502; empty/provider failures or namespace/configuration inconsistencies fail closed with a sanitized 5xx JSON-RPC error. Private provider messages, credentials and argument values are not included in errors.

## Reproducing the HTTP verification

`RemotePromptNetworkTest` uses a Java 17 HTTP client that implements the gateway's pinned stateless MCP wire contract: JSON-RPC with request `_meta`, mirrored `MCP-Protocol-Version`/`Mcp-Method`, and `Mcp-Name` for get. It opens real sockets into a Reactor gateway and two independently paged MCP upstream servers, in all four inbound JSON/SSE and outbound JSON/SSE combinations. This is a protocol client fixture; it does not claim unmodified Inspector, OpenCode, or a 2025-06-18 SDK can use the 2026-07-28 ingress without adaptation.

`KeyAuthMcpPromptHttpTest` separately exercises the public key-auth and Agent Gateway plugin chain over real HTTP with two service identities, tenant-specific prompts, explicit denials and tools coexistence. Its cache is fixture-owned; it is not an Admin/WebSocket deployment test.

The network fixture reuses its HTTP client, checks readiness, then verifies the
upstream handshake and tool interface before each scenario. Production request
and discovery budgets are unchanged. Unmodified third-party client acceptance
remains pending; the protocol fixture is not a substitute for that acceptance.

```shell
./mvnw -B -ntp -pl shenyu-spring-boot-starter/shenyu-spring-boot-starter-plugin/shenyu-spring-boot-starter-plugin-agent-gateway -am test -Dtest=AgentPromptRegistryTest,RemotePromptDirectoryTest,AgentMcpPromptIntegrationTest,RemotePromptNetworkTest,KeyAuthMcpPromptHttpTest -Dsurefire.failIfNoSpecifiedTests=false -DskipRemoteResources=true
```
