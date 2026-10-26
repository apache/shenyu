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

# API key identities for Agent Gateway tools

Agent Gateway can reuse the existing `key-auth` plugin's successful authentication.
It does not verify a second credential or derive an identity from client headers.
Install and enable the key-auth starter alongside the Agent Gateway starter.
The key-auth plugin must match and authenticate the request before Agent Gateway.
Its existing header/query credential contract and error response remain unchanged.
Prefer HTTPS and header credentials; query credentials can appear in access logs.

## Explicit deployment policy

This adapter is disabled by default. Configure it in the gateway application,
not in a client request or MCP `_meta`:

```yaml
shenyu:
  plugins:
    agent:
      gateway:
        enabled: true
        security:
          key-auth:
            enabled: true
            bindings:
              - namespace-id: production-namespace-id
                selector-id: service-a-selector-id
                rule-id: service-a-key-rule-id
                tenant: tenant-a
                subject: service-a
                tool-grants: [inventory.lookup, orders.read, orders.delete]
                tool-denials: [orders.delete]
```

Replace all three configuration IDs with the IDs of the authenticated key-auth
rule. They are matched exactly as a tuple. The namespace is a configuration
identifier, **not** an inferred tenant. Neither the API key nor its hash belongs
in this policy. A rule denotes one service identity; users sharing its key are
not distinguished. Use separate rules and credentials for separate identities.

All five identity/reference fields are required. Duplicate rule bindings, empty
binding lists, unknown properties and wildcard permissions fail startup. Tool
names are exact, case-sensitive names, including prefixes for remote tools; the
adapter accepts 1–128 ASCII letters, digits, underscores, dots and hyphens.
An omitted grant list is empty and grants no tools.

Effective permission is `(tool-grants - tool-denials) ∩ rule.allowedTools`.
The Agent Gateway rule must separately enable the intended tools. Both
`tools/list` and `tools/call` use this same permission snapshot, for JSON and SSE.
Unauthorized calls are rejected before tool execution; listing does not grant
permission. Existing tool registration, target restrictions and result filtering
remain in effect. The adapter adds no prompts/resources authorization.

## Isolation, conflicts and changes

- Authentication is a credential-free, immutable reference in the request-bound
  Reactor Context. A different exchange cannot borrow it; disabled or unmatched
  key-auth execution cannot inherit an earlier authentication result.
- Missing authentication, an unbound reference or an existing principal produces
  no trusted identity and the MCP handler responds with HTTP 401. An existing
  principal is rejected conservatively, even if it names the same subject. The
  adapter does not silently combine authentication authorities.
- Configure only one `AgentMcpSecurityResolver`. Multiple resolver beans fail
  startup, including when one is `@Primary`. Custom deployments needing source
  arbitration must provide their own single resolver and disable this adapter.
- Bindings are copied at startup; restart the application to change them. There
  is no implicit tenant, anonymous fallback, live policy mutation or new admin
  CRUD API. Existing adapters using subject-only identities remain compatible.
- Key-auth credential rotation continues to use its existing configuration sync.
  Changing a key on the same rule retains the bound service identity. Changing
  the namespace, selector or rule reference requires a corresponding policy
  binding; never reuse an authorized rule for a different identity accidentally.
- New requests use current matched rule configuration. An admitted in-flight
  call retains its immutable identity and permissions; revocation does not undo
  already-executed tool effects. Cancellation remains linked to the execution.
- No identity or raw key is automatically added to outbound MCP server requests.
  Existing fixed server credentials remain independent of the inbound identity.

## Regression scope

Key-auth tests exercise authentication, scheduler transitions and cross-request
isolation. Starter tests cover configuration, public key-auth → Agent Gateway
plugin execution, real loopback JSON/SSE HTTP, tenant isolation, deny precedence,
key rotation, missing bindings and authorization snapshots. The test cache and
tool providers are controlled fixtures, not a deployed Admin/WebSocket or real
model integration environment.
